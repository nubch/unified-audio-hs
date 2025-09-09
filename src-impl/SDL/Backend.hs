{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module SDL.Backend
  ( runAudio
  , SDLSound
  ) where

-- base / std
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map

-- effectful
import Effectful (IOE, type (:>), Eff, withEffToIO)
import Effectful.Dispatch.Static (evalStaticRep)

-- sdl
import qualified SDL.Mixer as Mix

-- interface
import qualified UnifiedAudio.Effectful as I

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type FinishMap = MVar (Map.Map Mix.Channel (MVar ()))

data SDLSound :: I.Status -> Type where
  LoadedSound  :: Mix.Chunk -> I.SoundType -> SDLSound I.Loaded
  PlayingSound :: Mix.Channel -> MVar () -> I.SoundType -> SDLSound I.Playing
  PausedSound  :: Mix.Channel -> MVar () -> I.SoundType -> SDLSound I.Paused
  StoppedSound :: Mix.Channel -> SDLSound I.Stopped

----------------------------------------------------------------
-- Init / Finalization
----------------------------------------------------------------

initSDLFinishedMap :: IO FinishMap
initSDLFinishedMap = do
  finishMap <- newMVar Map.empty
  Mix.whenChannelFinished $ \ch ->
    modifyMVar_ finishMap $ \m ->
      case Map.lookup ch m of
        Just done -> do
          _ <- tryPutMVar done ()
          pure (Map.delete ch m)
        Nothing   -> pure m
  pure finishMap

----------------------------------------------------------------
-- Loading
----------------------------------------------------------------

loadSDL :: I.Source -> I.SoundType -> IO (SDLSound I.Loaded)
loadSDL src sType = 
  case src of
    I.FromFile fp  -> loadFromFile fp
    I.FromBytes by -> loadFromBytes by
    where
      loadFromFile fp = do
        loaded <- Mix.load fp
        pure $ LoadedSound loaded sType
      loadFromBytes bytes = do
        decoded <- Mix.decode bytes
        pure $ LoadedSound decoded sType

        

----------------------------------------------------------------
-- Play / Pause / Resume / Stop / Status
----------------------------------------------------------------

playSDL :: FinishMap -> SDLSound I.Loaded -> I.Times -> IO (SDLSound I.Playing)
playSDL fm (LoadedSound loaded sType) times = do
  channel <- Mix.playOn Mix.AllChannels sdlTimes loaded
  done    <- newEmptyMVar
  modifyMVar_ fm $ \m -> pure $ Map.insert channel done m
  pure (PlayingSound channel done sType)
  where
    sdlTimes = case times of
      I.Once    -> Mix.Once
      I.Times n -> fromIntegral n
      I.Forever -> Mix.Forever

pauseSDL :: SDLSound I.Playing -> IO (SDLSound I.Paused)
pauseSDL (PlayingSound channel finished sType) =
  Mix.pause channel >> return (PausedSound channel finished sType)

resumeSDL :: SDLSound I.Paused -> IO (SDLSound I.Playing)
resumeSDL (PausedSound channel finished sType) =
  Mix.resume channel >> return (PlayingSound channel finished sType)

stopSDL :: forall st. I.Stoppable st => FinishMap -> SDLSound st -> IO (SDLSound I.Stopped)
stopSDL fm stoppable =
  case stoppable of
    (PlayingSound channel finished _) -> stop channel finished
    (PausedSound  channel finished _) -> stop channel finished
  where
    stop channel finished = do
      Mix.halt channel
      modifyMVar_ fm $ \m -> pure (Map.delete channel m)
      _ <- tryPutMVar finished ()
      return (StoppedSound channel)

hasFinishedSDL :: SDLSound I.Playing -> IO Bool
hasFinishedSDL (PlayingSound _ finished _) = do
  fin <- isEmptyMVar finished
  pure $ not fin

----------------------------------------------------------------
-- Volume / Panning (and helpers)
----------------------------------------------------------------

setVolumeSDL :: forall adj. I.Adjustable adj => SDLSound adj -> I.Volume -> IO ()
setVolumeSDL adjustable vol =
  case adjustable of
    (PlayingSound channel _ _) -> Mix.setVolume volume channel
    (PausedSound  channel _ _) -> Mix.setVolume volume channel
  where
    volume = toSDLVolume vol

setPanningSDL :: forall adj. I.Adjustable adj => SDLSound adj -> I.Panning -> IO ()
setPanningSDL adjustable pan =
  case adjustable of
    (PlayingSound channel _ sType) -> setPan channel sType
    (PausedSound  channel _ sType) -> setPan channel sType
  where
    setPan :: Mix.Channel -> I.SoundType -> IO ()
    setPan channel sType = do
      let x      = I.unPanning pan
          (l, r) = case sType of
                     I.Mono   -> toSDLPanningMono   x  -- equal-power (mono→stereo)
                     I.Stereo -> toSDLPanningStereo x  -- balance (tilt)
      void $ Mix.effectPan channel l r

toSDLVolume :: I.Volume -> Int
toSDLVolume vol = round (volume * 128)
  where
    volume = I.unVolume vol

-- 0..128 per side (128 = full). effectPan doubles internally to 0..255.
toSDLPanningStereo :: Float -> (Int, Int)
toSDLPanningStereo x0 =
  let x = max (-1) (min 1 x0)
      l = round (128 * (1 - max 0 x))   -- reduce LEFT only when panning right
      r = round (128 * (1 + min 0 x))   -- reduce RIGHT only when panning left
  in (l, r)                             -- center => (128,128)

toSDLPanningMono :: Float -> (Int, Int)
toSDLPanningMono x0 =
  let x  = max (-1) (min 1 x0)
      gL = sqrt ((1 - x) / 2)           -- ≈0.707 at center (−3 dB/side)
      gR = sqrt ((1 + x) / 2)
  in ( round (128 * gL), round (128 * gR) )

----------------------------------------------------------------
-- Backend wiring / Runner
----------------------------------------------------------------

makeBackendSDL :: FinishMap -> I.AudioBackend SDLSound
makeBackendSDL fm =
  I.AudioBackend
    { I.playA        = playSDL fm
    , I.stopChannelA = stopSDL fm
    , I.loadA        = loadSDL
    , I.pauseA       = pauseSDL
    , I.resumeA      = resumeSDL
    , I.setPanningA  = setPanningSDL
    , I.setVolumeA   = setVolumeSDL
    , I.hasFinishedA = hasFinishedSDL
    }

runAudio :: (IOE :> es) => Eff (I.Audio SDLSound : es) a -> Eff es a
runAudio eff =
  withEffToIO $ \runInIO ->
    Mix.withAudio Mix.defaultAudio 4096 $ do
      finishMap <- initSDLFinishedMap
      runInIO (evalStaticRep (I.AudioRep (makeBackendSDL finishMap)) eff)