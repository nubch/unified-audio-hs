{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module SDL.Backend
  ( runAudio,
    SDLSound
  )
where

-- Effectful
import Effectful ( IOE, type (:>), Eff, withEffToIO )
import Effectful.Dispatch.Static ( evalStaticRep )

-- SDL Library
import qualified SDL.Mixer as Mix

-- Interface
import qualified UnifiedAudio.Effectful as I
import Control.Monad ( when, void )
import Data.Kind (Type)

loadSDL :: FilePath -> IO (SDLSound I.Loaded)
loadSDL fp = do
  loaded <- Mix.load fp
  pure $ LoadedSound loaded

playSDL :: SDLSound I.Loaded -> I.Times ->  IO (SDLSound I.Playing)
playSDL (LoadedSound loaded) times = do
  mChannel <- Mix.getAvailable Mix.DefaultGroup
  case mChannel of
    Just channel -> do
      playing <- Mix.playOn channel sdlTimes loaded
      pure $ PlayingSound playing
    Nothing      -> error "No available SDL Channel!" -- Maybe handle this more gracefully
    where sdlTimes = case times of
            I.Once -> Mix.Once
            I.Times n -> fromIntegral n
            I.Forever -> Mix.Forever

resumeSDL :: SDLSound I.Paused -> IO (SDLSound I.Playing)
resumeSDL (PausedSound playing) =
  Mix.resume playing >> return (PlayingSound playing)

stopSDL :: SDLSound I.Playing -> IO (SDLSound I.Stopped)
stopSDL (PlayingSound channel) =
  Mix.halt channel >> return (StoppedSound channel)

pauseSDL :: SDLSound I.Playing -> IO (SDLSound I.Paused)
pauseSDL (PlayingSound channel) =
  Mix.pause channel >> return (PausedSound channel)

setVolumeSDL :: SDLSound I.Playing -> I.Volume -> IO ()
setVolumeSDL (PlayingSound channel) vol =
  Mix.setVolume (toSDLVolume vol) channel

setPanningSDL :: SDLSound I.Playing -> I.Panning -> IO ()
setPanningSDL (PlayingSound channel) pan = do
  let (left, right) = toSDLPanning pan
  void $ Mix.effectPan channel left right

isPlayingSDL :: SDLSound I.Playing -> IO Bool
isPlayingSDL (PlayingSound channel) =
  Mix.playing channel

onFinishedSDL :: (SDLSound I.Playing -> IO ()) -> SDLSound I.Playing -> IO ()
onFinishedSDL callback pl@(PlayingSound channel) = do
  Mix.whenChannelFinished targetedCallBack
    where targetedCallBack ch = when (ch == channel) $ callback pl

toSDLVolume :: I.Volume -> Int
toSDLVolume vol = round (volume * 128)
  where volume = I.unVolume vol

toSDLPanning :: I.Panning -> (Int, Int)
toSDLPanning pan =
  let panning = I.unPanning pan
      left    = round $ 64 * (1 - panning)
      right   = round $ 64 * (1 + panning)
  in (left, right)

makeBackendSDL :: I.AudioBackend SDLSound
makeBackendSDL =
  I.AudioBackend
    { I.playA  = playSDL,
      I.loadA  = loadSDL,
      I.pauseA  = pauseSDL,
      I.resumeA = resumeSDL,
      I.setPanningA = setPanningSDL,
      I.setVolumeA = setVolumeSDL,
      I.stopChannelA = stopSDL,
      I.isPlayingA = isPlayingSDL,
      I.onFinishedA = onFinishedSDL
    }

data SDLSound :: I.Status -> Type where
  LoadedSound :: Mix.Chunk -> SDLSound I.Loaded
  PlayingSound :: Mix.Channel -> SDLSound I.Playing
  PausedSound :: Mix.Channel -> SDLSound I.Paused
  StoppedSound :: Mix.Channel -> SDLSound I.Stopped

runAudio :: (IOE :> es) => Eff (I.Audio SDLSound : es) a -> Eff es a
runAudio eff =
  withEffToIO $ \runInIO ->
    Mix.withAudio Mix.defaultAudio 4096 $
      runInIO (evalStaticRep (I.AudioRep makeBackendSDL) eff)