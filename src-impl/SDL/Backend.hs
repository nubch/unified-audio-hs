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
import qualified Data.Map.Strict as Map

import Control.Concurrent.MVar

-- SDL Library
import qualified SDL.Mixer as Mix

-- Interface
import qualified UnifiedAudio.Effectful as I
import Control.Monad ( void )
import Data.Kind (Type)

loadSDL :: FilePath -> IO (SDLSound I.Loaded)
loadSDL fp = do
  loaded <- Mix.load fp
  pure $ LoadedSound loaded

type FinishMap = MVar (Map.Map Mix.Channel (MVar ()))

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

playSDL :: FinishMap -> SDLSound I.Loaded -> I.Times -> IO (SDLSound I.Playing)
playSDL fm (LoadedSound loaded) times = do
  channel  <- Mix.playOn Mix.AllChannels sdlTimes loaded       
  done <- newEmptyMVar 

  modifyMVar_ fm $ \m ->
    pure $ Map.insert channel done m

  pure (PlayingSound channel done)
  where
    sdlTimes = case times of
      I.Once    -> Mix.Once
      I.Times n -> fromIntegral n
      I.Forever -> Mix.Forever

resumeSDL :: SDLSound I.Paused -> IO (SDLSound I.Playing)
resumeSDL (PausedSound channel finished) =
  Mix.resume channel >> return (PlayingSound channel finished)

stopSDL :: FinishMap -> SDLSound I.Playing -> IO (SDLSound I.Stopped)
stopSDL fm (PlayingSound channel finished) = do
  Mix.halt channel
  modifyMVar_ fm $ \m -> pure (Map.delete channel m)
  _ <- tryPutMVar finished ()
  return (StoppedSound channel)

pauseSDL :: SDLSound I.Playing -> IO (SDLSound I.Paused)
pauseSDL (PlayingSound channel finished) =
  Mix.pause channel >> return (PausedSound channel finished)

setVolumeSDL :: SDLSound I.Playing -> I.Volume -> IO ()
setVolumeSDL (PlayingSound channel _) vol =
  Mix.setVolume (toSDLVolume vol) channel

setPanningSDL :: SDLSound I.Playing -> I.Panning -> IO ()
setPanningSDL (PlayingSound channel _) pan = do
  let (left, right) = toSDLPanning pan
  void $ Mix.effectPan channel left right

hasFinishedSDL :: SDLSound I.Playing -> IO Bool
hasFinishedSDL (PlayingSound _ finished) = do 
  fin <- isEmptyMVar finished
  pure $ not fin

toSDLVolume :: I.Volume -> Int
toSDLVolume vol = round (volume * 128)
  where volume = I.unVolume vol

toSDLPanning :: I.Panning -> (Int, Int)
toSDLPanning pan =
  let panning = I.unPanning pan
      left    = round $ 64 * (1 - panning)
      right   = round $ 64 * (1 + panning)
  in (left, right)

makeBackendSDL :: FinishMap -> I.AudioBackend SDLSound
makeBackendSDL fm =
  I.AudioBackend
    { I.playA  = playSDL fm,
      I.stopChannelA = stopSDL fm,
      I.loadA  = loadSDL,
      I.pauseA  = pauseSDL,
      I.resumeA = resumeSDL,
      I.setPanningA = setPanningSDL,
      I.setVolumeA = setVolumeSDL,
      I.hasFinishedA = hasFinishedSDL
    }

data SDLSound :: I.Status -> Type where
  LoadedSound  :: Mix.Chunk -> SDLSound I.Loaded
  PlayingSound :: Mix.Channel -> MVar () -> SDLSound I.Playing
  PausedSound  :: Mix.Channel -> MVar () -> SDLSound I.Paused
  StoppedSound :: Mix.Channel -> SDLSound I.Stopped

runAudio :: (IOE :> es) => Eff (I.Audio SDLSound : es) a -> Eff es a
runAudio eff =
  withEffToIO $ \runInIO ->
    Mix.withAudio Mix.defaultAudio 4096 $ do
      finishMap <- initSDLFinishedMap
      runInIO (evalStaticRep (I.AudioRep (makeBackendSDL finishMap)) eff)