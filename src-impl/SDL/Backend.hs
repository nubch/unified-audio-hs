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

import Effectful
import Effectful.Dispatch.Static
import Interface
import qualified SDL
import qualified SDL.Mixer as Mix
import Control.Monad
import Data.Kind (Type)

type Channel = Mix.Channel

initSDL :: IO ()
initSDL = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 4096
  putStrLn "SDL Init completed."
  return ()

loadSDL :: FilePath -> IO (SDLSound Loaded)
loadSDL fp = do
  loaded <- Mix.load fp
  pure $ LoadedSound loaded

playSDL :: SDLSound Loaded -> IO (SDLSound Playing)
playSDL (LoadedSound loaded) = do
  mChannel <- Mix.getAvailable Mix.DefaultGroup
  case mChannel of 
    Just channel -> do 
      playing <- Mix.playOn channel Mix.Once loaded
      pure $ PlayingSound playing
    Nothing      -> error "No available SDL Channel!" -- Maybe handle this more gracefully

resumeSDL :: SDLSound Paused -> IO (SDLSound Playing)
resumeSDL (PausedSound playing) = do
  Mix.resume playing
  pure $ PlayingSound playing

stopSDL :: Channel -> IO ()
stopSDL channel = do
  Mix.halt channel

pauseSDL :: SDLSound Playing -> IO (SDLSound Paused)
pauseSDL (PlayingSound channel) = do
  Mix.pause channel
  pure $ PausedSound channel

setVolumeSDL :: Channel -> Volume -> IO ()
setVolumeSDL playing vol = do 
  Mix.setVolume (toSDLVolume vol) playing


setPanningSDL :: Channel -> Panning -> IO ()
setPanningSDL playing pan = do
  let (left, right) = toSDLPanning pan
  void $ Mix.effectPan playing left right
  

toSDLVolume :: Volume -> Int
toSDLVolume vol = round (volume * 128)
  where volume = unVolume vol

toSDLPanning :: Panning -> (Int, Int)
toSDLPanning pan = 
  let panning = unPanning pan
      left    = round $ 64 * (1 - panning)
      right   = round $ 64 * (1 + panning)
  in (left, right)


makeBackendSDL :: AudioBackend SDLSound
makeBackendSDL =
  AudioBackend
    { playA  = playSDL,
      loadA  = loadSDL,
      pauseA  = pauseSDL,
      resumeA = resumeSDL
      --setPanningB = setPanningSDL
    }

data SDLSound :: Status -> Type where
  LoadedSound :: Mix.Chunk -> SDLSound Loaded
  PlayingSound :: Mix.Channel -> SDLSound Playing
  PausedSound :: Mix.Channel -> SDLSound Paused

runAudio :: (IOE :> es) => Eff (Audio SDLSound : es) a -> Eff es a
runAudio eff = do
  unsafeEff_ initSDL
  evalStaticRep (AudioRep makeBackendSDL) eff