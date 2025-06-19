{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module SDL.SDL
  ( runAudio,
    Channel
  )
where

import Effectful
import Effectful.Dispatch.Static
import Interface
import qualified SDL
import qualified SDL.Mixer as Mix
import Control.Monad

type SystemHandle = ()

type SoundHandle = Mix.Chunk

type Channel = Mix.Channel

initSDL :: IO SystemHandle
initSDL = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 4096
  putStrLn "SDL Init completed."
  return ()

loadSDL :: FilePath -> IO SoundHandle
loadSDL fp = do
  Mix.load fp

playSDL :: FilePath -> IO Channel
playSDL fp = do
  chunk    <- loadSDL fp
  mChannel <- Mix.getAvailable Mix.DefaultGroup
  case mChannel of 
    Just channel -> Mix.playOn channel Mix.Once chunk :: IO Channel
    Nothing      -> error "No available SDL Channel!" -- Maybe handle this more gracefully

stopSDL :: Channel -> IO ()
stopSDL channel = do
  Mix.halt channel

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


makeBackendSDL :: AudioBackend Channel
makeBackendSDL =
  AudioBackend
    { playSoundB  = playSDL,
      stopSoundB  = stopSDL,
      setVolumeB  = setVolumeSDL,
      setPanningB = setPanningSDL
    }

runAudio :: (IOE :> es) => Eff (AudioEffect Channel : es) a -> Eff es a
runAudio eff = do
  unsafeEff_ initSDL
  evalStaticRep (AudioRep makeBackendSDL) eff