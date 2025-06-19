{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module SDL.SDL
  ( runAudio,
    PlayingHandle
  )
where

import Effectful
import Effectful.Dispatch.Static
import Interface
import qualified SDL
import qualified SDL.Mixer as Mix

type SystemHandle = ()

type SoundHandle = Mix.Chunk

type PlayingHandle = Mix.Channel

initSDL :: IO SystemHandle
initSDL = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 4096
  putStrLn "SDL Init completed."
  return ()

loadSDL :: FilePath -> IO SoundHandle
loadSDL fp = do
  Mix.load fp

playSDL :: FilePath -> IO PlayingHandle
playSDL fp = do
  chunk <- loadSDL fp
  Mix.playOn 1 Mix.Once chunk

stopSDL :: PlayingHandle -> IO ()
stopSDL channel = do
  Mix.halt channel

setVolumeSDL :: Volume -> PlayingHandle -> IO ()
setVolumeSDL vol playing = do 
  Mix.setVolume (toSDLVolume vol) playing

toSDLVolume :: Volume -> Int
toSDLVolume vol = round (volume * 128)
  where volume = unVolume vol

makeBackendSDL :: AudioBackend PlayingHandle
makeBackendSDL =
  AudioBackend
    { playSoundB = playSDL,
      stopSoundB = stopSDL,
      setVolumeB = setVolumeSDL
    }


runAudio :: (IOE :> es) => Eff (AudioEffect PlayingHandle : es) a -> Eff es a
runAudio eff = do
  unsafeEff_ initSDL
  evalStaticRep (AudioRep makeBackendSDL) eff