module Audio.SDL
  ( backendSDL, playSDL, loadSDL, stopSDL
  )
where

import Audio.Interface
    ( AudioBackend(..) )
import qualified SDL
import qualified SDL.Mixer as Mix

type SystemHandle = ()
type SoundHandle = Mix.Chunk
type PlayingHandle = Mix.Channel

initSDL :: IO SystemHandle
initSDL = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 4096
  return ()

loadSDL :: SystemHandle -> FilePath -> IO SoundHandle
loadSDL _ fp = do
  Mix.load fp

playSDL :: SystemHandle -> SoundHandle -> IO PlayingHandle
playSDL _ chunk = do
  Mix.playOn 1 Mix.Once chunk

stopSDL :: SystemHandle -> PlayingHandle -> IO ()
stopSDL _ channel = do
  Mix.halt channel

backendSDL :: AudioBackend SystemHandle SoundHandle PlayingHandle
backendSDL = AudioBackend {
    initAudio = initSDL,
    loadSound = loadSDL,
    playSound = playSDL,
    stopSound = stopSDL
}
