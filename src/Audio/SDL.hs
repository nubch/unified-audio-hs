module Audio.Backend.SDL
  ( backendSDL
  )
where

import Audio.Interface
    ( AudioBackend(..), PlayingHandle(..), SoundHandle(..) )
import qualified SDL
import qualified SDL.Mixer as Mix

initSDL :: IO ()
initSDL = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 4096

loadSDL :: FilePath -> IO SoundHandle
loadSDL fp = do
  chunk <- Mix.load fp
  return $ SDLSound chunk

playSDL :: SoundHandle -> IO PlayingHandle
playSDL (SDLSound chunk) = do
  channel <- Mix.playOn 1 Mix.Once chunk
  return $ SDLPlaying channel

stopSDL :: PlayingHandle -> IO ()
stopSDL (SDLPlaying channel) = do
  Mix.halt channel

backendSDL :: AudioBackend
backendSDL = AudioBackend {
    initAudio = initSDL,
    loadSound = loadSDL,
    playSound = playSDL,
    stopSound = stopSDL
}
