module SDL.SDL
  ( backendSDL, playSDL, loadSDL, stopSDL
  )
where

import Interface
    ( AudioEffect(..), AudioBackend(..))
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

loadSDL :: SystemHandle -> FilePath -> IO SoundHandle
loadSDL _ fp = do
  Mix.load fp

playSDL :: SystemHandle -> SoundHandle -> IO PlayingHandle
playSDL _ chunk = do
  Mix.playOn 1 Mix.Once chunk

stopSDL :: SystemHandle -> PlayingHandle -> IO ()
stopSDL _ channel = do
  Mix.halt channel

backendSDL :: AudioBackend
backendSDL = AudioBackend {
    initAudioB = initSDL,
    loadSoundB = loadSDL,
    playSoundB = playSDL,
    stopSoundB = stopSDL
}
