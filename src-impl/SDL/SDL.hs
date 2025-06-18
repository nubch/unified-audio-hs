{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module SDL.SDL
  ( runAudio,
    playSound,
    stopSound,
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

setVolumeSDL :: Int -> PlayingHandle -> IO ()
setVolumeSDL vol playing = do 
  Mix.setVolume vol playing

makeBackendSDL :: AudioBackend PlayingHandle
makeBackendSDL =
  AudioBackend
    { playSoundB = playSDL,
      stopSoundB = stopSDL,
      setVolumeB = setVolumeSDL
    }

playSound :: (AudioEffect PlayingHandle :> es) => FilePath -> Eff es PlayingHandle
playSound fp = do
  AudioRep (AudioBackend play _ _) <- getStaticRep
  unsafeEff_ $ play fp

stopSound :: (AudioEffect PlayingHandle :> es) => PlayingHandle -> Eff es ()
stopSound playing = do
  AudioRep (AudioBackend _ stop _ ) <- getStaticRep
  unsafeEff_ $ stop playing

runAudio :: (IOE :> es) => Eff (AudioEffect PlayingHandle : es) a -> Eff es a
runAudio eff = do
  unsafeEff_ initSDL
  evalStaticRep (AudioRep makeBackendSDL) eff