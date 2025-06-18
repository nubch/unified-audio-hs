{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Mock
  ( runAudio,
    playSound
  ) where

import Interface
import Effectful
import Effectful.Dispatch.Static

-- Mock SoundHandle
newtype MockPlaying = MockPlaying String

playSound :: AudioEffect MockPlaying :> es => FilePath -> Eff es MockPlaying
playSound s = do
  AudioRep (AudioBackend play _) <- getStaticRep
  unsafeEff_ $ play s

stopSound :: AudioEffect MockPlaying :> es => MockPlaying -> Eff es ()
stopSound s = do
  AudioRep (AudioBackend _ stop) <- getStaticRep
  unsafeEff_ $ stop s

-- Das eigentliche Backend
mockBackend :: AudioBackend MockPlaying
mockBackend =
  AudioBackend
    { playSoundB = \fp -> do
        putStrLn $ "[Mock] Loading sound: " ++ fp
        pure $ MockPlaying ("Playing " ++ fp),
      stopSoundB = \(MockPlaying pl) -> do
        putStrLn $ "[Mock] Stopping sound:" ++ pl
        pure ()
    }

-- Effekt-Runner
runAudio :: IOE :> es => Eff (AudioEffect MockPlaying : es) a -> Eff es a
runAudio = evalStaticRep (AudioRep mockBackend)
