{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Mock
  ( runAudio,
    playSound,
    stopSound,
  )
where

import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static
  ( evalStaticRep,
    getStaticRep,
    unsafeEff_,
  )
import Interface
  ( AudioBackend (..),
    AudioEffect,
    StaticRep (AudioRep),
  )

newtype MockPlaying = MockPlaying String

playSound :: (AudioEffect MockPlaying :> es) => FilePath -> Eff es MockPlaying
playSound s = do
  AudioRep (AudioBackend play _ _) <- getStaticRep
  unsafeEff_ $ play s

stopSound :: (AudioEffect MockPlaying :> es) => MockPlaying -> Eff es ()
stopSound s = do
  AudioRep (AudioBackend _ stop _ ) <- getStaticRep
  unsafeEff_ $ stop s

mockBackend :: AudioBackend MockPlaying
mockBackend =
  AudioBackend
    { playSoundB = \fp -> do
        putStrLn $ prefix ++ "Loading sound: " ++ fp
        pure $ MockPlaying ("Playing " ++ fp),
      stopSoundB = \(MockPlaying pl) -> do
        putStrLn $ prefix ++ "Stopping sound:" ++ pl
        pure (),
      setVolumeB = \vol (MockPlaying pl) -> do
        putStrLn $ prefix ++ "Setting volume of " ++ pl ++ " to " ++ show vol
    }
  
prefix :: String
prefix = "[Mock] -> "

runAudio :: (IOE :> es) => Eff (AudioEffect MockPlaying : es) a -> Eff es a
runAudio = evalStaticRep (AudioRep mockBackend)
