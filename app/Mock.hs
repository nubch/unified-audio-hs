{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Mock
  ( runAudio,
    MockPlaying,
  )
where

import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static (evalStaticRep)
import Interface
  ( AudioBackend (..),
    AudioEffect,
    StaticRep (AudioRep),
  )

newtype MockPlaying = MockPlaying String

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
