{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Mock
  ( runAudio,
    Channel,
  )
where

import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static (evalStaticRep)
import Interface
  ( AudioBackend (..),
    AudioEffect,
    StaticRep (AudioRep),
  )

newtype Channel = Channel String

mockBackend :: AudioBackend Channel
mockBackend =
  AudioBackend
    { playSoundB = \fp -> do
        putStrLn $ prefix ++ "Loading sound: " ++ fp
        pure $ Channel ("Playing " ++ fp),
      stopSoundB = \(Channel pl) -> do
        putStrLn $ prefix ++ "Stopping sound:" ++ pl
        pure (),
      setVolumeB = \(Channel pl) vol -> do
        putStrLn $ prefix ++ "Setting volume of " ++ pl ++ " to " ++ show vol,
      setPanningB = \(Channel pl) pan -> do
        putStrLn $ prefix ++ "Setting panning of " ++ pl ++ " to " ++ show pan
    }

prefix :: String
prefix = "[Mock] -> "

runAudio :: (IOE :> es) => Eff (AudioEffect Channel : es) a -> Eff es a
runAudio = evalStaticRep (AudioRep mockBackend)
