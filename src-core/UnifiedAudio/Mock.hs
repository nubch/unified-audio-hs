{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module UnifiedAudio.Mock
  ( runAudio,
  )
where

import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static (evalStaticRep)
import Data.Kind (Type)
import UnifiedAudio.Effectful

data MockSound :: Status -> Type where
  LoadedSound :: String -> MockSound Loaded
  PlayingSound :: String -> MockSound Playing
  PausedSound :: String -> MockSound Paused
  StoppedSound :: String -> MockSound Stopped

mockBackend :: AudioBackend MockSound
mockBackend =
  AudioBackend
    { loadA = \fp -> do
        putStrLn $ prefix ++ "Loading sound: " ++ fp
        pure $ LoadedSound (fp ++ "- LOADED"),
      playA = \(LoadedSound i) -> do
        putStrLn $ prefix ++ "Playing " ++ i
        pure (PlayingSound i),
      pauseA = \(PlayingSound i) -> do
        putStrLn $ "paused " ++ i
        pure (PausedSound i),
      resumeA = \(PausedSound i) -> do
        putStrLn $ "resumed" ++ i
        pure (PlayingSound i),
      setVolumeA = \(PlayingSound pl) vol -> do
        putStrLn $ prefix ++ "Setting volume of " ++ pl ++ " to " ++ show vol,
      setPanningA = \(PlayingSound pl) pan -> do
        putStrLn $ prefix ++ "Setting panning of " ++ pl ++ " to " ++ show pan,
      stopChannelA = \(PlayingSound pl) -> do
        putStrLn $ prefix ++ "Stopping channel " ++ pl
        pure (StoppedSound pl)
    }

prefix :: String
prefix = "[Mock] -> "

runAudio :: (IOE :> es) => Eff (Audio MockSound : es) a -> Eff es a
runAudio = evalStaticRep (AudioRep mockBackend)
