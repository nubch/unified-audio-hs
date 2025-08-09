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
        logMock $ "Loading sound: " ++ fp
        pure $ LoadedSound (fp ++ "- LOADED"),
      playA = \(LoadedSound i) times -> do
        logMock $ "Playing " ++ i ++ "times: " ++ show times
        pure (PlayingSound i),
      pauseA = \(PlayingSound i) -> do
        logMock $ "paused " ++ i
        pure (PausedSound i),
      resumeA = \(PausedSound i) -> do
        logMock $ "resumed" ++ i
        pure (PlayingSound i),
      setVolumeA = \(PlayingSound pl) vol -> do
        logMock $ "Setting volume of " ++ pl ++ " to " ++ show vol,
      setPanningA = \(PlayingSound pl) pan -> do
        logMock $ "Setting panning of " ++ pl ++ " to " ++ show pan,
      stopChannelA = \(PlayingSound pl) -> do
        logMock $ "Stopping channel " ++ pl
        pure (StoppedSound pl),
      isPlayingA = \(PlayingSound _) -> pure True, --always true for mock,
      onFinishedA = \callb pl  -> do
        logMock "Mock cant detect on finished, calling callback immediately"
        callb pl

    }

logMock :: String -> IO ()
logMock msg = putStrLn $ prefix ++ msg

prefix :: String
prefix = "[Mock] -> "

runAudio :: (IOE :> es) => Eff (Audio MockSound : es) a -> Eff es a
runAudio = evalStaticRep (AudioRep mockBackend)
