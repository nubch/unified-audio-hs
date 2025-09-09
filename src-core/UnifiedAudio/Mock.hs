{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
    { loadA = \src st ->
        case src of
          FromFile fp -> do
            logMock $ "Loading from file" ++ show fp ++ show st ++ " sound: "
            pure $ LoadedSound (fp ++ "- LOADED")
          FromBytes _ -> do
            logMock $ "Loading from bytes " ++ show st
            pure $ LoadedSound "LOADED FROM BYTES",

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
      hasFinishedA = \(PlayingSound pl) -> do
        logMock $ "Checking if " ++ pl ++ " has finished"
        pure False
    }

logMock :: String -> IO ()
logMock msg = putStrLn $ prefix ++ msg
  where prefix = "[Mock] -> "

runAudio :: (IOE :> es) => Eff (Audio MockSound : es) a -> Eff es a
runAudio = evalStaticRep (AudioRep mockBackend)
