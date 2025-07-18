{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Mock
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

mockBackend :: AudioBackend MockSound
mockBackend =
  AudioBackend
    { loadA = \fp -> do
        putStrLn $ prefix ++ "Loading sound: " ++ fp
        pure $ LoadedSound "-loaded_",
      playA = \(LoadedSound i) -> do
        putStrLn $ prefix ++ "Playing " ++ i
        pure (PlayingSound i),
      pauseA = \(PlayingSound i) -> do
        putStrLn $ "paused " ++ i
        pure (PausedSound i),
      resumeA = \(PausedSound i) -> do
        putStrLn $ "resumed" ++ i
        pure (PlayingSound i)
      --setVolumeB = \(Channel pl) vol -> do
      --  putStrLn $ prefix ++ "Setting volume of " ++ pl ++ " to " ++ show vol,
      --setPanningB = \(Channel pl) pan -> do
      --  putStrLn $ prefix ++ "Setting panning of " ++ pl ++ " to " ++ show pan
    }

prefix :: String
prefix = "[Mock] -> "

runAudio :: (IOE :> es) => Eff (Audio MockSound : es) a -> Eff es a
runAudio = evalStaticRep (AudioRep mockBackend)
