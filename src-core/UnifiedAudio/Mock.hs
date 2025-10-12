{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module UnifiedAudio.Mock
where

import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Static (evalStaticRep)
import Data.Kind (Type)
import UnifiedAudio.Effectful
    ( AudioBackend(..),
      Audio,
      Status(..),
      Group(GroupId),
      Source(FromBytes, FromFile),
      defaultPlacement,
      defaultVolume )

data MockSound :: Status -> Type where
  LoadedSound :: String -> MockSound Loaded
  PlayingSound :: String -> MockSound Playing
  PausedSound :: String -> MockSound Paused
  StoppedSound :: String -> MockSound Stopped
  UnloadedSound :: MockSound Unloaded

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
      setVolumeA = \ch vol -> case ch of
        PlayingSound pl -> logMock $ "Setting volume of " ++ pl ++ " to " ++ show vol
        PausedSound  pl -> logMock $ "Setting volume of (paused) " ++ pl ++ " to " ++ show vol,
      getVolumeA = \_ -> pure defaultVolume,
      setPlacementA = \ch pan -> case ch of
        PlayingSound pl -> logMock $ "Setting Placement of " ++ pl ++ " to " ++ show pan
        PausedSound  pl -> logMock $ "Setting Placement of (paused) " ++ pl ++ " to " ++ show pan,
      getPlacementA = \_ -> pure defaultPlacement,
      stopChannelA = \ch -> case ch of
        PlayingSound pl -> do logMock $ "Stopping channel " ++ pl; pure (StoppedSound pl)
        PausedSound  pl -> do logMock $ "Stopping channel (paused) " ++ pl; pure (StoppedSound pl),
      hasFinishedA = \(PlayingSound pl) -> do
        logMock $ "Checking if " ++ pl ++ " has finished"; pure False,
      awaitFinishedA = \(PlayingSound pl) -> logMock $ "Await finished for " ++ pl,
      unloadA = \(LoadedSound i) -> do
        logMock $ "Unloading " ++ i
        pure UnloadedSound,
      makeGroupA = do
        logMock "makeGroup"
        pure (GroupId 0),
      addToGroupA = \(GroupId gid) ch -> case ch of
        PlayingSound pl -> logMock $ "addToGroup gid=" ++ show gid ++ " ch=" ++ pl
        PausedSound  pl -> logMock $ "addToGroup gid=" ++ show gid ++ " ch(paused)=" ++ pl,
      removeFromGroupA = \(GroupId gid) ch -> case ch of
        PlayingSound pl -> logMock $ "removeFromGroup gid=" ++ show gid ++ " ch=" ++ pl
        PausedSound  pl -> logMock $ "removeFromGroup gid=" ++ show gid ++ " ch(paused)=" ++ pl,
      pauseGroupA = \(GroupId gid) -> logMock $ "pauseGroup gid=" ++ show gid,
      resumeGroupA = \(GroupId gid) -> logMock $ "resumeGroup gid=" ++ show gid,
      stopGroupA = \(GroupId gid) -> logMock $ "stopGroup gid=" ++ show gid,
      isGroupPausedA = \(GroupId gid) -> do
        logMock $ "isGroupPaused gid=" ++ show gid
        pure False,
      setGroupVolumeA = \(GroupId gid) vol -> logMock $ "setGroupVolume gid=" ++ show gid ++ " vol=" ++ show vol,
      getGroupVolumeA = \_ -> pure defaultVolume,
      setGroupPlacementA = \(GroupId gid) pan -> logMock $ "setGroupPlacement gid=" ++ show gid ++ " pan=" ++ show pan
    , getGroupPlacementA = \_ -> pure defaultPlacement
    }

logMock :: String -> IO ()
logMock msg = putStrLn $ prefix ++ msg
  where prefix = "[Mock] -> "
