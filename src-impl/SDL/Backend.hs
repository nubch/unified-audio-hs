{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module SDL.Backend
  ( runAudio
  , SDLSound
  ) where

-- base / std
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- effectful
import Effectful (IOE, type (:>), Eff, withEffToIO)
import Effectful.Dispatch.Static (evalStaticRep)

-- sdl
import qualified SDL.Mixer as Mix

-- interface
import qualified UnifiedAudio.Effectful as I

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type FinishMap = MVar (Map.Map Mix.Channel Finished)

type Finished = MVar ()

-- Base per-channel state (not including group effects)
data ChannelState = ChannelState
  { chPanning :: I.Panning
  , chVolume  :: I.Volume
  , chPaused  :: Bool -- base paused state
  , chType    :: I.SoundType
  }

type ChannelStateMap = MVar (Map.Map Mix.Channel ChannelState)

data GroupSDL = GroupSDL
  { gPaused  :: Bool
  , gMembers :: Set.Set Mix.Channel
  , gVolume  :: I.Volume
  , gPanning :: I.Panning
  }

type GroupMap = MVar (Map.Map Int GroupSDL)
type ChanGroupMap = MVar (Map.Map Mix.Channel Int) -- reverse index: channel -> group id

data EnvSDL = EnvSDL
  { finishMap   :: FinishMap
  , chanState   :: ChannelStateMap
  , groupMap    :: GroupMap
  , chanGroupMap :: ChanGroupMap
  , groupCounter :: MVar Int
  }

data SDLSound :: I.Status -> Type where
  LoadedSound    :: Mix.Chunk -> I.SoundType -> SDLSound I.Loaded
  UnloadedSound  :: SDLSound I.Unloaded
  PlayingChannel :: Mix.Channel -> Finished -> SDLSound I.Playing
  PausedChannel  :: Mix.Channel -> Finished -> SDLSound I.Paused
  StoppedChannel :: SDLSound I.Stopped

----------------------------------------------------------------
-- Init / Finalization
----------------------------------------------------------------

initSDLEnv :: IO EnvSDL
initSDLEnv = do
  chStateVar <- newMVar Map.empty
  finMap <- newMVar Map.empty
  gMap   <- newMVar Map.empty
  cgMap  <- newMVar Map.empty
  gCounter <- newMVar 0
  Mix.whenChannelFinished $ \ch -> do
    modifyMVar_ finMap $ \m ->
      case Map.lookup ch m of
        Just done -> do
          _ <- tryPutMVar done ()
          pure (Map.delete ch m)
        Nothing   -> pure m
    modifyMVar_ chStateVar $ \m -> pure (Map.delete ch m)
    modifyMVar_ cgMap $ \cgm -> pure (Map.delete ch cgm)
    -- prune from all groups
    modifyMVar_ gMap $ \gm ->
      let prune gr = gr { gMembers = Set.delete ch (gMembers gr) }
      in pure (Map.map prune gm)
  pure EnvSDL { finishMap = finMap
              , chanState = chStateVar
              , groupMap = gMap
              , chanGroupMap = cgMap
              , groupCounter = gCounter
              }


----------------------------------------------------------------
-- Loading
----------------------------------------------------------------

loadSDL :: I.Source -> I.SoundType -> IO (SDLSound I.Loaded)
loadSDL src sType =
  case src of
    I.FromFile fp  -> loadFromFile fp
    I.FromBytes by -> loadFromBytes by
    where
      loadFromFile fp = do
        loaded <- Mix.load fp
        pure $ LoadedSound loaded sType
      loadFromBytes bytes = do
        decoded <- Mix.decode bytes
        pure $ LoadedSound decoded sType

unloadSDL :: SDLSound I.Loaded -> IO (SDLSound I.Unloaded)
unloadSDL (LoadedSound chunk _) =
  Mix.free chunk >> pure UnloadedSound

----------------------------------------------------------------
-- Play / Pause / Resume / Stop / Status
----------------------------------------------------------------

playSDL :: EnvSDL -> SDLSound I.Loaded -> I.LoopMode -> IO (SDLSound I.Playing)
playSDL env (LoadedSound loaded sType) times = do
  channel <- Mix.playOn Mix.AllChannels sdlLoopMode loaded
  done    <- newEmptyMVar
  modifyMVar_ env.finishMap $ \m -> pure $ Map.insert channel done m
  -- Initialize per-channel state
  modifyMVar_ env.chanState $ \m ->
    let cs = ChannelState { chPanning = I.defaultPanning
                          , chVolume  = I.defaultVolume
                          , chPaused  = False
                          , chType    = sType
                          }
    in pure (Map.insert channel cs m)

  -- Reset pan and volume if channel gets reused
  let playingChannel = PlayingChannel channel done
  setPanningSDL env playingChannel I.defaultPanning
  setVolumeSDL env playingChannel I.defaultVolume
  pure playingChannel
  where
    sdlLoopMode = case times of
      I.Once    -> Mix.Once
      I.Forever -> Mix.Forever

pauseSDL :: EnvSDL -> SDLSound I.Playing -> IO (SDLSound I.Paused)
pauseSDL env (PlayingChannel channel finished) = do
  modifyMVar_ env.chanState $ \m ->
    let upd cs = cs { chPaused = True }
    in pure (Map.adjust upd channel m)
  -- Apply final paused = basePaused || groupPaused
  applyPauseState env channel
  return (PausedChannel channel finished)

resumeSDL :: EnvSDL -> SDLSound I.Paused -> IO (SDLSound I.Playing)
resumeSDL env (PausedChannel channel finished) = do
  modifyMVar_ env.chanState $ \m ->
    let upd cs = cs { chPaused = False }
    in pure (Map.adjust upd channel m)
  -- Apply final paused = basePaused || groupPaused
  applyPauseState env channel
  return (PlayingChannel channel finished)

-- Stop a channel and clean all associated state (maps, groups, reverse index).
stopChannelAndCleanup :: EnvSDL -> Mix.Channel -> IO ()
stopChannelAndCleanup env channel = do
  Mix.halt channel
  -- Signal finished if present and remove from finish map
  modifyMVar_ env.finishMap $ \m -> case Map.lookup channel m of
    Just done -> do _ <- tryPutMVar done (); pure (Map.delete channel m)
    Nothing   -> pure m
  -- Remove channel state entry
  modifyMVar_ env.chanState    $ \m -> pure (Map.delete channel m)
  modifyMVar_ env.chanGroupMap $ \cgm -> pure (Map.delete channel cgm)
  -- Prune the channel from all groups' membership sets
  modifyMVar_ env.groupMap $ \gm ->
    let prune gr = gr { gMembers = Set.delete channel (gMembers gr) }
    in pure (Map.map prune gm)

stopSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO (SDLSound I.Stopped)
stopSDL env stoppable =
  case stoppable of
    (PlayingChannel channel _) -> do stopChannelAndCleanup env channel; pure StoppedChannel
    (PausedChannel  channel _) -> do stopChannelAndCleanup env channel; pure StoppedChannel

hasFinishedSDL :: SDLSound I.Playing -> IO Bool
hasFinishedSDL (PlayingChannel _ finished) = do
  fin <- isEmptyMVar finished
  pure $ not fin

awaitFinishedSDL :: SDLSound I.Playing -> IO ()
awaitFinishedSDL (PlayingChannel _ finished) =
  readMVar finished

----------------------------------------------------------------
-- Volume / Panning (and helpers)
----------------------------------------------------------------

setVolumeSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> I.Volume -> IO ()
setVolumeSDL env adjustable vol =
  case adjustable of
    (PlayingChannel channel _) -> setV channel
    (PausedChannel  channel _) -> setV channel
  where
    setV channel = do
      modifyMVar_ env.chanState $ \m ->
        let upd cs = cs { chVolume = vol }
            cs0    = ChannelState I.defaultPanning I.defaultVolume False I.Stereo
        in pure (Map.insert channel (upd (Map.findWithDefault cs0 channel m)) m)
      -- recompute effective volume = base * group
      gVol <- getGroupVolumeForChannel env channel
      let eff = toSDLVolume (mulVolume vol gVol)
      Mix.setVolume eff channel

getVolumeSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO I.Volume
getVolumeSDL env adjustable =
  case adjustable of
    (PlayingChannel channel _) -> getV channel
    (PausedChannel  channel _) -> getV channel
  where
    getV ch = do
      m <- readMVar env.chanState
      pure $ maybe I.defaultVolume chVolume (Map.lookup ch m)

setPanningSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> I.Panning -> IO ()
setPanningSDL env adjustable pan =
  case adjustable of
    (PlayingChannel channel _) -> setPan channel
    (PausedChannel  channel _) -> setPan channel
  where
    setPan :: Mix.Channel -> IO ()
    setPan channel = do
      -- get sound type from channel state
      m <- readMVar env.chanState
      let sType = maybe I.Stereo chType (Map.lookup channel m)
      -- store base pan
      modifyMVar_ env.chanState $ \m2 ->
        let upd cs = cs { chPanning = pan }
            cs0    = ChannelState I.defaultPanning I.defaultVolume False sType
        in pure (Map.insert channel (upd (Map.findWithDefault cs0 channel m2)) m2)
      -- recompute effective pan by multiplying L/R gains from base and group
      gPan <- getGroupPanningForChannel env channel
      let (lBase, rBase) = case sType of
                             I.Mono   -> panGainsMono   (I.unPanning pan)
                             I.Stereo -> panGainsStereo (I.unPanning pan)
          (lGrp,  rGrp)  = case sType of
                             I.Mono   -> panGainsMono   (I.unPanning gPan)
                             I.Stereo -> panGainsStereo (I.unPanning gPan)
          lEff = round (128 * clamp01 (lBase * lGrp))
          rEff = round (128 * clamp01 (rBase * rGrp))
      void $ Mix.effectPan channel lEff rEff

getPanningSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO I.Panning
getPanningSDL env adjustable =
  case adjustable of
    (PlayingChannel ch _) -> getPan ch
    (PausedChannel  ch _) -> getPan ch
  where
    getPan ch = do
      m <- readMVar env.chanState
      pure $ maybe (I.mkPanning 0) chPanning (Map.lookup ch m)

-- Helpers: volumes and panning math
mulVolume :: I.Volume -> I.Volume -> I.Volume
mulVolume a b = I.mkVolume (I.unVolume a * I.unVolume b)

-- Convert pan in [-1,1] to per-side gains (0..1)
panGainsStereo :: Float -> (Float, Float)
panGainsStereo x0 =
  let x = max (-1) (min 1 x0)
      l = 1 - max 0 x      -- reduce LEFT only when panning right
      r = 1 + min 0 x      -- reduce RIGHT only when panning left
  in (l, r)                -- center => (1,1)

panGainsMono :: Float -> (Float, Float)
panGainsMono x0 =
  let x  = max (-1) (min 1 x0)
      gL = sqrt ((1 - x) / 2)  -- equal-power
      gR = sqrt ((1 + x) / 2)
  in (gL, gR)

clamp01 :: Float -> Float
clamp01 = max 0 . min 1

toSDLVolume :: I.Volume -> Int
toSDLVolume vol = round (volume * 128)
  where
    volume = I.unVolume vol

----------------------------------------------------------------
-- Backend wiring / Runner
----------------------------------------------------------------

makeBackendSDL :: EnvSDL -> I.AudioBackend SDLSound
makeBackendSDL env =
  I.AudioBackend
    { I.playA          = playSDL env
    , I.stopChannelA   = stopSDL env
    , I.loadA          = loadSDL
    , I.pauseA         = pauseSDL env
    , I.resumeA        = resumeSDL env
    , I.setPanningA    = setPanningSDL env
    , I.getPanningA    = getPanningSDL env
    , I.getVolumeA     = getVolumeSDL env
    , I.setVolumeA     = setVolumeSDL env
    , I.unloadA        = unloadSDL
    , I.hasFinishedA   = hasFinishedSDL
    , I.awaitFinishedA = awaitFinishedSDL
    , I.makeGroupA     = makeGroupSDL env
    , I.addToGroupA    = addToGroupSDL env
    , I.removeFromGroupA = removeFromGroupSDL env
    , I.pauseGroupA    = pauseGroupSDL env
    , I.resumeGroupA   = resumeGroupSDL env
    , I.stopGroupA     = stopGroupSDL env
    , I.isGroupPausedA = isGroupPausedSDL env
    , I.setGroupVolumeA = setGroupVolumeSDL env
    , I.getGroupVolumeA = getGroupVolumeSDL env
    , I.setGroupPanningA = setGroupPanningSDL env
    , I.getGroupPanningA = getGroupPanningSDL env
    }

runAudio :: (IOE :> es) => Eff (I.Audio SDLSound : es) a -> Eff es a
runAudio eff =
  withEffToIO $ \runInIO -> do
    let hiFi = Mix.Audio
              { Mix.audioFrequency = 44100
              , Mix.audioFormat    = Mix.FormatS16_Sys
              , Mix.audioOutput  = Mix.Stereo
              }
    Mix.withAudio hiFi 1024 $ do
      env <- initSDLEnv
      runInIO (evalStaticRep (I.AudioRep (makeBackendSDL env)) eff)

----------------------------------------------------------------
-- Groups (SDL)
----------------------------------------------------------------

makeGroupSDL :: EnvSDL -> IO (I.Group SDLSound)
makeGroupSDL env = do
  gid <- modifyMVar env.groupCounter $ \n ->
    let next = n + 1
    in pure (next, n)
  let group = GroupSDL
        { gPaused = False
        , gMembers = Set.empty
        , gVolume = I.defaultVolume
        , gPanning = I.defaultPanning
        }
  modifyMVar_ env.groupMap $ \gm -> pure (Map.insert gid group gm)
  pure (I.GroupId gid)

addToGroupSDL :: forall alive. I.Alive alive => EnvSDL -> I.Group SDLSound -> SDLSound alive -> IO ()
addToGroupSDL env (I.GroupId gid) s = do
  ch <- case s of
    PlayingChannel c _ -> pure c
    PausedChannel  c _ -> pure c
  -- Enforce exclusive membership: remove from all groups, then add to target
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just _gr -> do
      -- remove channel from every group's membership set
      let gmCleared = Map.map (\gr -> gr { gMembers = Set.delete ch (gMembers gr) }) gm
      -- add channel to the target group's membership set
      let gmUpdated = Map.adjust (\gr -> gr { gMembers = Set.insert ch (gMembers gr) }) gid gmCleared
      pure gmUpdated
  -- update reverse index with exclusive membership
  modifyMVar_ env.chanGroupMap $ \cgm -> pure (Map.insert ch gid cgm)
  -- Apply group bus settings to the channel and correct pause state
  applyEffectiveForChannel env ch
  applyPauseState env ch

removeFromGroupSDL :: forall alive. I.Alive alive => EnvSDL -> I.Group SDLSound -> SDLSound alive -> IO ()
removeFromGroupSDL env (I.GroupId gid) s = do
  ch <- case s of
    PlayingChannel c _ -> pure c
    PausedChannel  c _ -> pure c
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr { gMembers = Set.delete ch (gMembers gr) }
      pure (Map.insert gid gr' gm)
  -- remove reverse index entry since membership is exclusive
  modifyMVar_ env.chanGroupMap $ \cgm -> pure (Map.delete ch cgm)
  -- Re-apply effective settings (likely reverting to base settings)
  applyEffectiveForChannel env ch
  applyPauseState env ch

pauseGroupSDL :: EnvSDL -> I.Group SDLSound -> IO ()
pauseGroupSDL env (I.GroupId gid) = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr{ gPaused = True }
      pure (Map.insert gid gr' gm)
  -- Apply pause to members (final pause = basePaused || groupPaused)
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyPauseState env) (Set.toList (gMembers gr))

resumeGroupSDL :: EnvSDL -> I.Group SDLSound -> IO ()
resumeGroupSDL env (I.GroupId gid) = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr{ gPaused = False }
      pure (Map.insert gid gr' gm)
  -- Force resume: clear base pause for all members and resume them
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> do
      let members = Set.toList (gMembers gr)
      modifyMVar_ env.chanState $ \m ->
        let m' = foldr (Map.adjust (\cs -> cs { chPaused = False })) m members
        in pure m'
      mapM_ (applyPauseState env) members

stopGroupSDL :: EnvSDL -> I.Group SDLSound -> IO ()
stopGroupSDL env (I.GroupId gid) = do
  -- Snapshot members, then stop each via common helper
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> do
      let members = Set.toList (gMembers gr)
      mapM_ (stopChannelAndCleanup env) members
      -- After stopping, clear membership set for the group (redundant but explicit)
      modifyMVar_ env.groupMap $ \gm2 -> case Map.lookup gid gm2 of
        Nothing  -> pure gm2
        Just gr2 -> pure (Map.insert gid gr2{ gMembers = Set.empty } gm2)

-- Note: SDL Mixer has no group volume; we approximate by setting member volumes directly.
setGroupVolumeSDL :: EnvSDL -> I.Group SDLSound -> I.Volume -> IO ()
setGroupVolumeSDL env (I.GroupId gid) vol = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> pure (Map.insert gid gr{ gVolume = vol } gm)
  -- Apply to current members: effective = base * group
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyEffectiveForChannel env) (Set.toList (gMembers gr))

-- Approximate group panning by applying stereo balance to members.
setGroupPanningSDL :: EnvSDL -> I.Group SDLSound -> I.Panning -> IO ()
setGroupPanningSDL env (I.GroupId gid) pan = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> pure (Map.insert gid gr{ gPanning = pan } gm)
  -- Apply to current members: effective = clamp(base + group)
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyEffectiveForChannel env) (Set.toList (gMembers gr))

getGroupVolumeSDL :: EnvSDL -> I.Group SDLSound -> IO I.Volume
getGroupVolumeSDL env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure I.defaultVolume
    Just gr -> pure (gVolume gr)

getGroupPanningSDL :: EnvSDL -> I.Group SDLSound -> IO I.Panning
getGroupPanningSDL env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure I.defaultPanning
    Just gr -> pure (gPanning gr)

-- Queries
isGroupPausedSDL :: EnvSDL -> I.Group SDLSound -> IO Bool
isGroupPausedSDL env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure False
    Just gr -> pure (gPaused gr)

-- removed: isGroupStoppedSDL (no longer part of API)

----------------------------------------------------------------
-- Internal helpers: effective state application
----------------------------------------------------------------

-- Apply effective volume and panning to a channel based on its base state and group
applyEffectiveForChannel :: EnvSDL -> Mix.Channel -> IO ()
applyEffectiveForChannel env ch = do
  -- volume
  baseV <- do
    m <- readMVar env.chanState
    pure $ maybe I.defaultVolume chVolume (Map.lookup ch m)
  gVol  <- getGroupVolumeForChannel env ch
  Mix.setVolume (toSDLVolume (mulVolume baseV gVol)) ch
  -- panning: multiply L/R gains from base and group
  baseP <- do
    m <- readMVar env.chanState
    pure $ maybe I.defaultPanning chPanning (Map.lookup ch m)
  gPan  <- getGroupPanningForChannel env ch
  sType <- do
    m <- readMVar env.chanState
    pure $ maybe I.Stereo chType (Map.lookup ch m)
  let (lBase, rBase) = case sType of
                         I.Mono   -> panGainsMono   (I.unPanning baseP)
                         I.Stereo -> panGainsStereo (I.unPanning baseP)
      (lGrp,  rGrp)  = case sType of
                         I.Mono   -> panGainsMono   (I.unPanning gPan)
                         I.Stereo -> panGainsStereo (I.unPanning gPan)
      lEff = round (128 * clamp01 (lBase * lGrp))
      rEff = round (128 * clamp01 (rBase * rGrp))
  void $ Mix.effectPan ch lEff rEff

-- Apply pause state considering base paused and group paused
applyPauseState :: EnvSDL -> Mix.Channel -> IO ()
applyPauseState env ch = do
  basePaused <- do
    m <- readMVar env.chanState
    pure $ maybe False chPaused (Map.lookup ch m)
  grpPaused  <- getGroupPausedForChannel env ch
  if basePaused || grpPaused then Mix.pause ch else Mix.resume ch

-- Lookup helpers for group-based properties
getGroupForChannel :: EnvSDL -> Mix.Channel -> IO (Maybe GroupSDL)
getGroupForChannel env ch = do
  mGid <- Map.lookup ch <$> readMVar env.chanGroupMap
  case mGid of
    Nothing   -> pure Nothing
    Just gid -> do
      gm <- readMVar env.groupMap
      pure (Map.lookup gid gm)

getGroupVolumeForChannel :: EnvSDL -> Mix.Channel -> IO I.Volume
getGroupVolumeForChannel env ch = do
  mGr <- getGroupForChannel env ch
  pure $ maybe I.defaultVolume gVolume mGr

getGroupPanningForChannel :: EnvSDL -> Mix.Channel -> IO I.Panning
getGroupPanningForChannel env ch = do
  mGr <- getGroupForChannel env ch
  pure $ maybe I.defaultPanning gPanning mGr

getGroupPausedForChannel :: EnvSDL -> Mix.Channel -> IO Bool
getGroupPausedForChannel env ch = do
  mGr <- getGroupForChannel env ch
  pure $ maybe False gPaused mGr
