{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module SDL.Backend (runAudio) where

-- base / std
import Control.Concurrent.MVar
    ( MVar,
      newMVar,
      modifyMVar_,
      tryPutMVar,
      newEmptyMVar,
      isEmptyMVar,
      readMVar,
      modifyMVar )
import Control.Monad (void, when)
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
-- Backend wiring / Runner
----------------------------------------------------------------

-- | Construct the SDL implementation for the unified audio interface.
makeBackendSDL :: EnvSDL -> I.AudioBackend SDLSound
makeBackendSDL env =
  I.AudioBackend
    { I.playA          = playSDL env
    , I.stopChannelA   = stopSDL env
    , I.loadA          = loadSDL
    , I.pauseA         = pauseSDL env
    , I.resumeA        = resumeSDL env
    , I.setPlacementA    = setPlacementSDL env
    , I.getPlacementA    = getPlacementSDL env
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
    , I.setGroupPlacementA = setGroupPlacementSDL env
    , I.getGroupPlacementA = getGroupPlacementSDL env
    }

-- | Run the effectful computation with the SDL Handler.
runAudio :: (IOE :> es) => Eff (I.Audio SDLSound : es) a -> Eff es a
runAudio eff =
  withEffToIO $ \runInIO -> do
    let hiFi = Mix.Audio
              { Mix.audioFrequency = 44100
              , Mix.audioFormat    = Mix.FormatS16_Sys
              , Mix.audioOutput    = Mix.Stereo
              }
    Mix.withAudio hiFi 1024 $ do
      env <- initSDLEnv
      runInIO (evalStaticRep (I.AudioRep (makeBackendSDL env)) eff)

----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type FinishMap = MVar (Map.Map Mix.Channel Finished)

type Finished = MVar ()

-- Base per-channel state (not including group effects)
data ChannelState = ChannelState
  { chPlacement :: I.Placement
  , chVolume  :: I.Volume
  , chPaused  :: Bool
  , chType    :: I.SoundType
  }

type ChannelStateMap = MVar (Map.Map Mix.Channel ChannelState)

data GroupSDL = GroupSDL
  { gPaused  :: Bool
  , gMembers :: Set.Set Mix.Channel
  , gVolume  :: I.Volume
  , gPlacement :: I.Placement
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
-- Init
----------------------------------------------------------------

-- | Initialize the SDL backend environment and register a finish callback
-- that cleans up per-channel and group state when a channel ends.
initSDLEnv :: IO EnvSDL
initSDLEnv = do
  stateMap <- newMVar Map.empty
  finMap   <- newMVar Map.empty
  grpMap   <- newMVar Map.empty
  chGrpMap <- newMVar Map.empty
  gCounter <- newMVar 0
  Mix.whenChannelFinished $ \ch -> do
    modifyMVar_ finMap $ \m ->
      case Map.lookup ch m of
        Just done -> do
          _ <- tryPutMVar done ()
          pure (Map.delete ch m)
        Nothing   -> pure m
    modifyMVar_ stateMap $ \m -> pure (Map.delete ch m)
    modifyMVar_ chGrpMap $ \cgm -> pure (Map.delete ch cgm)
    -- prune from all groups
    modifyMVar_ grpMap $ \gm ->
      let prune gr = gr { gMembers = Set.delete ch (gMembers gr) }
      in pure (Map.map prune gm)
  pure EnvSDL { finishMap = finMap
              , chanState = stateMap
              , groupMap = grpMap
              , chanGroupMap = chGrpMap
              , groupCounter = gCounter
              }


----------------------------------------------------------------
-- Loading
----------------------------------------------------------------

-- | Load a sound from a file path or bytes.
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

-- | Free a loaded sound chunk.
unloadSDL :: SDLSound I.Loaded -> IO (SDLSound I.Unloaded)
unloadSDL (LoadedSound chunk _) =
  Mix.free chunk >> pure UnloadedSound

----------------------------------------------------------------
-- Play / Pause / Resume / Stop / Status
----------------------------------------------------------------

-- | Start playback for a loaded sound with loop mode. Initializes
-- per-channel base state and resets mixer volume/pan.
playSDL :: EnvSDL -> SDLSound I.Loaded -> I.LoopMode -> IO (SDLSound I.Playing)
playSDL env (LoadedSound loaded sType) times = do
  channel <- Mix.playOn Mix.AllChannels sdlLoopMode loaded
  done    <- newEmptyMVar
  modifyMVar_ env.finishMap $ \m -> pure $ Map.insert channel done m

  -- Initialize per-channel state
  modifyMVar_ env.chanState $ \m ->
    let cs = ChannelState { chPlacement = I.defaultPlacement
                          , chVolume  = I.defaultVolume
                          , chPaused  = False
                          , chType    = sType
                          }
    in pure (Map.insert channel cs m)

  -- Reset pan and volume if channel gets reused
  let playingChannel = PlayingChannel channel done
  setPlacementSDL env playingChannel I.defaultPlacement
  setVolumeSDL env playingChannel I.defaultVolume
  pure playingChannel
  where
    sdlLoopMode = case times of
      I.Once    -> Mix.Once
      I.Forever -> Mix.Forever

-- | Pause a playing channel by setting the channel's pause state.
pauseSDL :: EnvSDL -> SDLSound I.Playing -> IO (SDLSound I.Paused)
pauseSDL env (PlayingChannel channel finished) = do
  modifyMVar_ env.chanState $ \m ->
    let upd cs = cs { chPaused = True }
    in pure (Map.adjust upd channel m)
  -- Apply final paused = basePaused || groupPaused
  applyPauseState env channel
  return (PausedChannel channel finished)

-- | Resume a paused channel by clearing the channel's pause state.
resumeSDL :: EnvSDL -> SDLSound I.Paused -> IO (SDLSound I.Playing)
resumeSDL env (PausedChannel channel finished) = do
  modifyMVar_ env.chanState $ \m ->
    let upd cs = cs { chPaused = False }
    in pure (Map.adjust upd channel m)
  -- Apply final paused = basePaused || groupPaused
  applyPauseState env channel
  return (PlayingChannel channel finished)

-- | Stop a channel and remove it from all tracking maps.
stopChannelAndCleanup :: EnvSDL -> Mix.Channel -> IO ()
stopChannelAndCleanup env channel = do
  Mix.halt channel
  -- Set finished if present and remove from finish map
  modifyMVar_ env.finishMap $ \m -> case Map.lookup channel m of
    Just done -> do _ <- tryPutMVar done (); pure (Map.delete channel m)
    Nothing   -> pure m
  -- Remove channel state entry
  modifyMVar_ env.chanState    $ \m -> pure (Map.delete channel m)
  modifyMVar_ env.chanGroupMap $ \cgm -> pure (Map.delete channel cgm)
  -- Prune the channel from all groups
  modifyMVar_ env.groupMap $ \gm ->
    let prune gr = gr { gMembers = Set.delete channel (gMembers gr) }
    in pure (Map.map prune gm)

-- | Stop a playing/paused channel and clean up state.
stopSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO (SDLSound I.Stopped)
stopSDL env stoppable = do
  let channel = getAliveChannel stoppable
  stopChannelAndCleanup env channel
  pure StoppedChannel

-- | Check whether a playing channel has finished.
hasFinishedSDL :: SDLSound I.Playing -> IO Bool
hasFinishedSDL (PlayingChannel _ finished) = do
  fin <- isEmptyMVar finished
  pure $ not fin

-- | Block until the playing channel has finished.
awaitFinishedSDL :: SDLSound I.Playing -> IO ()
awaitFinishedSDL (PlayingChannel _ finished) =
  readMVar finished

----------------------------------------------------------------
-- Volume / Placement (and helpers)
----------------------------------------------------------------
-- | Set per-channel base volume and re-apply effective volume/pan.
setVolumeSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> I.Volume -> IO ()
setVolumeSDL env adjustable vol = do
  let channel = getAliveChannel adjustable 
  updated <- modifyMVar env.chanState $ \m ->
    case Map.lookup channel m of
      Just cs ->
        let cs' = cs { chVolume = vol }
        in pure (Map.insert channel cs' m, True)
      Nothing ->
        pure (m, False)
  -- Re-apply effective settings (volume and pan) only if channel still tracked
  when updated $ applyEffectiveForChannel env channel

-- | Get per-channel base volume (not the SDL-effective value).
getVolumeSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO I.Volume
getVolumeSDL env adjustable = do
  let ch = getAliveChannel adjustable
  m <- readMVar env.chanState
  pure $ maybe I.defaultVolume chVolume (Map.lookup ch m)

-- | Set per-channel base Placement and re-apply effective Placement.
setPlacementSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> I.Placement -> IO ()
setPlacementSDL env alive pan = do
  let channel = getAliveChannel alive
  updated <- modifyMVar env.chanState $ \m ->
    case Map.lookup channel m of
      Just cs ->
        let cs' = cs { chPlacement = pan }
        in pure (Map.insert channel cs' m, True)
      Nothing -> pure (m, False)
  when updated $ applyEffectiveForChannel env channel

-- | Get per-channel base Placement (not the SDL-effective values).
getPlacementSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO I.Placement
getPlacementSDL env alive = do
  let ch = getAliveChannel alive
  m <- readMVar env.chanState
  pure $ maybe (I.mkPlacement 0) chPlacement (Map.lookup ch m)

-- | Multiply two logical volumes.
mulVolume :: I.Volume -> I.Volume -> I.Volume
mulVolume a b = I.mkVolume (I.unVolume a * I.unVolume b)

-- | Stereo balancing: convert the balance float into left and right gains.
panGainsStereo :: Float -> (Float, Float)
panGainsStereo x =
  let l = 1 - max 0 x     
      r = 1 + min 0 x   
  in (l, r)                

-- | Mono pan law: convert the pan float into left and right gains.
panGainsMono :: Float -> (Float, Float)
panGainsMono x =
  let gL = sqrt ((1 - x) / 2)  -- equal-power
      gR = sqrt ((1 + x) / 2)
  in (gL, gR)

-- | Convert logical volume (0..1) to SDL_mixer units (0..128).
toSDLVolume :: I.Volume -> Int
toSDLVolume vol = round (volume * 128)
  where
    volume = I.unVolume vol


----------------------------------------------------------------
-- Groups (SDL)
----------------------------------------------------------------

-- | Create a new logical group with default settings.
makeGroupSDL :: EnvSDL -> IO (I.Group SDLSound)
makeGroupSDL env = do
  gid <- modifyMVar env.groupCounter $ \n ->
    let next = n + 1
    in pure (next, n)
  let group = GroupSDL
        { gPaused = False
        , gMembers = Set.empty
        , gVolume = I.defaultVolume
        , gPlacement = I.defaultPlacement
        }
  modifyMVar_ env.groupMap $ \gm -> pure (Map.insert gid group gm)
  pure (I.GroupId gid)

-- | Add a channel to a group, enforcing exclusive membership (removing it from others).
addToGroupSDL :: forall alive. I.Alive alive => EnvSDL -> I.Group SDLSound -> SDLSound alive -> IO ()
addToGroupSDL env (I.GroupId gid) s = do
  let ch = getAliveChannel s
  -- Enforce exclusive membership: remove from all groups, then add to target
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just _gr -> do
      -- remove channel from every group
      let gmCleared = Map.map (\gr -> gr { gMembers = Set.delete ch (gMembers gr) }) gm
      -- add channel to the target group
      let gmUpdated = Map.adjust (\gr -> gr { gMembers = Set.insert ch (gMembers gr) }) gid gmCleared
      pure gmUpdated
  -- update reverse index
  modifyMVar_ env.chanGroupMap $ \cgm -> pure (Map.insert ch gid cgm)
  -- Apply group bus settings to the channel and correct pause state
  applyEffectiveForChannel env ch
  applyPauseState env ch

-- | Remove a channel from a specific group and clear its reverse index entry.
removeFromGroupSDL :: forall alive. I.Alive alive => EnvSDL -> I.Group SDLSound -> SDLSound alive -> IO ()
removeFromGroupSDL env (I.GroupId gid) s = do
  let ch = getAliveChannel s
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr { gMembers = Set.delete ch (gMembers gr) }
      pure (Map.insert gid gr' gm)
  modifyMVar_ env.chanGroupMap $ \cgm -> pure (Map.delete ch cgm)
  applyEffectiveForChannel env ch
  applyPauseState env ch

-- | Mark group as paused and apply pause to current members.
pauseGroupSDL :: EnvSDL -> I.Group SDLSound -> IO ()
pauseGroupSDL env (I.GroupId gid) = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr{ gPaused = True }
      pure (Map.insert gid gr' gm)
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyPauseState env) (Set.toList (gMembers gr))

-- | Mark group as resumed; also clear base pause for members and apply resume.
resumeGroupSDL :: EnvSDL -> I.Group SDLSound -> IO ()
resumeGroupSDL env (I.GroupId gid) = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr{ gPaused = False }
      pure (Map.insert gid gr' gm)
  -- clear base pause for all members and resume them
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> do
      let members = Set.toList (gMembers gr)
      modifyMVar_ env.chanState $ \m ->
        let m' = foldr (Map.adjust (\cs -> cs { chPaused = False })) m members
        in pure m'
      mapM_ (applyPauseState env) members

-- | Stop all current member channels and clear the group's membership set.
stopGroupSDL :: EnvSDL -> I.Group SDLSound -> IO ()
stopGroupSDL env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> do
      let members = Set.toList (gMembers gr)
      mapM_ (stopChannelAndCleanup env) members

-- | Set group volume and re-apply effective settings to members.
setGroupVolumeSDL :: EnvSDL -> I.Group SDLSound -> I.Volume -> IO ()
setGroupVolumeSDL env (I.GroupId gid) vol = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> pure (Map.insert gid gr{ gVolume = vol } gm)
  -- Update volume for current members
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyEffectiveForChannel env) (Set.toList (gMembers gr))

-- | Set group Placement and re-apply effective settings to members.
setGroupPlacementSDL :: EnvSDL -> I.Group SDLSound -> I.Placement -> IO ()
setGroupPlacementSDL env (I.GroupId gid) pan = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup gid gm of
    Nothing -> pure gm
    Just gr -> pure (Map.insert gid gr{ gPlacement = pan } gm)
  -- Update Placement for current members
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyEffectiveForChannel env) (Set.toList (gMembers gr))

-- | Get the group's logical volume (defaults to 1.0 if missing).
getGroupVolumeSDL :: EnvSDL -> I.Group SDLSound -> IO I.Volume
getGroupVolumeSDL env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure I.defaultVolume
    Just gr -> pure (gVolume gr)

-- | Get the group's logical Placement (defaults to center if missing).
getGroupPlacementSDL :: EnvSDL -> I.Group SDLSound -> IO I.Placement
getGroupPlacementSDL env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure I.defaultPlacement
    Just gr -> pure (gPlacement gr)

-- | Check if a group is marked paused.
isGroupPausedSDL :: EnvSDL -> I.Group SDLSound -> IO Bool
isGroupPausedSDL env (I.GroupId gid) = do
  gm <- readMVar env.groupMap
  case Map.lookup gid gm of
    Nothing -> pure False
    Just gr -> pure (gPaused gr)

----------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------

-- | Compute and apply effective volume and stereo gains for a channel based on
-- its base state and group settings.
applyEffectiveForChannel :: EnvSDL -> Mix.Channel -> IO ()
applyEffectiveForChannel env ch = do
  -- volume
  baseV <- do
    stateMap <- readMVar env.chanState
    pure $ maybe I.defaultVolume chVolume (Map.lookup ch stateMap)
  gVol  <- getGroupVolumeForChannel env ch
  Mix.setVolume (toSDLVolume (mulVolume baseV gVol)) ch
  -- Placement: multiply L/R gains from base and group
  baseP <- do
    stateMap <- readMVar env.chanState
    pure $ maybe I.defaultPlacement chPlacement (Map.lookup ch stateMap)
  gPan  <- getGroupPlacementForChannel env ch
  sType <- do
    stateMap <- readMVar env.chanState
    pure $ maybe I.Stereo chType (Map.lookup ch stateMap)
  let (lBase, rBase) = case sType of
                         I.Mono   -> panGainsMono   (I.unPlacement baseP)
                         I.Stereo -> panGainsStereo (I.unPlacement baseP)
      (lGrp,  rGrp)  = case sType of
                         I.Mono   -> panGainsMono   (I.unPlacement gPan)
                         I.Stereo -> panGainsStereo (I.unPlacement gPan)
      lGain = I.mkVolume (lBase * lGrp)
      rGain = I.mkVolume (rBase * rGrp)
      lEff  = toSDLVolume $ mulVolume (mulVolume baseV gVol) lGain
      rEff  = toSDLVolume $ mulVolume (mulVolume baseV gVol) rGain
  void $ Mix.effectPan ch lEff rEff

-- | Apply pause state for a channel considering base pause and group pause.
applyPauseState :: EnvSDL -> Mix.Channel -> IO ()
applyPauseState env ch = do
  basePaused <- do
    m <- readMVar env.chanState
    pure $ maybe False chPaused (Map.lookup ch m)
  grpPaused  <- getGroupPausedForChannel env ch
  if basePaused || grpPaused then Mix.pause ch else Mix.resume ch

-- | Lookup the 'GroupSDL' a channel belongs to via the reverse index.
getGroupForChannel :: EnvSDL -> Mix.Channel -> IO (Maybe GroupSDL)
getGroupForChannel env ch = do
  mGid <- Map.lookup ch <$> readMVar env.chanGroupMap
  case mGid of
    Nothing   -> pure Nothing
    Just gid -> do
      gm <- readMVar env.groupMap
      pure (Map.lookup gid gm)

-- | Get the group's volume for a given channel (defaults to 1.0 when ungrouped).
getGroupVolumeForChannel :: EnvSDL -> Mix.Channel -> IO I.Volume
getGroupVolumeForChannel env ch = do
  mGr <- getGroupForChannel env ch
  pure $ maybe I.defaultVolume gVolume mGr

-- | Get the group's Placement for a given channel (defaults to center when ungrouped).
getGroupPlacementForChannel :: EnvSDL -> Mix.Channel -> IO I.Placement
getGroupPlacementForChannel env ch = do
  mGr <- getGroupForChannel env ch
  pure $ maybe I.defaultPlacement gPlacement mGr

-- | Get the group's paused state for a given channel (defaults to False when ungrouped).
getGroupPausedForChannel :: EnvSDL -> Mix.Channel -> IO Bool
getGroupPausedForChannel env ch = do
  mGr <- getGroupForChannel env ch
  pure $ maybe False gPaused mGr

-- | Extract the underlying 'Mix.Channel' from a playing/paused handle.
getAliveChannel :: I.Alive alive => SDLSound alive -> Mix.Channel
getAliveChannel s = case s of
  PlayingChannel c _ -> c
  PausedChannel  c _ -> c