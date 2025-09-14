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

type FinishMap = MVar (Map.Map Mix.Channel (MVar ()))

-- Base per-channel state (not including group effects)
type PanMap  = MVar (Map.Map Mix.Channel I.Panning)
type VolMap  = MVar (Map.Map Mix.Channel I.Volume)
type PauseMap = MVar (Map.Map Mix.Channel Bool) -- base paused state
type TypeMap = MVar (Map.Map Mix.Channel I.SoundType)

data GroupSDL = GroupSDL
  { gPaused  :: !Bool
  , gMembers :: !(Set.Set Mix.Channel)
  , gVolume  :: !I.Volume
  , gPanning :: !I.Panning
  }

type GroupMap = MVar (Map.Map String GroupSDL)

data EnvSDL = EnvSDL
  { finishMap   :: FinishMap
  , panMap      :: PanMap
  , volMap      :: VolMap
  , pauseMap    :: PauseMap
  , typeMap     :: TypeMap
  , groupMap    :: GroupMap
  }

data SDLSound :: I.Status -> Type where
  LoadedSound  :: Mix.Chunk -> I.SoundType -> SDLSound I.Loaded
  UnloadedSound :: SDLSound I.Unloaded
  PlayingSound :: Mix.Channel -> MVar () -> I.SoundType -> SDLSound I.Playing
  PausedSound  :: Mix.Channel -> MVar () -> I.SoundType -> SDLSound I.Paused
  StoppedSound :: Mix.Channel -> SDLSound I.Stopped

----------------------------------------------------------------
-- Init / Finalization
----------------------------------------------------------------

initSDLEnv :: IO EnvSDL
initSDLEnv = do
  pMap   <- newMVar Map.empty
  vMap   <- newMVar Map.empty
  paMap  <- newMVar Map.empty
  tMap   <- newMVar Map.empty
  finMap <- newMVar Map.empty
  gMap   <- newMVar Map.empty
  Mix.whenChannelFinished $ \ch -> do
    modifyMVar_ finMap $ \m ->
      case Map.lookup ch m of
        Just done -> do
          _ <- tryPutMVar done ()
          pure (Map.delete ch m)
        Nothing   -> pure m
    modifyMVar_ pMap  $ \pm -> pure (Map.delete ch pm)
    modifyMVar_ vMap  $ \vm -> pure (Map.delete ch vm)
    modifyMVar_ paMap $ \pm -> pure (Map.delete ch pm)
    modifyMVar_ tMap  $ \tm -> pure (Map.delete ch tm)
    -- prune from all groups
    modifyMVar_ gMap $ \gm ->
      let prune gr = gr { gMembers = Set.delete ch (gMembers gr) }
      in pure (Map.map prune gm)
  pure EnvSDL { finishMap = finMap
              , panMap = pMap
              , volMap = vMap
              , pauseMap = paMap
              , typeMap = tMap
              , groupMap = gMap
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
unloadSDL (LoadedSound chunk _) = do
  Mix.free chunk
  pure UnloadedSound

----------------------------------------------------------------
-- Play / Pause / Resume / Stop / Status
----------------------------------------------------------------

playSDL :: EnvSDL -> SDLSound I.Loaded -> I.Times -> IO (SDLSound I.Playing)
playSDL env (LoadedSound loaded sType) times = do
  channel <- Mix.playOn Mix.AllChannels sdlTimes loaded
  done    <- newEmptyMVar
  modifyMVar_ env.finishMap $ \m -> pure $ Map.insert channel done m
  modifyMVar_ env.typeMap  $ \tm -> pure $ Map.insert channel sType tm
  -- Initialize base state
  modifyMVar_ env.panMap   $ \pm -> pure $ Map.insert channel I.defaultPanning pm
  modifyMVar_ env.volMap   $ \vm -> pure $ Map.insert channel I.defaultVolume vm
  modifyMVar_ env.pauseMap $ \pm -> pure $ Map.insert channel False pm

  -- Reset pan and volume if channel gets reused
  let playingChannel = PlayingSound channel done sType
  setPanningSDL env playingChannel I.defaultPanning
  setVolumeSDL env playingChannel I.defaultVolume
  pure playingChannel
  where
    sdlTimes = case times of
      I.Once    -> Mix.Once
      I.Times n -> fromIntegral n
      I.Forever -> Mix.Forever

pauseSDL :: EnvSDL -> SDLSound I.Playing -> IO (SDLSound I.Paused)
pauseSDL env (PlayingSound channel finished sType) = do
  modifyMVar_ env.pauseMap $ \pm -> pure (Map.insert channel True pm)
  -- Apply final paused = basePaused || groupPaused
  applyPauseState env channel
  return (PausedSound channel finished sType)

resumeSDL :: EnvSDL -> SDLSound I.Paused -> IO (SDLSound I.Playing)
resumeSDL env (PausedSound channel finished sType) = do
  modifyMVar_ env.pauseMap $ \pm -> pure (Map.insert channel False pm)
  -- Apply final paused = basePaused || groupPaused
  applyPauseState env channel
  return (PlayingSound channel finished sType)

stopSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO (SDLSound I.Stopped)
stopSDL env stoppable =
  case stoppable of
    (PlayingSound channel finished _) -> stop channel finished
    (PausedSound  channel finished _) -> stop channel finished
  where
    stop channel finished = do
      Mix.halt channel
      modifyMVar_ env.finishMap $ \m  -> pure (Map.delete channel m)
      modifyMVar_ env.panMap    $ \pm -> pure (Map.delete channel pm)
      modifyMVar_ env.volMap    $ \vm -> pure (Map.delete channel vm)
      modifyMVar_ env.pauseMap  $ \pm -> pure (Map.delete channel pm)
      modifyMVar_ env.typeMap   $ \tm -> pure (Map.delete channel tm)
      modifyMVar_ env.groupMap  $ \gm ->
        let prune gr = gr { gMembers = Set.delete channel (gMembers gr) }
        in pure (Map.map prune gm)
      _ <- tryPutMVar finished ()
      return (StoppedSound channel)

hasFinishedSDL :: SDLSound I.Playing -> IO Bool
hasFinishedSDL (PlayingSound _ finished _) = do
  fin <- isEmptyMVar finished
  pure $ not fin

awaitFinishedSDL :: SDLSound I.Playing -> IO ()
awaitFinishedSDL (PlayingSound _ finished _) =
  takeMVar finished

----------------------------------------------------------------
-- Volume / Panning (and helpers)
----------------------------------------------------------------

setVolumeSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> I.Volume -> IO ()
setVolumeSDL env adjustable vol =
  case adjustable of
    (PlayingSound channel _ _) -> setV channel
    (PausedSound  channel _ _) -> setV channel
  where
    setV channel = do
      modifyMVar_ env.volMap $ \vm -> pure (Map.insert channel vol vm)
      -- recompute effective volume = base * group
      gVol <- getGroupVolumeForChannel env channel
      let eff = toSDLVolume (mulVolume vol gVol)
      Mix.setVolume eff channel

getVolumeSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO I.Volume
getVolumeSDL env adjustable =
  case adjustable of
    (PlayingSound channel _ _) -> getV channel
    (PausedSound  channel _ _) -> getV channel
  where
    getV ch = do
      vm <- readMVar env.volMap
      pure (Map.findWithDefault I.defaultVolume ch vm)

setPanningSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> I.Panning -> IO ()
setPanningSDL env adjustable pan =
  case adjustable of
    (PlayingSound channel _ sType) -> setPan channel sType
    (PausedSound  channel _ sType) -> setPan channel sType
  where
    setPan :: Mix.Channel -> I.SoundType -> IO ()
    setPan channel sType = do
      -- store base pan
      modifyMVar_ env.panMap $ \pm -> pure $ Map.insert channel pan pm
      -- recompute effective pan = clamp(base + group)
      gPan <- getGroupPanningForChannel env channel
      let x      = I.unPanning (addPanning pan gPan)
          (l, r) = case sType of
                     I.Mono   -> toSDLPanningMono   x  -- equal-power (mono→stereo)
                     I.Stereo -> toSDLPanningStereo x  -- balance (tilt)
      void $ Mix.effectPan channel l r

getPanningSDL :: forall alive. I.Alive alive => EnvSDL -> SDLSound alive -> IO I.Panning
getPanningSDL env adjustable =
  case adjustable of
    (PlayingSound ch _ _) -> getPan ch
    (PausedSound  ch _ _) -> getPan ch
  where
    getPan ch = do
      pm <- readMVar env.panMap
      pure (Map.findWithDefault (I.mkPanning 0) ch pm)

-- Helpers: volumes and panning math
mulVolume :: I.Volume -> I.Volume -> I.Volume
mulVolume a b = I.mkVolume (I.unVolume a * I.unVolume b)

addPanning :: I.Panning -> I.Panning -> I.Panning
addPanning a b = I.mkPanning (I.unPanning a + I.unPanning b)

toSDLVolume :: I.Volume -> Int
toSDLVolume vol = round (volume * 128)
  where
    volume = I.unVolume vol

-- 0..128 per side (128 = full). effectPan doubles internally to 0..255.
toSDLPanningStereo :: Float -> (Int, Int)
toSDLPanningStereo x0 =
  let x = max (-1) (min 1 x0)
      l = round (128 * (1 - max 0 x))   -- reduce LEFT only when panning right
      r = round (128 * (1 + min 0 x))   -- reduce RIGHT only when panning left
  in (l, r)                             -- center => (128,128)

toSDLPanningMono :: Float -> (Int, Int)
toSDLPanningMono x0 =
  let x  = max (-1) (min 1 x0)
      gL = sqrt ((1 - x) / 2)           -- ≈0.707 at center (−3 dB/side)
      gR = sqrt ((1 + x) / 2)
  in ( round (128 * gL), round (128 * gR) )

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
    , I.mkOrGetGroupA  = mkOrGetGroupSDL env
    , I.addToGroupA    = addToGroupSDL env
    , I.removeFromGroupA = removeFromGroupSDL env
    , I.pauseGroupA    = pauseGroupSDL env
    , I.resumeGroupA   = resumeGroupSDL env
    , I.setGroupVolumeA = setGroupVolumeSDL env
    , I.setGroupPanningA = setGroupPanningSDL env
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

mkGroupSDL :: EnvSDL -> String -> IO (I.Group SDLSound)
mkGroupSDL env name = do
  modifyMVar_ env.groupMap $ \gm ->
    let g = GroupSDL { gPaused = False
                     , gMembers = Set.empty
                     , gVolume = I.defaultVolume
                     , gPanning = I.defaultPanning
                     }
    in pure (Map.insert name g gm)
  pure (I.GroupName name)

mkOrGetGroupSDL :: EnvSDL -> String -> IO (I.Group SDLSound)
mkOrGetGroupSDL env name = do
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Just _ -> pure (I.GroupName name)
    Nothing  -> mkGroupSDL env name

addToGroupSDL :: forall alive. I.Alive alive => EnvSDL -> I.Group SDLSound -> SDLSound alive -> IO ()
addToGroupSDL env (I.GroupName name) s = do
  ch <- case s of
    PlayingSound c _ _ -> pure c
    PausedSound  c _ _ -> pure c
  -- Enforce exclusive membership: remove from all groups, then add to target
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup name gm of
    Nothing -> pure gm
    Just _gr -> do
      -- remove channel from every group's membership set
      let gmCleared = Map.map (\gr -> gr { gMembers = Set.delete ch (gMembers gr) }) gm
      -- add channel to the target group's membership set
      let gmUpdated = Map.adjust (\gr -> gr { gMembers = Set.insert ch (gMembers gr) }) name gmCleared
      pure gmUpdated
  -- Apply group bus settings to the channel and correct pause state
  applyEffectiveForChannel env ch
  applyPauseState env ch

removeFromGroupSDL :: forall alive. I.Alive alive => EnvSDL -> I.Group SDLSound -> SDLSound alive -> IO ()
removeFromGroupSDL env (I.GroupName name) s = do
  ch <- case s of
    PlayingSound c _ _ -> pure c
    PausedSound  c _ _ -> pure c
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup name gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr { gMembers = Set.delete ch (gMembers gr) }
      pure (Map.insert name gr' gm)
  -- Re-apply effective settings (likely reverting to base settings)
  applyEffectiveForChannel env ch
  applyPauseState env ch

pauseGroupSDL :: EnvSDL -> I.Group SDLSound -> IO ()
pauseGroupSDL env (I.GroupName name) = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup name gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr{ gPaused = True }
      pure (Map.insert name gr' gm)
  -- Apply pause to members (final pause = basePaused || groupPaused)
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyPauseState env) (Set.toList (gMembers gr))

resumeGroupSDL :: EnvSDL -> I.Group SDLSound -> IO ()
resumeGroupSDL env (I.GroupName name) = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup name gm of
    Nothing -> pure gm
    Just gr -> do
      let gr' = gr{ gPaused = False }
      pure (Map.insert name gr' gm)
  -- Resume only members that are not base-paused
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyPauseState env) (Set.toList (gMembers gr))

-- Note: SDL Mixer has no group volume; we approximate by setting member volumes directly.
setGroupVolumeSDL :: EnvSDL -> I.Group SDLSound -> I.Volume -> IO ()
setGroupVolumeSDL env (I.GroupName name) vol = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup name gm of
    Nothing -> pure gm
    Just gr -> pure (Map.insert name gr{ gVolume = vol } gm)
  -- Apply to current members: effective = base * group
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyEffectiveForChannel env) (Set.toList (gMembers gr))

-- Approximate group panning by applying stereo balance to members.
setGroupPanningSDL :: EnvSDL -> I.Group SDLSound -> I.Panning -> IO ()
setGroupPanningSDL env (I.GroupName name) pan = do
  modifyMVar_ env.groupMap $ \gm -> case Map.lookup name gm of
    Nothing -> pure gm
    Just gr -> pure (Map.insert name gr{ gPanning = pan } gm)
  -- Apply to current members: effective = clamp(base + group)
  gm <- readMVar env.groupMap
  case Map.lookup name gm of
    Nothing -> pure ()
    Just gr -> mapM_ (applyEffectiveForChannel env) (Set.toList (gMembers gr))

----------------------------------------------------------------
-- Internal helpers: effective state application
----------------------------------------------------------------

-- Apply effective volume and panning to a channel based on its base state and group
applyEffectiveForChannel :: EnvSDL -> Mix.Channel -> IO ()
applyEffectiveForChannel env ch = do
  -- volume
  baseV <- Map.findWithDefault I.defaultVolume ch <$> readMVar env.volMap
  gVol  <- getGroupVolumeForChannel env ch
  Mix.setVolume (toSDLVolume (mulVolume baseV gVol)) ch
  -- panning
  baseP <- Map.findWithDefault I.defaultPanning ch <$> readMVar env.panMap
  gPan  <- getGroupPanningForChannel env ch
  sType <- Map.findWithDefault I.Stereo ch <$> readMVar env.typeMap
  let panX = I.unPanning (addPanning baseP gPan)
      (l, r) = case sType of
                 I.Mono   -> toSDLPanningMono panX
                 I.Stereo -> toSDLPanningStereo panX
  void $ Mix.effectPan ch l r

-- Apply pause state considering base paused and group paused
applyPauseState :: EnvSDL -> Mix.Channel -> IO ()
applyPauseState env ch = do
  basePaused <- Map.findWithDefault False ch <$> readMVar env.pauseMap
  grpPaused  <- getGroupPausedForChannel env ch
  if basePaused || grpPaused then Mix.pause ch else Mix.resume ch

-- Lookup helpers for group-based properties
getGroupForChannel :: EnvSDL -> Mix.Channel -> IO (Maybe GroupSDL)
getGroupForChannel env ch = do
  gm <- readMVar env.groupMap
  pure $
    let matches gr = Set.member ch (gMembers gr)
    in case [ gr | gr <- Map.elems gm, matches gr ] of
         (gr:_) -> Just gr
         []     -> Nothing

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
