{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Interface
  ( AudioEffect,
    AudioBackend(..),
    StaticRep(..)
  ) where

import Data.Kind
import Effectful
import Effectful.Dispatch.Static

data AudioEffect (playing :: Type) :: Effect
  
type instance DispatchOf (AudioEffect playing) = Static WithSideEffects

newtype instance StaticRep (AudioEffect playing) = AudioRep (AudioBackend playing)

data AudioBackend playing = AudioBackend
  { 
    playSoundB :: FilePath -> IO playing,
    stopSoundB :: playing -> IO ()
  }