module Audio.Interface
  ( AudioBackend(..)
  , SoundHandle(..)
  , PlayingHandle(..)
  ) where

import qualified SDL.Mixer as Mix

-- | A loaded sound effect (Mix.Chunk)
newtype SoundHandle = SDLSound Mix.Chunk

-- | A handle to a currently playing sound (channel number)
newtype PlayingHandle = SDLPlaying Mix.Channel

-- | Interface to an audio backend
data AudioBackend = AudioBackend
  { initAudio   :: IO ()
  , loadSound   :: FilePath -> IO SoundHandle
  , playSound   :: SoundHandle -> IO PlayingHandle
  , stopSound   :: PlayingHandle -> IO ()
  }
