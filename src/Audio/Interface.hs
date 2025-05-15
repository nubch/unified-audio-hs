module Audio.Interface
  ( AudioBackend(..)
  ) where

import qualified SDL.Mixer as Mix
import Foreign

-- | A loaded sound effect (Mix.Chunk)

-- | A handle to a currently playing sound (channel number)
newtype PlayingHandle = SDLPlaying Mix.Channel

-- | Interface to an audio backend
data AudioBackend sys sound playing = AudioBackend
  { initAudio   :: IO sys
  , loadSound   :: sys -> FilePath -> IO sound
  , playSound   :: sys -> sound -> IO playing
  , stopSound   :: sys -> playing -> IO ()
  }
