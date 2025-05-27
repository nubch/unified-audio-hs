module Interface
  ( AudioBackend(..)
  ) where

data AudioBackend sys sound playing = AudioBackend
  { initAudio   :: IO sys
  , loadSound   :: sys -> FilePath -> IO sound
  , playSound   :: sys -> sound -> IO playing
  , stopSound   :: sys -> playing -> IO ()
  }
