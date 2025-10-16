# unified-audio-hs

Effect-based, unified audio abstractions with interchangeable backends (FMOD Core and SDL Mixer).

## FMOD Core
This project targets FMOD Core API 2.02.30.

Get “FMOD Engine” (Core API) 2.02.30 for Linux from the FMOD website and extract it.

If you are using another version, adjust the version in the Fmod.Safe module with the one from FMOD (can be found in the `/api/core/inc/fmod.xs` file)

## Cabal Setup (FMOD paths)
The Cabal file contains placeholders that must be replaced with your local, **absolute** paths:

Edit `unified-audio-hs.cabal` and replace:

- `**PATH_TO_INC**` with the absolute path to the `inc` folder of FMOD (its usually under `/api/core/inc`)

- `**PATH_TO_x86_64**` with the absolute path to the x86_64 folder of FMOD (its usually under `/api/core/lib/x86_64`)


## Run The Tests
Run `cabal run`.
The tests uses both backends (first SDL and then FMOD). They also play audio files in `sounds/`, so ensure a working audio device.
