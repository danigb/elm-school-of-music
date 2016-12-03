module Music1 exposing (
    makeMusic
  , pMusicMaker
  , pvMusicMaker
  , nMusicMaker
  , apMusicMaker)


{-
as close an approach as we can get with Elm at the moment to a typeclass

pseudo typeclass
@docs makeMusic

pseudo typeclass instances
@docs pMusicMaker
    , pvMusicMaker
    , nMusicMaker
    , apMusicMaker

-- these are the original Haskell typeclasses that we're doing our best to emulate

class ToMusic1 a where
     toMusic1 :: Music a -> Music1

 instance ToMusic1 Pitch where
     toMusic1 = mMap (\p -> (p, []))

 instance ToMusic1 (Pitch, Volume) where
     toMusic1  = mMap (\(p, v) -> (p, [Volume v]))

 instance ToMusic1 (Note1) where
     toMusic1 = id

 instance ToMusic1 (AbsPitch) where
     toMusic1 = mMap (\a -> (pitch a, []))

-}

import Prelude exposing (id)
import Music exposing (Music(..), Pitch, AbsPitch, Volume, Note1, Music1, pitch)
import MoreMusic exposing (mMap)

{-| make Music1 from Music Pitch -}
toMusic1P : Music Pitch -> Music1
toMusic1P =
   mMap (\p -> (p, []))

{-| make Music1 from Music (Pitch, Volume) -}
toMusic1PV : Music (Pitch, Volume) -> Music1
toMusic1PV =
   mMap (\(p, v) -> (p, [Volume v]))

{-| make Music1 from Music Note1 (aka Music1) -}
toMusic1N : Music Note1 -> Music1
toMusic1N =
   id

{-| make Music1 from Music AbsPitch -}
toMusic1AP : Music AbsPitch -> Music1
toMusic1AP =
    mMap (\a -> (pitch a, []))

-- pseudo typeclass instances
pMusicMaker  = { toMusic1 = toMusic1P }  -- just Pitch
pvMusicMaker = { toMusic1 = toMusic1PV } -- Pitch and Volume
nMusicMaker  = { toMusic1 = toMusic1N }  -- Already Music1 (i.e. Note1)
apMusicMaker = { toMusic1 = toMusic1AP } -- just Absolute Pitch

{-| makeMusic pseudo typeclass -}
makeMusic musicMaker x = musicMaker.toMusic1 x

{- use like this (I'm afraid)
testMakeMusic : Music Pitch -> Music1
testMakeMusic =
  makeMusic pMusicMaker 
-}
