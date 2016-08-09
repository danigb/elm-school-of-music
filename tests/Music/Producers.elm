module Music.Producers exposing (..)

import Array exposing (..)
import Maybe exposing (withDefault)
import Random exposing (Generator)
import Check.Producer exposing (..)
import Random.Extra exposing (..)
import Music exposing (..)
import MoreMusic exposing (..)
import Ratio exposing (Rational, over)

-- producers for Music

pitchClasses : Array PitchClass
pitchClasses =
   [  Cff, Cf  , C , Dff , Cs , Df , Css , D , Eff , Ds
   ,  Ef , Fff , Dss , E , Ff , Es , F , Gff , Ess , Fs
   ,  Gf , Fss , G , Aff , Gs , Af , Gss , A , Bff , As
   ,  Bf , Ass , B , Bs , Bss ]
     |> Array.fromList

pitchClass : Producer PitchClass
pitchClass =
   let
     f i = Array.get i pitchClasses
             |> withDefault C
   in
     rangeInt 0 (Array.length pitchClasses - 1)
       |> Check.Producer.map f

octave : Producer Octave
octave =
   rangeInt 0 8

interval : Producer Int
interval =
   rangeInt -11 11

apitch : Producer Pitch
apitch = Check.Producer.tuple (pitchClass, octave)

pitchWithInterval : Producer (Pitch, Int)
pitchWithInterval = Check.Producer.tuple (apitch, interval)

pitchWithTwoIntervals : Producer (Pitch, Int, Int)
pitchWithTwoIntervals = Check.Producer.tuple3 (apitch, interval, interval)


-- producers for MoreMusic

{-| duration -}
dur : Producer Dur
dur =
  let
    num = rangeInt 0 64
    den = rangeInt 1 16
    pair = Check.Producer.tuple (num, den)
  in 
    pair 
      |> Check.Producer.map (\(a,b) -> over a b)

{-| pitched note -}
pitchedNote : Producer (Music Pitch)
pitchedNote =
  let
    pair = Check.Producer.tuple (dur, apitch)
  in
    pair
      |> Check.Producer.map (\(a,b) -> Prim (Note a b))

{-| a line of notes as music -}
musicPitchLine : Producer (Music Pitch)
musicPitchLine = 
   list pitchedNote
     |> Check.Producer.map line

{-| a rest -}
arest : Producer (Music Pitch)
arest =
  dur
    |> Check.Producer.map (\d -> Prim (Rest d))

{- a sequential or parallel production of music 
   we need to make this lazy
-}
parseq : (Music Pitch -> Music Pitch -> Music Pitch) -> Producer (Music Pitch)
parseq constructor =
  let
    {- this crashes because of the recursion in music : 
       pair = Check.Producer.tuple (music, music) 
    -}
    pair = Check.Producer.tuple (pitchedNote, pitchedNote)
  in
    pair
      |> Check.Producer.map (\(a,b) -> constructor a b)


{-| music - NOT COMPLETE - Modify ignored at the moment -}
music : Producer (Music Pitch) 
music =
  let
    generator = 
      choices
        [ pitchedNote.generator
        , arest.generator
        , (parseq Seq).generator
        , (parseq Par).generator
        ]
    shrinker pr =
      case pr of
        Prim (Note _ _) ->  pitchedNote.shrinker pr
        Seq _ _ -> (parseq Seq).shrinker pr
        Par _ _ -> (parseq Par).shrinker pr
        -- Prim (Rest _) -> arest.shrinker pr
        _ -> arest.shrinker pr
  in
    Producer generator shrinker 







      
