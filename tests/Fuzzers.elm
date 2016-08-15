module Fuzzers exposing (..)

import Array exposing (..)
import Maybe exposing (withDefault)
import Fuzz exposing (..)
import Music exposing (..)
import MoreMusic exposing (..)
import Ratio exposing (Rational, over)

pitchClasses : Array PitchClass
pitchClasses =
   [  Cff, Cf  , C , Dff , Cs , Df , Css , D , Eff , Ds
   ,  Ef , Fff , Dss , E , Ff , Es , F , Gff , Ess , Fs
   ,  Gf , Fss , G , Aff , Gs , Af , Gss , A , Bff , As
   ,  Bf , Ass , B , Bs , Bss ]
     |> Array.fromList

pitchClass : Fuzzer PitchClass
pitchClass =
   let
     f i = Array.get i pitchClasses
             |> withDefault C
   in
     intRange 0 (Array.length pitchClasses - 1)
       |> Fuzz.map f

octave : Fuzzer Octave
octave =
   intRange 0 8

interval : Fuzzer Int
interval =
   intRange -11 11

apitch : Fuzzer Pitch
apitch = Fuzz.tuple (pitchClass, octave)

pitchWithInterval : Fuzzer (Pitch, Int)
pitchWithInterval = Fuzz.tuple (apitch, interval)

pitchWithTwoIntervals : Fuzzer (Pitch, Int, Int)
pitchWithTwoIntervals = Fuzz.tuple3 (apitch, interval, interval)


-- Fuzzers for MoreMusic

{-| duration -}
duration : Fuzzer Dur
duration =
  let
    num = intRange 0 64
    den = intRange 1 16
    pair = Fuzz.tuple (num, den)
  in 
    pair 
      |> Fuzz.map (\(a,b) -> over a b)

{-| pitched note -}
pitchedNote : Fuzzer (Music Pitch)
pitchedNote =
  let
    pair = Fuzz.tuple (duration, apitch)
  in
    pair
      |> Fuzz.map (\(a,b) -> Prim (Note a b))

{-| a line of notes as music -}
musicPitchLine : Fuzzer (Music Pitch)
musicPitchLine = 
   list pitchedNote
     |> Fuzz.map line

{-| a rest -}
arest : Fuzzer (Music Pitch)
arest =
  duration
    |> Fuzz.map (\d -> Prim (Rest d))


{-| music - NOT COMPLETE - Modify ignored at the moment -}
music : Int -> Fuzzer (Music Pitch)
music depth =
  let
    {- stop the recursion at a reasonable tree depth -}
    nextLevel =
      if depth <= 0 then
        pitchedNote
      else
        music (depth - 1)

    seq =
      Fuzz.tuple ( pitchedNote, nextLevel ) |> Fuzz.map (uncurry Seq)

    par =
      Fuzz.tuple ( pitchedNote, nextLevel ) |> Fuzz.map (uncurry Par)
  in
    if depth > 1 then
      Fuzz.frequencyOrCrash
        [ ( 10, pitchedNote )
        , ( 1,  arest )
        , ( 20, seq )
        , ( 3,  par )
        ]
    else
      Fuzz.frequencyOrCrash
         [ ( 10, pitchedNote )
         , ( 1,  arest )
         ]

      
