module MoreMusic exposing (
    pMap
  , mMap
  , mFold
  , line
  , chord
  , offset
  , times
  , invert
  , shiftPitches
  , scaleDurations
  , changeInstrument
  , retro
  )

{- maps and folds etc for music

@docs 
    pMap
  , mMap
  , mFold
  , line
  , chord
  , offset
  , times
  , invert
  , shiftPitches
  , scaleDurations
  , changeInstrument
  , retro


-}

import Maybe exposing (withDefault)
import Ratio exposing (Rational, over, fromInt, toFloat, add, divide, negate)
import Lazy exposing (lazy, force, Lazy)
import Prelude exposing (..)
import Music exposing (..)

{-| functor behaviour for primitives -}
pMap : (a -> b) -> Primitive a -> Primitive b
pMap f p =
  case p of
    Note d x ->
      Note d (f x)
    Rest d ->
      Rest d

{-| functor behaviour for music -}
mMap : (a -> b) -> Music a -> Music b
mMap f m =
  case m of
    Prim p ->
      Prim (pMap f p)
    Seq m1 m2 ->
      Seq (mMap f m1) (mMap f m2)
    Par m1 m2 -> 
      Par (mMap f m1) (mMap f m2)
    Modify c m ->
      Modify c (mMap f m)


{-| a fold over music -}
mFold : (Primitive a -> b) -> (b->b->b) -> (b->b->b) -> (Control -> b -> b) -> Music a -> b
mFold f seq par g m =
  let 
    rec = mFold f seq par g
  in case m of
    Prim p ->
      f p
    Seq m1 m2 ->
      seq (rec m1) (rec m2)
    Par m1 m2 -> 
      par (rec m1) (rec m2)
    Modify c m ->
      g c (rec m)


{-| a (horizontal) line of music -}
line : List (Music a) -> Music a
line = List.foldr Seq (iRest 0)

{-| a (vertical) chord of music -}
chord : List (Music a) -> Music a
chord = List.foldr Par (iRest 0)

{-| delay a piece of music by the supplied duration -}
offset : Dur -> Music a -> Music a
offset d m  = Seq (rest d) m

{-| repeat a piece of music the requested number of times -}
times : Int -> Music a -> Music a
times n m =
   case n of 
     0 -> 
       iRest 0
     _ -> 
       List.repeat n m
         |> line

{- Rational type constructor not currently supported by imeckler/Ratio
lineToList : Music a -> List (Music a)
lineToList m =
  case m of
    (Prim (Rest (Rational 0 _))) -> 
      []
    Seq n ns -> 
      n :: (lineToList ns)
    _ -> 
      []
-}

invertAt : Pitch -> Music Pitch -> Music Pitch
invertAt pRef = 
  mMap (\p -> pitch (2 * absPitch pRef - absPitch p))

invertAt1 : Pitch -> Music (Pitch, a) -> Music (Pitch, a)
invertAt1 pRef = 
  mMap (\(p,x) -> (pitch (2 * absPitch pRef - absPitch p),x))

invert : Music Pitch -> Music Pitch
invert m = 
  let 
    pRef = mFold pFun (++) (++) (flip const) m
    pFun n =
      case n of
        (Note d p) ->
          [p]
        _ ->
          []
  in  
    let
      mhead = List.head pRef
    in
      case mhead of
        Just pref ->
          invertAt pref m
        _ ->
          m -- no pitches in the structure!


retro : Music a -> Music a
retro m =
  case m of
    Prim _ -> 
      m
    Modify c m ->
      Modify c (retro m)
    Seq m1 m2 ->
      Seq (retro m2) (retro m1)
    Par m1 m2 ->  
      let  d1 = dur m1
           d2 = dur m2
      in 
        if (Ratio.toFloat d1 > Ratio.toFloat d2) then
          Par (retro m1) (Seq (rest (subtractDur d1 d2)) (retro m2))
        else 
          Par (Seq (rest (subtractDur d2 d1)) (retro m1)) (retro m2)



dur : Music a -> Dur
dur m =
  case m of
    Prim (Note d _) ->
      d
    Prim (Rest d) ->
      d
    Seq m1 m2 ->
      add (dur m1) (dur m2)
    Par m1 m2 ->  
      maxDur (dur m1) (dur m2)
    Modify (Tempo r) m  ->
      divide (dur m)  r
    Modify _ m -> 
      dur m

{- max is missing from imeckler/Ratio -}
maxDur : Dur -> Dur -> Dur
maxDur a b =
  if (Ratio.toFloat b) > (Ratio.toFloat a) then
    b
  else
    a

{- so is subtract -}
subtractDur : Dur -> Dur -> Dur
subtractDur a b =
  add a (Ratio.negate b)


{- Sometimes we may wish to alter the internal structure of a Music value
   rather than wrapping it with Modify. The following functions allow this.
-}

shiftPitches : AbsPitch -> Music Pitch -> Music Pitch
shiftPitches k = 
  mMap (trans k)

scaleDurations : Rational -> Music a -> Music a
scaleDurations r m =
  case m of
    Prim (Note d p)  -> 
      note (divide d r) p
    Prim (Rest d) -> 
      rest (divide d r)
    Seq m1 m2 ->
      Seq (scaleDurations r m1) (scaleDurations r m2)
    Par m1 m2 ->
      Par (scaleDurations r m1) (scaleDurations r m2)
    Modify c m ->
      Modify c (scaleDurations r m) 

changeInstrument : InstrumentName -> Music a -> Music a
changeInstrument i m = 
   removeInstruments m
     |> Modify (Instrument i) 

removeInstruments : Music a -> Music a
removeInstruments m =
  case m of
    Modify (Instrument i) m ->
      removeInstruments m
    Modify c m -> 
      Modify c (removeInstruments m)
    Seq m1 m2 ->
      Seq (removeInstruments m1) (removeInstruments m2)
    Par m1 m2 ->
      Par (removeInstruments m1) (removeInstruments m2)
    _ ->
      m









