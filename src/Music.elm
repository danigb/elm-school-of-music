module Music exposing ( PitchClass(..), Primitive(..), Music(..), Control(..),
  note, rest, tempo, absPitch, pitch )

{-|
My attempt to port HSoM to elm

2.1 Preliminaries

Octave, PitchClass, Pitch, Dur

# 2.2 Notes, Music, and Polymorphism

@docs PitchClass, Primitive, Music, Control

# 2.3 Convenient Auxiliary Functions

@docs note, rest, tempo

# 2.4

@docs absPitch, pitch

-}


{-- 2.1 Preliminaries --}

type alias Octave = Int

{-| PitchClass -}
type PitchClass =
      Cff| Cf  | C | Dff | Cs | Df | Css | D | Eff | Ds
   |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
   |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
   |  Bf | Ass | B | Bs | Bss

type alias Pitch = (PitchClass, Octave)
type alias Dur = Float

{-- 2.2 Notes, Music, and Polymorphism --}

{-| Primitive -}
type Primitive a =
  Note Dur a
  | Rest Dur

{-| Music -}
type Music a =
  Prim (Primitive a)
  | Seq (Music a) (Music a)
  | Par (Music a) (Music a)
  | Modify Control (Music a)

{-| Control -}
type Control = Tempo Float

{-- 2.3 Convenient Auxiliary Functions --}

{-| note -}
note : Dur -> a -> Music a
note d p =
  Prim (Note d p)

{-| rest -}
rest : Dur -> Music a
rest d =
  Prim (Rest d)

{-| tempo -}
tempo : Dur -> Music a -> Music a
tempo r m =
  Modify (Tempo r) m

{- 2.4 Absolute Pitches -}

{- Treating pitches simply as integers is useful in many settings, so Euterpea
uses a type synonym to define the concept of an “absolute pitch: -}

{- NOTE: I would prefer to use a Float -}

type alias AbsPitch = Int

{-| The absolute pitch of a Pitch -}
absPitch : Pitch -> AbsPitch
absPitch (pc, o) =
  12 * o + pcToInt pc


chromatic = [C, Df, D, Ef, E, F, Gf, G, Af, A, Bf, B]
{- NOTE: this is a hack. I don't want to return a Maybe... since ap % 2,
  I know positively it can't return Nothing -}
{-| Get the pitch class of an absolute pitch -}
pitchClass : AbsPitch -> PitchClass
pitchClass ap =
  case List.head (List.drop (ap % 12) chromatic) of
    Nothing -> C
    Just pc -> pc

{-| Get the octave of an absolute pitch -}
octave : AbsPitch -> Int
octave ap =
  if ap < 0 then (ap // 12) - 1 else ap // 12

{-| Converting an absolute pitch to a pitch is a bit more tricky, because of
enharmonic equivalences. For example, the absolute pitch 15 might correspond
to either (Ds, 1) or (Ef , 1). Euterpea takes the approach of always
returning a sharp in such ambiguous cases -}
pitch : AbsPitch -> Pitch
pitch ap =
  (pitchClass ap, octave ap)


{- NOTE: Those are at the end because they are too large. Any compact syntax? -}

pcToInt pc  = case pc of
  Cff  -> -2
  Cf  -> -1
  C  -> 0
  Cs  -> 1
  Css  -> 2
  Dff  -> 0
  Df  -> 1
  D  -> 2
  Ds  -> 3
  Dss  -> 4
  Eff  -> 2
  Ef  -> 3
  E  -> 4
  Es  -> 5
  Ess  -> 6
  Fff  -> 3
  Ff  -> 4
  F  -> 5
  Fs  -> 6
  Fss  -> 7
  Gff  -> 5
  Gf  -> 6
  G  -> 7
  Gs  -> 8
  Gss  -> 9
  Aff  -> 7
  Af  -> 8
  A  -> 9
  As  -> 10
  Ass  -> 11
  Bff  -> 9
  Bf  -> 10
  B  -> 11
  Bs  -> 12
  Bss  -> 13
