module Music exposing (
  PitchClass(..), Primitive(..), Music(..), Control(..), PlayerName, Octave, Pitch, AbsPitch, 
  Dur, InstrumentName (..), Volume, Mode (..), NoteAttribute (..), PhraseAttribute (..), Music1, Note1,
  Dynamic (..),StdLoudness (..), Articulation (..), Tempo (..),
  note, iNote, rest, iRest, tempo, qn,
  absPitch, pitch, trans,
  wts )

{-|

My attempt to port HSoM to elm

CHAPTER 2


# 2.2 Types

@docs PitchClass
    , Primitive
    , Music
    , Control
    , PlayerName
    , Octave
    , Pitch
    , AbsPitch
    , Dur
    , InstrumentName
    , Volume
    , Mode
    , NoteAttribute
    , PhraseAttribute
    , Music1
    , Note1
    , Dynamic
    , StdLoudness
    , Articulation
    , Tempo

# 2.3 Convenient Auxiliary Functions

@docs note, iNote, rest, iRest, tempo, qn

# 2.4 Absolute Pitches

@docs absPitch, pitch, trans

CHAPTER 3: Polymorphic and Higher-Order Functions

@docs wts

-}

import Ratio exposing (Rational, over, fromInt, divide)


{-- 2.1 Preliminaries --}
{-| Octave -}
type alias Octave = Int

{-| PitchClass -}
type PitchClass =
      Cff| Cf  | C | Dff | Cs | Df | Css | D | Eff | Ds
   |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
   |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
   |  Bf | Ass | B | Bs | Bss

{-| Pitch -}
type alias Pitch = (PitchClass, Octave)

{- NOTE: I would prefer to use a Float -}

{-| Absolute Pitch -}
type alias AbsPitch = Int

{-| Note duration -}
type alias Dur = Rational

{-| quarter note -}
qn : Dur
qn = over 1 4

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
type Control = 
    Tempo Rational                  -- scale the tempo
  | Transpose AbsPitch              -- transposition
  | Instrument InstrumentName       -- instrument label
  | Phrase (List PhraseAttribute)   -- phrase attributes
  | KeySig PitchClass Mode          -- key signature and mode 
  | Custom String                   --  for user-specified controls

{-| The name of the 'player' of the music -}
type alias PlayerName  = String

{-| Mode -}
type Mode = 
    Major 
  | Minor

{- note: elm doesn't allow repetition of type constructors across separate types
   because of ambiguity and so HSoM's Custom is renamed CustomInstrument.
-}
{-| Instrument Name -}
type InstrumentName =
     AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
  |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
  |  Harpsichord            | Clavinet               | Celesta 
  |  Glockenspiel           | MusicBox               | Vibraphone  
  |  Marimba                | Xylophone              | TubularBells
  |  Dulcimer               | HammondOrgan           | PercussiveOrgan 
  |  RockOrgan              | ChurchOrgan            | ReedOrgan
  |  Accordion              | Harmonica              | TangoAccordion
  |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
  |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
  |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
  |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
  |  SlapBass1              | SlapBass2              | SynthBass1   
  |  SynthBass2             | Violin                 | Viola  
  |  Cello                  | Contrabass             | TremoloStrings
  |  PizzicatoStrings       | OrchestralHarp         | Timpani
  |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
  |  SynthStrings2          | ChoirAahs              | VoiceOohs
  |  SynthVoice             | OrchestraHit           | Trumpet
  |  Trombone               | Tuba                   | MutedTrumpet
  |  FrenchHorn             | BrassSection           | SynthBrass1
  |  SynthBrass2            | SopranoSax             | AltoSax 
  |  TenorSax               | BaritoneSax            | Oboe  
  |  Bassoon                | EnglishHorn            | Clarinet
  |  Piccolo                | Flute                  | Recorder
  |  PanFlute               | BlownBottle            | Shakuhachi
  |  Whistle                | Ocarina                | Lead1Square
  |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
  |  Lead5Charang           | Lead6Voice             | Lead7Fifths
  |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
  |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
  |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
  |  FX1Train               | FX2Soundtrack          | FX3Crystal
  |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
  |  FX7Echoes              | FX8SciFi               | Sitar
  |  Banjo                  | Shamisen               | Koto
  |  Kalimba                | Bagpipe                | Fiddle 
  |  Shanai                 | TinkleBell             | Agogo  
  |  SteelDrums             | Woodblock              | TaikoDrum
  |  MelodicDrum            | SynthDrum              | ReverseCymbal
  |  GuitarFretNoise        | BreathNoise            | Seashore
  |  BirdTweet              | TelephoneRing          | Helicopter
  |  Applause               | Gunshot                | Percussion
  |  CustomInstrument String

{-| Phrase Attribute - dynamics, articulation etc. -}
type PhraseAttribute = 
    Dyn Dynamic
  | Tmp Tempo
  | Art Articulation
  | Orn Ornament

{-|-}
type Dynamic =  
    Accent Rational | Crescendo Rational | Diminuendo Rational
  | StdLoudness StdLoudness | Loudness Rational

{-|-}
type StdLoudness = 
  PPP | PP | P | MP | SF | MF | NF | FF | FFF

{-|-}
type Tempo =
   Ritardando Rational 
 | Accelerando Rational

{-|-}
type Articulation =  
    Staccato Rational | Legato Rational | Slurred Rational
  | Tenuto | Marcato | Pedal | Fermata | FermataDown | Breath
  | DownBow | UpBow | Harmonic | Pizzicato | LeftPizz
  | BartokPizz | Swell | Wedge | Thumb | Stopped

type Ornament =
    Trill | Mordent | InvMordent | DoubleMordent
  | Turn | TrilledTurn | ShortTrill
  | Arpeggio | ArpeggioUp | ArpeggioDown
  | Instruction String | Head NoteHead
  | DiatonicTrans Int

{-|-}
type NoteHead =  
    DiamondHead | SquareHead | XHead | TriangleHead
  | TremoloHead | SlashHead | ArtHarmonic | NoHead

{-|-}
type alias Volume = Int

addVolume : Volume -> Music Pitch -> Music (Pitch,Volume)
addVolume v = 
  mMap (\p -> (p,v))

{-|-}
type NoteAttribute = 
    Volume Int          --  MIDI convention: 0=min, 127=max
  | Fingering Int
  | Dynamics String
  | Params (List Float)  -- HSoM has Params (List Double) - need to nake a decision here

{-| the basic Note -}
type alias Note1 = 
  (Pitch, List NoteAttribute)

{-| Music composed od Note1s -}
type alias Music1 = 
  Music Note1


{-- 2.3 Convenient Auxiliary Functions --}

{- There is no ad hoc polymorphism im elm because it has no typeclasses.
   It is evident in the note and rest counstructors which give a type error if supplied with an integer duration.
   We need to work round this somehow.  Current plan is to introduce iNote and iRest.
-}

{-| note (fractional/rational duration) -}
note : Dur -> a -> Music a
note d p =
  Prim (Note d p)

{-| note (integral duration) -}
iNote : Int -> a -> Music a
iNote i p =
  note (fromInt i) p

{-| rest (fractional/rational duration) -}
rest : Dur -> Music a
rest d =
  Prim (Rest d)

{-| rest (integral duration) -}
iRest : Int -> Music a
iRest i =
  rest (fromInt i)

{-| tempo -}
tempo : Dur -> Music a -> Music a
tempo r m =
  Modify (Tempo r) m

{-| transpose -}
transpose : AbsPitch -> Music a -> Music a
transpose i m = 
  Modify (Transpose i) m

{-| instrument -}
instrument : InstrumentName -> Music a -> Music a
instrument i m  = 
  Modify (Instrument i) m

{-| phrase -}
phrase : List PhraseAttribute -> Music a -> Music a
phrase pa m = 
  Modify (Phrase pa) m

{-| key signaure -}
keysig : PitchClass -> Mode -> Music a -> Music a
keysig pc mo m = 
  Modify (KeySig pc mo) m

{- shorthand for notes
  Note: elm doesn't allow the haskell-like shorthand:

  cff,cf : Octave -> Dur -> Music Pitch
-}
cff : Octave -> Dur -> Music Pitch
cff o d = note d (Cff,  o)

cf : Octave -> Dur -> Music Pitch
cf o d = note d (Cf,  o)

c : Octave -> Dur -> Music Pitch
c o d = note d (C,  o)

cs : Octave -> Dur -> Music Pitch
cs o d = note d (Cs,  o)

css : Octave -> Dur -> Music Pitch
css o d = note d (Css,  o)

dff : Octave -> Dur -> Music Pitch
dff o d = note d (Dff,  o)

df : Octave -> Dur -> Music Pitch
df o d = note d (Df,  o)

d : Octave -> Dur -> Music Pitch
d o d = note d (D,  o)

ds : Octave -> Dur -> Music Pitch
ds o d = note d (Ds,  o)

dss : Octave -> Dur -> Music Pitch
dss o d = note d (Dss,  o)

eff : Octave -> Dur -> Music Pitch
eff o d = note d (Eff,  o)

ef : Octave -> Dur -> Music Pitch
ef o d = note d (Ef,  o)

e : Octave -> Dur -> Music Pitch
e o d = note d (E,  o)

es : Octave -> Dur -> Music Pitch
es o d = note d (Es,  o)

ess : Octave -> Dur -> Music Pitch
ess o d = note d (Ess,  o)

fff : Octave -> Dur -> Music Pitch
fff o d = note d (Fff,  o)

ff : Octave -> Dur -> Music Pitch
ff o d = note d (Ff,  o)

f : Octave -> Dur -> Music Pitch
f o d = note d (F,  o)

fs : Octave -> Dur -> Music Pitch
fs o d = note d (Fs,  o)

fss : Octave -> Dur -> Music Pitch
fss o d = note d (Fss,  o)

gff : Octave -> Dur -> Music Pitch
gff o d = note d (Gff,  o)

gf : Octave -> Dur -> Music Pitch
gf o d = note d (Gf,  o)

g : Octave -> Dur -> Music Pitch
g o d = note d (G,  o)

gs : Octave -> Dur -> Music Pitch
gs o d = note d (Gs,  o)

gss : Octave -> Dur -> Music Pitch
gss o d = note d (Ass,  o)

af : Octave -> Dur -> Music Pitch
af o d = note d (Af,  o)

a : Octave -> Dur -> Music Pitch
a o d = note d (A,  o)

{- note - haskell allows as but for us it's a reserved word -}
ash : Octave -> Dur -> Music Pitch
ash o d = note d (As,  o)

ass : Octave -> Dur -> Music Pitch
ass o d = note d (Ass,  o)

bff : Octave -> Dur -> Music Pitch
bff o d = note d (Bff,  o)

bf : Octave -> Dur -> Music Pitch
bf o d = note d (Bf,  o)

b : Octave -> Dur -> Music Pitch
b o d = note d (B,  o)

bs : Octave -> Dur -> Music Pitch
bs o d = note d (Bs,  o)

bss : Octave -> Dur -> Music Pitch
bss o d = note d (Bss,  o)

{- 2.4 Absolute Pitches -}

{- Treating pitches simply as integers is useful in many settings, so Euterpea
uses a type synonym to define the concept of an “absolute pitch: -}



{-| The absolute pitch of a Pitch -}
absPitch : Pitch -> AbsPitch
absPitch (pc, o) =
  12 * o + pcToInt pc


chromatic = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
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

{-| Given pitch and absPitch, it is now easy to define a function trans that
transposes pitches -}
trans : Int -> Pitch -> Pitch
trans i p =
  pitch (absPitch p + i)


{-
  CHAPTER 3 Polymorphic and Higher-Order Functions
  ================================================
-}

{-| For a musical example involving the use of map, consider the task of generating
a six-note whole-tone scale starting at a given pitch -}
wts : Pitch -> List (Music Pitch)
wts p =
  let
    step n = note qn (pitch (absPitch p + n))
  in
    List.map step [0, 2, 4, 6, 8]


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

pMap : (a -> b) -> Primitive a -> Primitive b
pMap f p =
  case p of 
   Note d x -> Note d (f x)
   Rest d -> Rest d


mMap : (a -> b) -> Music a -> Music b
mMap f m =
  case m of
    Prim p -> Prim (pMap f p)
    Seq m1 m2 -> Seq (mMap f m1) (mMap f m2)
    Par m1 m2 -> Par (mMap f m1) (mMap f m2)
    Modify c m -> Modify c (mMap f m)
