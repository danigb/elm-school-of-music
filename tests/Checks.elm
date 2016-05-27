module Checks exposing (..)

import Music.Producers exposing (..)
import Music exposing (..)
import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test
import ElmTest

enharmonicEquivalence : Pitch -> AbsPitch 
enharmonicEquivalence =
  absPitch


claim_abspitch_inverse_of_pitch =
  claim
    "the absolute pitch a pitch gives back the original"
  `that`
    (\a -> absPitch (pitch a))
  `is`
    (identity)
  `for`
    rangeInt 0 255

claim_pitch_inverse_of_abspitch =
  claim
    "the pitch of an absolute pitch gives an enharmonic equivalent of the original"
  `that`
    (\p -> absPitch (pitch (absPitch p)))
  `is`
    (enharmonicEquivalence)
  `for`
    pitches

suite_music =
  suite "Music Suite"
    [ claim_abspitch_inverse_of_pitch
    , claim_pitch_inverse_of_abspitch
    ]

evidence : Evidence
evidence = quickCheck suite_music

main =
    ElmTest.runSuite (Check.Test.evidenceToTest evidence)

