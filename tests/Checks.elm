module Checks exposing (..)

import Music.Producers exposing (..)
import Music exposing (..)
import MoreMusic exposing (..)
import Check exposing (..)
import Check.Producer exposing (..)
import Check.Test
import ElmTest

enharmonicEquivalence : Pitch -> AbsPitch 
enharmonicEquivalence =
  absPitch

-- Tests for Music

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
    apitch

claim_transposition_is_invertible =
  claim
    "transposition up and down leaves the original pitch unchanged"
  `that`
    (\(p,i) -> absPitch (trans -i (trans i p)))
  `is`
    (\(p,i) -> enharmonicEquivalence p)
  `for`
    pitchWithInterval

claim_transposition_composes =
  claim
    "transposition composes"
  `that`
    (\(p,i,j) -> absPitch (trans i (trans j p)))
  `is`
    (\(p,i,j) -> absPitch (trans (i + j) p))
  `for`
    pitchWithTwoIntervals

claim_retro_is_invertible =
  claim
    "a line of music reversed twice yields the original"
  `that`
    (\line -> (retro >> retro) line)
  `is`
    (identity)
  `for`
    musicPitchLine

suite_music =
  suite "Music Suite"
    [ claim_abspitch_inverse_of_pitch
    , claim_pitch_inverse_of_abspitch
    , claim_transposition_is_invertible
    , claim_transposition_composes
    , claim_retro_is_invertible 
    ]

evidence : Evidence
evidence = quickCheck suite_music

main =
    ElmTest.runSuite (Check.Test.evidenceToTest evidence)

