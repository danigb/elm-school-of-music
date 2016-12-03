module Tests exposing (..)

import Test exposing (..)
import Fuzz exposing (intRange)
import Fuzzers exposing (..)
import Expect
import String
import Music exposing (..)
import Music1 exposing (..)
import MoreMusic exposing (..)
import MEvent exposing (..)
import Ratio exposing (Rational, over, fromInt, compare)


enharmonicEquivalence : Pitch -> AbsPitch
enharmonicEquivalence =
    absPitch



{- is this in any way equivalent to musEquiv from Euterpea??? -}


musEquiv : Music Pitch -> Music Pitch -> Bool
musEquiv m1 m2 =
    let
        e1 =
            performP m1

        e2 =
            performP m2

        sortwith =
            \e1 e2 -> Ratio.compare e1.eTime e2.eTime
    in
        (List.sortWith sortwith e1) == (List.sortWith sortwith e2)


all : Test
all =
    concat
        [ music
        , musicChecks
        , moreMusicChecks
        ]


music : Test
music =
    describe "Music basic tests"
        [ test "Rest" <|
            \() ->
                Expect.equal (Rest (fromInt 3)) (Rest (fromInt 3))
        , test "Note" <|
            \() ->
                Expect.equal (Note (fromInt 3) ( C, 4 )) (Note (fromInt 3) ( C, 4 ))
        , test "Prim Rest" <|
            \() ->
                Expect.equal (Prim (Rest (over 3 4))) (Prim (Rest (over 3 4)))
        , test "Prim Note" <|
            \() ->
                Expect.equal (Prim (Note (fromInt 4) ( C, 5 ))) (Prim (Note (fromInt 4) ( C, 5 )))
        , test "note" <|
            \() ->
                Expect.equal (note (fromInt 3) C) (Prim (Note (fromInt 3) C))
        , test "rest" <|
            \() ->
                Expect.equal (rest (fromInt 1)) (Prim (Rest (fromInt 1)))
        , test "tempo" <|
            \() ->
                Expect.equal (tempo (fromInt 4) (note (fromInt 3) C)) (Modify (Tempo (fromInt 4)) (Prim (Note (fromInt 3) C)))
        , test "absPitch" <|
            \() ->
                Expect.equal 48 (absPitch ( C, 4 ))
        , test "abs pitches" <|
            \() ->
                Expect.equal [ 48, 50, 52, 53, 55 ]
                    (List.map absPitch [ ( C, 4 ), ( D, 4 ), ( E, 4 ), ( F, 4 ), ( G, 4 ) ])
        , test "pitch" <|
            \() ->
                Expect.equal ( Cs, 4 ) (pitch 49)
        , test "pitch 0 is C0" <|
            \() ->
                Expect.equal ( C, 0 ) (pitch 0)
        , test "negative pitch" <|
            \() ->
                Expect.equal ( As, -1 ) (pitch -2)
        , test "trans" <|
            \() ->
                Expect.equal ( F, 4 ) (trans 5 ( C, 4 ))
        , test "whole tone scale" <|
            \() ->
                Expect.equal
                    [ Prim (Note (over 1 4) ( A, 4 ))
                    , Prim (Note (over 1 4) ( B, 4 ))
                    , Prim (Note (over 1 4) ( Cs, 5 ))
                    , Prim (Note (over 1 4) ( Ds, 5 ))
                    , Prim (Note (over 1 4) ( F, 5 ))
                    ]
                    (wts ( A, 4 ))
        ]


musicChecks : Test
musicChecks =
    describe "Music fuzzy checks"
        [ fuzz (intRange 0 255) "absolute pitch of pitch" <|
            \i ->
                i
                    |> pitch
                    |> absPitch
                    |> Expect.equal i
        , fuzz (apitch) "pitch of absolute pitch" <|
            \p ->
                p
                    |> absPitch
                    |> pitch
                    |> Expect.equal (pitch (enharmonicEquivalence p))
        , fuzz (pitchWithInterval) "transposition up and down" <|
            \( p, i ) ->
                absPitch (trans -i (trans i p))
                    |> Expect.equal (enharmonicEquivalence p)
        , fuzz (pitchWithTwoIntervals) "transposition composes" <|
            \( p, i, j ) ->
                absPitch (trans i (trans j p))
                    |> Expect.equal (absPitch (trans (i + j) p))
        ]


moreMusicChecks : Test
moreMusicChecks =
    describe "More Music fuzzy checks"
        [ fuzz (musicPitchLine) "reverse music line twice" <|
            \line ->
                line
                    |> (retro << retro)
                    |> Expect.equal line
        , fuzz (Fuzzers.music 5) "inverted music duration" <|
            \mus ->
                mus
                    |> invert
                    |> dur
                    |> Expect.equal (dur mus)
        ]
