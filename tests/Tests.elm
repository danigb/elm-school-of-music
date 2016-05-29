import ElmTest exposing (..)
import Music exposing (..)
import String


import Ratio exposing (Rational, over, fromInt)



tests : Test
tests =
    suite "A Test Suite"
        [
            test "Addition" (assertEqual (3 + 7) 10),
            test "String.left" (assertEqual "a" (String.left 1 "abcdefg")),
            test "This test should pass" (assert True)
        ]

music : Test
music =
  suite "Music"
    [
      test "Rest" (assertEqual (Rest (fromInt 3)) (Rest (fromInt 3)) ),
      test "Note" (assertEqual (Note (fromInt 3) (C, 4)) (Note (fromInt 3) (C, 4))),
      test "Prim Rest" (assertEqual (Prim (Rest (3 `over` 4)))
        (Prim (Rest (3 `over` 4)))),
      test "Prim Note" (assertEqual (Prim (Note (fromInt 4) (C, 5)))
        (Prim (Note (fromInt 4) (C, 5)))),
      test "note" (assertEqual (note (fromInt 3) C) (Prim (Note (fromInt 3) C))),
      test "rest" (assertEqual (rest (fromInt 1)) (Prim (Rest (fromInt 1)))),
      test "tempo" (assertEqual (tempo (fromInt 4) (note (fromInt 3) C))
        (Modify (Tempo (fromInt 4)) (Prim (Note (fromInt 3) C)))),
      test "absPitch" (assertEqual 48 (absPitch (C, 4))),
      test "abs pitches" (assertEqual [48,50,52,53,55]
        (List.map absPitch [(C, 4), (D, 4), (E, 4), (F, 4), (G, 4)])),
      test "pitch" (assertEqual (Cs, 4) (pitch 49)),
      test "pitch 0 is C0" (assertEqual (C, 0) (pitch 0)),
      test "negative pitch" (assertEqual (As, -1) (pitch -2)),
      test "trans" (assertEqual (F, 4) (trans 5 (C, 4))),
      test "whole tone scale" (assertEqual 
        [Prim (Note (1 `over` 4) (A,4)),Prim (Note (1 `over` 4) (B,4)),Prim (Note (1 `over` 4) (Cs,5)),
         Prim (Note (1 `over` 4) (Ds,5)),Prim (Note (1 `over` 4) (F,5))]  (wts (A, 4)) )
    ]

main =
    runSuite music
