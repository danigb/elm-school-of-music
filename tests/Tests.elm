import ElmTest exposing (..)
import Music exposing (..)
import String


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
      test "Rest" (assertEqual (Rest 3) (Rest 3) ),
      test "Note" (assertEqual (Note 3 (C, 4)) (Note 3 (C, 4))),
      test "Prim Rest" (assertEqual (Prim (Rest 3))
        (Prim (Rest 3))),
      test "Prim Note" (assertEqual (Prim (Note 4 (C, 5)))
        (Prim (Note 4 (C, 5)))),
      test "note" (assertEqual (note 3 C) (Prim (Note 3 C))),
      test "rest" (assertEqual (rest 1) (Prim (Rest 1))),
      test "tempo" (assertEqual (tempo 4 (note 3 C))
        (Modify (Tempo 4) (Prim (Note 3 C)))),
      test "absPitch" (assertEqual 48 (absPitch (C, 4))),
      test "abs pitches" (assertEqual [48,50,52,53,55]
        (List.map absPitch [(C, 4), (D, 4), (E, 4), (F, 4), (G, 4)])),
      test "pitch" (assertEqual (Df, 4) (pitch 49)),
      test "pitch 0 is C0" (assertEqual (C, 0) (pitch 0)),
      test "negative pitch" (assertEqual (Bf, -1) (pitch -2)),
      test "trans" (assertEqual (F, 4) (trans 5 (C, 4))),
      test "whole tone scale" (assertEqual (wts (A, 4))
        [Prim (Note 0.25 (A,4)),Prim (Note 0.25 (B,4)),Prim (Note 0.25 (Df,5)),
         Prim (Note 0.25 (Ef,5)),Prim (Note 0.25 (F,5))])
    ]

main =
    runSuite music
