# Elm school of music

An attempt to port [HSoM](http://www.cs.yale.edu/homes/hudak/Papers/HSoM.pdf) to elm. You can read the original Haskell source code [here](https://github.com/Euterpea/Euterpea2/blob/master/Euterpea/Music.lhs)

__A work in progress. Very incomplete__


To run the tests, install the latest version of elm-test (currently 0.17.1), clone this repo and run:

    elm-test
    
(The tests now use _elm-community/elm-test/2.0.1_ which deprecates elm-check. It replaces the old Producers with the new concept of Fuzzers.  All QickCheck-style checks can now be incorporated into a conventional elm-test suite)

