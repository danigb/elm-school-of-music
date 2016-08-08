module Prelude exposing (..)

{- functions from the Haskell Prelude missing in elm 

@docs const

-}

{-| this is the SKI calculus K combinator -}
const : a -> b -> a
const a b =
  a
