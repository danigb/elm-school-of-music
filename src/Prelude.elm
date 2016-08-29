module Prelude exposing (..)

{- functions from the Haskell Prelude missing in elm 

@docs id, const

-}

{-|-}
id : a -> a
id a =
  a

{-| this is the SKI calculus K combinator -}
const : a -> b -> a
const a b =
  a
