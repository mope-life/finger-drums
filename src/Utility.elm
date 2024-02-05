module Utility exposing (..)

type alias Vec2 = ( Float, Float )

push : a -> List a -> List a
push a list =
  a :: list
