module Utility exposing (..)

type alias Vec2 = ( Float, Float )

apply2 : ( a -> b -> c ) -> ( a, b ) -> c
apply2 f ( a, b ) =
  f a b

push : a -> List a -> List a
push a list =
  a :: list
