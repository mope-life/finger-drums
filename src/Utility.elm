module Utility exposing (..)

type alias Vec2 = ( Float, Float )

apply2 : ( a -> b -> c ) -> ( a, b ) -> c
apply2 f ( a, b ) =
  f a b

push : a -> List a -> List a
push a list =
  a :: list

ifAnything : b -> b -> Maybe a -> b
ifAnything onAnything onNothing maybe =
  case maybe of
    Just _ -> onAnything
    Nothing -> onNothing

ifNothing : Maybe a -> Maybe a -> Maybe a
ifNothing onNothing maybe =
  ifAnything maybe onNothing maybe

findFirstValid : ( a -> Maybe b) -> List a -> Maybe b
findFirstValid transform =
  List.foldl
    ( \a -> ifNothing ( transform a ) )
    Nothing
