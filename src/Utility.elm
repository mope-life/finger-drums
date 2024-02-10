module Utility exposing (..)

import Task
import Browser.Dom as Dom
import MouseEvent
import Array exposing (Array)
import Dict exposing (Dict)

--------------------------------------------------------------------------------
-- Generic utilities -----------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Abstract Data Structures ----------------------------------------------------

type alias DataStructure index item struct =
  { struct : struct
  , getItem : index -> struct -> Maybe item
  , setItem : index -> item -> struct -> struct
  }

mapThing : ( a -> a ) -> i -> DataStructure i a s -> DataStructure i a s
mapThing transform index dataStruct =
  case dataStruct.getItem index dataStruct.struct of
    Nothing -> dataStruct
    Just a -> { dataStruct | struct = dataStruct.setItem index (transform a) dataStruct.struct }

unwrap : DataStructure i a s -> s
unwrap dataStruct = dataStruct.struct

wrapAndMap : ( s -> DataStructure i a s ) -> ( a -> a) -> i -> s -> s
wrapAndMap wrap transform index =
  unwrap << mapThing transform index << wrap

wrapArray : Array a -> DataStructure Int a (Array a)
wrapArray array =
  { struct = array
  , getItem = Array.get
  , setItem = Array.set
  }

wrapDict : Dict comparable a -> DataStructure comparable a (Dict comparable a)
wrapDict dict =
  { struct = dict
  , getItem = Dict.get
  , setItem = Dict.insert
  }

--------------------------------------------------------------------------------
-- DOM / Mouse utilities -------------------------------------------------------

fetchElement : String -> (Result Dom.Error Dom.Element -> msg) -> Cmd msg
fetchElement htmlId resultMessage =
  Task.attempt resultMessage (Dom.getElement htmlId)

fetchElements : List String -> (Result Dom.Error (List Dom.Element) -> msg) -> Cmd msg
fetchElements htmlIdList resultMessage =
  List.map Dom.getElement htmlIdList
  |> Task.sequence
  |> Task.attempt resultMessage

type alias Vec2 = ( Float, Float )

delta : Vec2 -> Vec2 -> Vec2
delta (x1, y1) (x2, y2) =
  (x2 - x1, y2 - y1)

mousePoint : MouseEvent.MouseInfo -> Vec2
mousePoint { clientX, clientY } =
  ( clientX, clientY )

positionFromMouseInfo : MouseEvent.MouseInfo -> Vec2
positionFromMouseInfo { clientX, clientY, offsetX, offsetY } =
  ( clientX - offsetX
  , clientY - offsetY
  )

dimensionsFromMouseInfo : MouseEvent.MouseInfo -> Vec2
dimensionsFromMouseInfo { targetWidth, targetHeight } =
  ( targetWidth, targetHeight )

midpointFromMouseInfo : MouseEvent.MouseInfo -> Vec2
midpointFromMouseInfo mouseInfo =
  midpointOf mouseInfo positionFromMouseInfo dimensionsFromMouseInfo

positionFromDomElement : Dom.Element -> Vec2
positionFromDomElement domElement =
  ( domElement.element.x, domElement.element.y )

dimensionsFromDomElement : Dom.Element -> Vec2
dimensionsFromDomElement domElement =
  ( domElement.element.width, domElement.element.height )

midpointFromDomElement : Dom.Element -> Vec2
midpointFromDomElement domElement =
  midpointOf domElement positionFromDomElement dimensionsFromDomElement

midpointOf : t -> ( t -> Vec2 ) -> ( t -> Vec2 ) -> Vec2
midpointOf thing getPosition getDimensions =
  let
    ( x, y ) = ( getPosition thing )
    ( width, height ) = ( getDimensions thing )
  in
    ( x + width / 2, y + height / 2 )
