module AudioModule exposing
  ( AudioModule
  , PrototypeModule
  , Type(..)
  , Mode(..)
  , PositionInfo
  , DragState(..)
  , init
  , initPrototype
  , at
  , translated
  , dragged
  , notDragged
  , mapEndpoint
  , viewPrototype
  , viewFloating
  , viewFixed
  )

import Html
import Html.Attributes as Attributes
import Array exposing (Array)
import AudioModule.Control as Control
import AudioModule.Control exposing (Control)
import AudioModule.Endpoint as Endpoint
import AudioModule.Endpoint exposing (Endpoint)
import AudioModule.Translators exposing (Translators)
import MouseEvent
import Utility exposing (..)

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init : Mode -> Type -> String -> AudioModule
init mode type_ htmlId =
  { mode = mode
  , htmlId = htmlId
  , controls = initControls type_ htmlId
  , endpoints = initEndpoints type_ htmlId
  }

initPrototype : Type -> String -> PrototypeModule
initPrototype type_ htmlId =
  { type_ = type_
  , htmlId = htmlId
  , controls = initControls type_ htmlId
  , endpoints = initEndpoints type_ htmlId
  }

initControls : Type -> String -> Array Control
initControls type_ parentHtmlId =
  ( case type_ of
    KeyboardModule ->
      [ Control.initKeyboard (parentHtmlId ++ "-keyboard") ]
    DestinationModule ->
      [ ]
    ConstantModule ->
      [ Control.initKnob (parentHtmlId ++ "-knob") ]
    VCOModule ->
      [ ( Control.initControlGroup
          [ Control.initRadio "sine" parentHtmlId (parentHtmlId ++ "-radio-sin")
          |> Control.labeled "sin"
          , Control.initRadio "square" parentHtmlId (parentHtmlId ++ "-radio-sqr")
          |> Control.labeled "sqr"
          , Control.initRadio "sawtooth" parentHtmlId (parentHtmlId ++ "-radio-saw")
          |> Control.labeled "saw"
          , Control.initRadio "triangle" parentHtmlId (parentHtmlId ++ "-radio-tri")
          |> Control.labeled "tri"
          ]
          parentHtmlId
        )
      ]
    VCAModule ->
      [ ]
    EnvelopeModule ->
      [ Control.initNumber (parentHtmlId ++ "-number-A")
      |> Control.labeled "A"
      , Control.initNumber (parentHtmlId ++ "-number-D")
      |> Control.labeled "D"
      , Control.initNumber (parentHtmlId ++ "-number-S")
      |> Control.labeled "S"
      , Control.initNumber (parentHtmlId ++ "-number-R")
      |> Control.labeled "R"
      ]
  )
  |> Array.fromList

initEndpoints : Type -> String -> Array Endpoint
initEndpoints type_ parentHtmlId =
  let
    initialize direction label =
      Endpoint.init direction label
        ( String.concat
          [ parentHtmlId
          , case direction of
            Endpoint.In -> "-endpoint-in-"
            Endpoint.Out -> "-endpoint-out-"
          , label
          ]
        )
  in ( case type_ of
    KeyboardModule ->
      [ initialize Endpoint.Out "freq"
      , initialize Endpoint.Out "gate"
      , initialize Endpoint.Out "trig"
      ]
    DestinationModule ->
      [ initialize Endpoint.In "signal"
      ]
    ConstantModule ->
      [ initialize Endpoint.Out "cv"
      ]
    VCOModule ->
      [ initialize Endpoint.In "freq"
      , initialize Endpoint.In "detune"
      , initialize Endpoint.Out "signal"
      ]
    VCAModule ->
      [ initialize Endpoint.In "signal"
      , initialize Endpoint.In "cv"
      , initialize Endpoint.Out "signal"
      ]
    EnvelopeModule ->
      [ initialize Endpoint.In "gate"
      , initialize Endpoint.In "trig"
      , initialize Endpoint.Out "cv"
      ]
  )
  |> Array.fromList

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias AudioModule =
  { mode : Mode
  , htmlId : String
  , controls : Array Control
  , endpoints : Array Endpoint
  }

type alias PrototypeModule =
  { type_ : Type
  , htmlId : String
  , controls : Array Control
  , endpoints : Array Endpoint
  }

type Mode
  = Floating PositionInfo
  | Fixed

type alias PositionInfo =
  { dragState : DragState
  , position : Vec2
  }

type DragState = Dragged | NotDragged

type Type
  = KeyboardModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule

dragged : AudioModule -> AudioModule
dragged audioModule =
  case audioModule.mode of
    Floating posinfo ->
      { audioModule | mode = Floating { posinfo | dragState = Dragged } }
    _ ->
      audioModule

notDragged : AudioModule -> AudioModule
notDragged audioModule =
  case audioModule.mode of
    Floating posinfo ->
      { audioModule | mode = Floating { posinfo | dragState = NotDragged } }
    _ ->
      audioModule

at : Vec2 -> AudioModule -> AudioModule
at position audioModule =
  case audioModule.mode of
    Floating posinfo ->
      { audioModule | mode = Floating { posinfo | position = position } }
    _ ->
      audioModule

translated : (Float, Float) -> AudioModule -> AudioModule
translated (dx, dy) audioModule =
  case audioModule.mode of
    Floating posinfo ->
      { audioModule
        | mode = Floating
          { posinfo
          | position = (Tuple.mapBoth (\x -> x + dx) (\y -> y + dy) posinfo.position)
          }
      }
    _ ->
      audioModule

mapEndpoint : ( Endpoint -> Endpoint ) -> Int -> AudioModule -> AudioModule
mapEndpoint transform index audioModule =
  case (Array.get index audioModule.endpoints) of
    Nothing ->
      audioModule
    Just endpoint ->
      { audioModule
      | endpoints = Array.set index (transform endpoint) audioModule.endpoints
      }

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
viewFloating : PositionInfo -> Translators msg -> AudioModule -> Html.Html msg
viewFloating { position, dragState } translators audioModule =
  let
    pxFromFloat = \float -> ( String.fromInt << round <| float ) ++ "px"
    ( xpx, ypx ) = Tuple.mapBoth pxFromFloat pxFromFloat position
  in
    Html.div
    ( List.concat
      [ commonAttributes audioModule.htmlId
      , [ Attributes.style "left" xpx
        , Attributes.style "top" ypx
        , MouseEvent.onCustom "mousedown" translators.startDrag
        ]
      , case dragState of
        Dragged -> [ Attributes.class "grabbing" ]
        NotDragged -> [ ]
      ]
    )
    ( commonContents ( Just translators ) audioModule.endpoints audioModule.controls )

viewFixed : Translators msg -> AudioModule -> Html.Html msg
viewFixed translators audioModule =
  Html.div
    ( commonAttributes audioModule.htmlId )
    ( commonContents ( Just translators ) audioModule.endpoints audioModule.controls )

viewPrototype : ( Type -> MouseEvent.MouseInfo -> msg ) -> PrototypeModule -> Html.Html msg
viewPrototype createAudioModule prototype =
  Html.div
    ( commonAttributes prototype.htmlId )
    ( Html.div
      [ Attributes.class "prototype-click-shield"
      , MouseEvent.onCustom "mousedown" ( createAudioModule prototype.type_ )
      ]
      [ ]
      :: ( commonContents Nothing prototype.endpoints prototype.controls )
    )

commonAttributes : String -> List (Html.Attribute msg)
commonAttributes htmlId =
  [ Attributes.class "audio-module"
  , Attributes.id htmlId
  ]

commonContents : Maybe ( Translators msg ) -> Array Endpoint -> Array Control -> List ( Html.Html msg )
commonContents maybeTranslators endpoints controls =
  [ viewEndpointBank maybeTranslators Endpoint.In endpoints
  , viewControlBank maybeTranslators controls
  , viewEndpointBank maybeTranslators Endpoint.Out endpoints
  ]

viewEndpointBank : Maybe ( Translators msg ) -> Endpoint.Direction -> Array Endpoint -> Html.Html msg
viewEndpointBank maybeTranslators direction endpoints =
  endpoints
  |> Array.filter (\e -> e.direction == direction )
  |> Array.indexedMap ( Endpoint.view maybeTranslators )
  |>(\elements -> if Array.isEmpty elements
    then [ ]
    else
      ( Html.text <| case direction of
        Endpoint.In -> "in:"
        Endpoint.Out -> "out:"
      )
      :: ( Array.toList elements )
    )
  |> Html.div [ Attributes.class "endpoint-bank" ]

viewControlBank : Maybe ( Translators msg ) -> Array Control -> Html.Html msg
viewControlBank maybeTranslators controls =
  Html.div
    [ Attributes.class "control-bank" ]
    ( Array.indexedMap ( Control.view maybeTranslators ) controls |> Array.toList )
