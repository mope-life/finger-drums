module AudioModule exposing
  ( AudioModule
  , PrototypeModule
  , Type(..)
  , Mode(..)
  , init
  , initPrototype
  , at
  , translated
  , mapEndpoint
  , updateControlValue
  , updateEndpointDelta
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
import Browser.Dom as Dom

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
    makeHtmlId direction label =
      ( String.concat
        [ parentHtmlId
        , case direction of
          Endpoint.In -> "-endpoint-in-"
          Endpoint.Out -> "-endpoint-out-"
        , label
        ]
      )
    initialize direction label =
      Endpoint.init direction label ( makeHtmlId direction label )

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
  = Floating Vec2
  | Fixed

type Type
  = KeyboardModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule

at : Vec2 -> AudioModule -> AudioModule
at position audioModule =
  case audioModule.mode of
    Floating _ ->
      { audioModule
      | mode = Floating position
      , endpoints = Array.map (Endpoint.updateMidpoint position) audioModule.endpoints
      }
    _ ->
      audioModule

translated : (Float, Float) -> AudioModule -> AudioModule
translated (dx, dy) audioModule =
  case audioModule.mode of
    Floating position ->
      let
        newpos = Tuple.mapBoth (\x -> x + dx) (\y -> y + dy) position
      in
        { audioModule
        | mode = Floating newpos
        , endpoints = Array.map (Endpoint.updateMidpoint newpos) audioModule.endpoints
        }
    _ ->
      audioModule

mapEndpoint : ( Endpoint -> Endpoint ) -> Int -> AudioModule -> AudioModule
mapEndpoint transform index audioModule =
  { audioModule
  | endpoints = wrapAndMap wrapArray transform index audioModule.endpoints
  }

mapControl : ( Control -> Control ) -> Int -> AudioModule -> AudioModule
mapControl transform index audioModule =
  { audioModule
  | controls = wrapAndMap wrapArray transform index audioModule.controls
  }

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
updateControlValue : String -> Int -> AudioModule -> AudioModule
updateControlValue value =
  mapControl (\control -> { control | value = value } )

updateEndpointDelta : Dom.Element -> Dom.Element -> Int -> AudioModule -> AudioModule
updateEndpointDelta endpointElement audioModuleElement =
  let
    endpointMidpoint = midpointFromDomElement endpointElement
    audioModulePosition = positionFromDomElement audioModuleElement
  in
    mapEndpoint
      ( delta audioModulePosition endpointMidpoint
      |> Endpoint.atDelta audioModulePosition
      )

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
viewFloating : Vec2 -> Translators msg -> AudioModule -> Html.Html msg
viewFloating position translators audioModule =
  let
    pxFromFloat = \float -> ( String.fromInt << round <| float ) ++ "px"
    ( xpx, ypx ) = Tuple.mapBoth pxFromFloat pxFromFloat position
  in
    Html.div
    ( List.concat
      [ commonAttributes audioModule.htmlId
      , [ Attributes.style "left" xpx
        , Attributes.style "top" ypx
        , Attributes.class "grabbable"
        , MouseEvent.onCustom "mousedown" translators.startDrag
        ]
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
      , Attributes.class "grabbable"
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
  let
    viewEndpoint index =
      Maybe.map (\t -> t.endpointTranslators index ) maybeTranslators
      |> Endpoint.view
  in
    Array.toIndexedList endpoints
    |> List.filter (\(_, endpoint) -> endpoint.direction == direction )
    |> List.map ( apply2 viewEndpoint )
    |>(\elements ->
      if List.isEmpty elements
      then [ ]
      else
        ( Html.text ( case direction of
          Endpoint.In -> "in:"
          Endpoint.Out -> "out:"
        ) )
        :: elements
      )
    |> Html.div [ Attributes.class "endpoint-bank" ]

viewControlBank : Maybe ( Translators msg ) -> Array Control -> Html.Html msg
viewControlBank maybeTranslators controls =
  let
    viewControl index =
      Maybe.map (\t -> t.controlTranslators index ) maybeTranslators
      |> Control.view
  in
    Html.div
      [ Attributes.class "control-bank" ]
      ( Array.indexedMap viewControl controls |> Array.toList )
