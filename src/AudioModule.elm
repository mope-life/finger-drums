module AudioModule exposing
  ( AudioModule
  , Translators
  , Prototype
  , Position
  , Msg(..)
  , Type(..)
  , init
  , initPrototype
  , at
  , translated
  , dragged
  , notDragged
  , update
  , view
  , viewPrototype
  )

import Html
import Html.Attributes as Attributes
import Array exposing (Array)
import AudioModule.Control as Control
import AudioModule.Control exposing (Control)
import AudioModule.Endpoint as Endpoint
import AudioModule.Endpoint exposing (Endpoint)
import MouseEvent

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init : Translators msg -> Type -> String -> AudioModule msg
init translators type_ id =
  let
    endpointInitializer = ( initEndpoint translators.endpointTranslators id )
  in
    { id = id
    , position = (0, 0)
    , dragged = False
    , translators = translators
    , controls = initControls translators type_ id
    , endpoints = initEndpoints endpointInitializer type_
    }

initPrototype : Translators msg -> Type -> String -> Prototype msg
initPrototype translators type_ id =
  let
    endpointInitializer = ( initEndpoint translators.endpointTranslators id )
  in
    { id = id
    , type_ = type_
    , translators = translators
    , controls = initControls translators type_ id
    , endpoints = initEndpoints endpointInitializer type_
    }

initControls : Translators msg -> Type -> String -> Array (Control msg)
initControls translators type_ id =
  ( case type_ of
    ControllerModule ->
      [ ]
    DestinationModule ->
      [ ]
    ConstantModule ->
      [ ( Control.initKnob (id ++ "-knob"), Nothing) ]
    VCOModule ->
      [ (\t -> Control.initControlGroup
          [ Control.initRadio "sine" id (id ++ "-radio-sin") t
            |> Control.labeled "sin"
          , Control.initRadio "square" id (id ++ "-radio-sqr") t
            |> Control.labeled "sqr"
          , Control.initRadio "sawtooth" id (id ++ "-radio-saw") t
            |> Control.labeled "saw"
          , Control.initRadio "triangle" id (id ++ "-radio-tri") t
            |> Control.labeled "tri"
          ] id t
        , Nothing
        )
      ]
    VCAModule ->
      [ ]
    EnvelopeModule ->
      [ ( Control.initNumber (id ++ "-number-A"), Just "A" )
      , ( Control.initNumber (id ++ "-number-D"), Just "D" )
      , ( Control.initNumber (id ++ "-number-S"), Just "S" )
      , ( Control.initNumber (id ++ "-number-R"), Just "R" )
      ]
  )
  |> List.indexedMap
    (\index (ctrl, label) ->
      ctrl
      { loopback = translators.loopback << ControlDelegate index
      , input = translators.loopback << Input index
      }
      |> case label of
        Nothing -> identity
        Just str -> Control.labeled str
    )
  |> Array.fromList

initEndpoints :
  (Endpoint.Direction -> String -> Endpoint msg)
  -> Type
  -> Array (Endpoint msg)
initEndpoints initializer type_ =
  ( case type_ of
    ControllerModule ->
      [ initializer Endpoint.Out "freq"
      , initializer Endpoint.Out "gate"
      , initializer Endpoint.Out "trig"
      ]
    DestinationModule ->
      [ initializer Endpoint.In "signal"
      ]
    ConstantModule ->
      [ initializer Endpoint.Out "cv"
      ]
    VCOModule ->
      [ initializer Endpoint.In "freq"
      , initializer Endpoint.In "detune"
      , initializer Endpoint.Out "signal"
      ]
    VCAModule ->
      [ initializer Endpoint.In "signal"
      , initializer Endpoint.In "cv"
      , initializer Endpoint.Out "signal"
      ]
    EnvelopeModule ->
      [ initializer Endpoint.In "gate"
      , initializer Endpoint.In "trig"
      , initializer Endpoint.Out "cv"
      ]
  )
  |> Array.fromList

initEndpoint :
  Endpoint.Translators msg
  -> String
  -> Endpoint.Direction
  -> String
  -> Endpoint msg
initEndpoint translators parentId direction label =
  let
    midfix = case direction of
      Endpoint.In ->
        "-endpoint-in-"
      Endpoint.Out ->
        "-endpoint-out-"
    endpointId = (parentId ++ midfix ++ label)
  in
    Endpoint.init translators endpointId direction label

dragged : AudioModule msg -> AudioModule msg
dragged audioModule =
  { audioModule | dragged = True }

notDragged : AudioModule msg -> AudioModule msg
notDragged audioModule =
  { audioModule | dragged = False }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias AudioModule msg =
  { id : String
  , position : Position
  , dragged : Bool
  , translators : Translators msg
  , controls : Array (Control msg)
  , endpoints : Array (Endpoint msg)
  }

type alias Prototype msg =
  { id : String
  , type_ : Type
  , translators : Translators msg
  , controls : Array (Control msg)
  , endpoints : Array (Endpoint msg)
  }

type alias Translators msg =
  { loopback : Msg -> msg
  , mouseDown : MouseEvent.MouseInfo -> msg
  , endpointTranslators : Endpoint.Translators msg
  }

type Msg
  = ControlDelegate Int Control.Msg
  | Input Int String
  | Ignore MouseEvent.MouseInfo

type Type
  = ControllerModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule

type alias Position = (Float, Float)

at : Position -> AudioModule msg -> AudioModule msg
at position audioModule =
    { audioModule | position = position }

translated : (Float, Float) -> AudioModule msg -> AudioModule msg
translated (dx, dy) audioModule =
  { audioModule
    | position = audioModule.position
    |> Tuple.mapBoth (\x -> x + dx) (\y -> y + dy)
  }

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> AudioModule msg -> (AudioModule msg, Cmd msg)
update msg audioModule =
  case msg of
    ControlDelegate index controlMsg ->
      updateControl index controlMsg audioModule
    _ ->
      ( audioModule, Cmd.none )

updateControl :
  Int
  -> Control.Msg
  -> AudioModule msg
  -> (AudioModule msg, Cmd msg)
updateControl index controlMsg audioModule =
  case (Array.get index audioModule.controls) of
    Nothing ->
      ( audioModule, Cmd.none )
    Just control ->
      let
        ( ctrl, cmd ) = Control.update controlMsg control
      in
        ( { audioModule | controls = Array.set index ctrl audioModule.controls }
        , cmd
        )

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
viewPrototype : Prototype msg -> Html.Html msg
viewPrototype prototype =
  Html.div
    [ Attributes.class "audio-module"
    , Attributes.id (prototype.id ++ "-proto")
    ]
    [ Html.div
      [ Attributes.class "prototype-click-shield"
      , MouseEvent.onCustom "mousedown" prototype.translators.mouseDown
      ]
      []
    , viewEndpointBank prototype.endpoints Endpoint.In
    , viewControlBank prototype.controls
    , viewEndpointBank prototype.endpoints Endpoint.Out
    ]

view : AudioModule msg -> Html.Html msg
view audioModule =
  let
    pxFromFloat = \float -> ( String.fromInt << round <| float ) ++ "px"
    (xpx, ypx) = Tuple.mapBoth pxFromFloat pxFromFloat audioModule.position
  in
    Html.div
    [ Attributes.style "left" xpx
    , Attributes.style "top" ypx
    , Attributes.classList
      [ ("audio-module", True)
      , ("grabbing", audioModule.dragged)
      ]
    , Attributes.id audioModule.id
    , MouseEvent.onCustom "mousedown" audioModule.translators.mouseDown
    ]
    [ viewEndpointBank audioModule.endpoints Endpoint.In
    , viewControlBank audioModule.controls
    , viewEndpointBank audioModule.endpoints Endpoint.Out
    ]

viewControlBank : Array (Control msg) -> Html.Html msg
viewControlBank controls =
  Html.div
    [ Attributes.class "control-bank" ]
    ( Array.map Control.view controls |> Array.toList )

viewEndpointBank : Array (Endpoint msg) -> Endpoint.Direction -> Html.Html msg
viewEndpointBank endpoints direction =
  let
    elements =
      endpoints
      |> Array.filter (\e -> e.direction == direction)
      |> Array.map Endpoint.view
      |> Array.toList
  in
    Html.div
      [ Attributes.class "endpoint-bank" ]
      ( if List.isEmpty elements
        then []
        else
          ( Html.text <| case direction of
              Endpoint.In -> "in:"
              Endpoint.Out -> "out:"
          ) :: elements
      )
