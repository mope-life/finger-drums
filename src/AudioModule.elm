module AudioModule exposing
  ( AudioModule
  , Prototype
  , Msg(..)
  , Type(..)
  , Position
  , init
  , initPrototype
  , at
  , translated
  , update
  , view
  , viewPrototype
  )

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Array exposing (Array)

import MouseEvent
import AudioModule.Control as Control
import Json.Decode as Decode

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init : Type -> String -> AudioModule
init type_ name =
  { type_  = type_
  , position = (0, 0)
  , name = name
  , controls = initControls type_ name
  }

initPrototype : Type -> Prototype
initPrototype type_ =
  { type_ = type_
  , controls = initControls type_ "proto"
  }

initControls : Type -> String -> Array Control.Control
initControls type_ name =
  case type_ of
    ControllerModule ->
      Array.empty
    DestinationModule ->
      Array.empty
    ConstantModule ->
      [ Control.knobControl ("const-" ++ name ++ "-knob")
      ]
      |> Array.fromList
    VCOModule ->
      let
        group = "osc-" ++ name
      in
        [ Control.controlGroup
          [ Control.radioControl "sine" group (group ++ "-radio-sin")
            |> Control.labeled "sin"
          , Control.radioControl "square" group (group ++ "-radio-sqr")
            |> Control.labeled "sqr"
          , Control.radioControl "sawtooth" group (group ++ "-radio-saw")
            |> Control.labeled "saw"
          , Control.radioControl "triangle" group (group ++ "-radio-tri")
            |> Control.labeled "tri"
          ]
          group
        ]
        |> Array.fromList
    VCAModule ->
      Array.empty
    EnvelopeModule ->
      let
        group = "env-" ++ name
      in
        [ Control.numberControl (group ++ "-number-A")
          |> Control.labeled "A"
        , Control.numberControl (group ++ "-number-D")
          |> Control.labeled "D"
        , Control.numberControl (group ++ "-number-S")
          |> Control.labeled "S"
        , Control.numberControl (group ++ "-number-R")
          |> Control.labeled "R"
        ]
        |> Array.fromList

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias AudioModule =
  { type_ : Type
  , position : Position
  , name : String
  , controls : Array Control.Control
  }

type alias Prototype =
  { type_ : Type
  , controls : Array Control.Control
  }

type Msg
  = EndpointMouseDown MouseEvent.Point
  | ControlDelegate Int Control.Msg

type Type
  = ControllerModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule

type Direction
  = In
  | Out

type alias Position = MouseEvent.Position

at : Position -> AudioModule -> AudioModule
at position audioModule =
    { audioModule | position = position }

translated : (Float, Float) -> AudioModule  -> AudioModule
translated (dx, dy) audioModule =
  { audioModule
    | position = audioModule.position
    |> Tuple.mapBoth (\x -> x + dx) (\y -> y + dy)
  }

mapControlWithCmd :
  (Control.Control -> (Control.Control, Cmd msg))
  -> Int
  -> AudioModule
  -> (AudioModule, Cmd msg)
mapControlWithCmd transform index audioModule =
  case (Array.get index audioModule.controls) of
    Nothing ->
      (audioModule, Cmd.none)
    Just control ->
      let
        (ctrl, cmd) = transform control
      in
        ( { audioModule | controls = Array.set index ctrl audioModule.controls }
        , cmd
        )

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : (Msg -> msg) -> Msg -> AudioModule -> (AudioModule, Cmd msg)
update delegate msg audioModule =
  case msg of
    ControlDelegate index controlMsg ->
      mapControlWithCmd
        (Control.update (delegate << ControlDelegate index) controlMsg)
        index
        audioModule
    _ ->
      (audioModule, Cmd.none)

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
viewPrototype : List (Html.Attribute msg) -> Prototype -> Html.Html msg
viewPrototype extraAttributes prototype =
  Html.div
    ( Attributes.class "module-wrapper" :: extraAttributes )
    [ Html.div [ Attributes.class "prototype-click-shield"] []
    , viewEndpointBank Nothing prototype.type_ In
    , viewControlBank Nothing prototype.controls
    , viewEndpointBank Nothing prototype.type_ Out
    ]

view : (Msg -> msg) -> List (Html.Attribute msg) -> AudioModule -> Html.Html msg
view delegate extraAttributes audioModule =
  let
    pxFromFloat = \float -> ( String.fromInt << round <| float ) ++ "px"
    (xpx, ypx) = Tuple.mapBoth pxFromFloat pxFromFloat audioModule.position
  in
    Html.div
    ( List.append
      [ Attributes.style "left" xpx
      , Attributes.style "top" ypx
      , Attributes.class "module-wrapper"
      ]
      extraAttributes
    )
    [ viewEndpointBank (Just delegate) audioModule.type_ In
    , viewControlBank (Just delegate) audioModule.controls
    , viewEndpointBank (Just delegate) audioModule.type_ Out
    ]

viewControlBank : Maybe (Msg -> msg) -> Array Control.Control -> Html.Html msg
viewControlBank maybeDelegate controls =
  let
    controlView = case maybeDelegate of
      Nothing ->
        (\_ control ->
          Control.view Nothing control)
      Just delegate ->
        (\index control ->
          Control.view (Just <| delegate << ControlDelegate index) control)
  in
    Html.div
      [ Attributes.class "control-bank" ]
      ( Array.indexedMap controlView controls |> Array.toList )

viewEndpointBank : Maybe (Msg -> msg) -> Type -> Direction -> Html.Html msg
viewEndpointBank delegate type_ direction =
  let
    ( elements, label ) = case direction of
      In ->
        ( viewEndpointsIn delegate type_, "in:" )
      Out ->
        ( viewEndpointsOut delegate type_, "out:" )
    div = Html.div [ Attributes.class "endpoint-bank" ]
  in
    if List.isEmpty elements
      then div []
      else div <| Html.text label :: elements

viewEndpointsIn : Maybe (Msg -> msg) -> Type -> List (Html.Html msg)
viewEndpointsIn delegate type_ =
  case type_ of
    ControllerModule ->
      []
    DestinationModule ->
      [ viewEndpoint delegate "signal" ]
    ConstantModule ->
      []
    VCOModule ->
      [ viewEndpoint delegate "freq"
      , viewEndpoint delegate "detune"
      ]
    VCAModule ->
      [ viewEndpoint delegate "signal"
      , viewEndpoint delegate "cv"
      ]
    EnvelopeModule ->
      [ viewEndpoint delegate "gate"
      , viewEndpoint delegate "trig"
      ]

viewEndpointsOut : Maybe (Msg -> msg) -> Type -> List (Html.Html msg)
viewEndpointsOut delegate type_ =
  case type_ of
    ControllerModule ->
      [ viewEndpoint delegate "freq"
      , viewEndpoint delegate "gate"
      , viewEndpoint delegate "trig"
      ]
    DestinationModule ->
      []
    ConstantModule ->
      [ viewEndpoint delegate "cv" ]
    VCOModule ->
      [ viewEndpoint delegate "signal" ]
    VCAModule ->
      [ viewEndpoint delegate "signal" ]
    EnvelopeModule ->
      [ viewEndpoint delegate "level" ]

viewEndpoint : Maybe (Msg -> msg) -> String -> Html.Html msg
viewEndpoint maybeDelegate label =
  let
    events = case maybeDelegate of
      Nothing ->
        []
      Just delegate ->
        [ MouseEvent.pointMessageDecoder (delegate << EndpointMouseDown)
          |> Decode.andThen
            (\m -> Decode.succeed
              { message = m
              , stopPropagation = True
              , preventDefault = True
              }
            )
          |> Events.custom "mousedown"
        ]
  in
    Html.div
      ( Attributes.class "endpoint-wrapper" :: events )
      [ Html.div [ Attributes.class "endpoint-jack", Attributes.class "grabbable" ] []
      , Html.label [ Attributes.class "endpoint-label" ] [ Html.text label ]
      ]
