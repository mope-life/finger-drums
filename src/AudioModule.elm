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
import Array exposing (Array)
import AudioModule.Control as Control
import AudioModule.Control exposing (Control)
import AudioModule.Endpoint as Endpoint
import AudioModule.Endpoint exposing (Endpoint)
import MouseEvent

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init : Type -> String -> AudioModule
init type_ name =
  { type_  = type_
  , position = (0, 0)
  , name = name
  , controls = initControls type_ name
  , endpoints = initEndpoints type_ name
  }

initPrototype : Type -> Prototype
initPrototype type_ =
  { type_ = type_
  , controls = initControls type_ "proto"
  , endpoints = initEndpoints type_ "proto"
  }

initControls : Type -> String -> Array Control
initControls type_ name =
  case type_ of
    ControllerModule ->
      Array.empty
    DestinationModule ->
      Array.empty
    ConstantModule ->
      [ Control.initKnob ("const-" ++ name ++ "-knob")
      ] |> Array.fromList
    VCOModule ->
      let
        group = "osc-" ++ name
      in
        [ Control.initControlGroup
          [ Control.initRadio "sine" group (group ++ "-radio-sin")
            |> Control.labeled "sin"
          , Control.initRadio "square" group (group ++ "-radio-sqr")
            |> Control.labeled "sqr"
          , Control.initRadio "sawtooth" group (group ++ "-radio-saw")
            |> Control.labeled "saw"
          , Control.initRadio "triangle" group (group ++ "-radio-tri")
            |> Control.labeled "tri"
          ]
          group
        ] |> Array.fromList
    VCAModule ->
      Array.empty
    EnvelopeModule ->
      let
        group = "env-" ++ name
      in
        [ Control.initNumber (group ++ "-number-A")
          |> Control.labeled "A"
        , Control.initNumber (group ++ "-number-D")
          |> Control.labeled "D"
        , Control.initNumber (group ++ "-number-S")
          |> Control.labeled "S"
        , Control.initNumber (group ++ "-number-R")
          |> Control.labeled "R"
        ] |> Array.fromList

initEndpoints : Type -> String -> Array Endpoint
initEndpoints type_ name =
  ( case type_ of
      ControllerModule ->
        [ initEndpoint name Endpoint.Out "freq"
        , initEndpoint name Endpoint.Out "gate"
        , initEndpoint name Endpoint.Out "trig"
        ]
      DestinationModule ->
        [ initEndpoint name Endpoint.In "signal"
        ]
      ConstantModule ->
        [ initEndpoint name Endpoint.Out "cv"
        ]
      VCOModule ->
        [ initEndpoint name Endpoint.In "freq"
        , initEndpoint name Endpoint.In "detune"
        , initEndpoint name Endpoint.Out "signal"
        ]
      VCAModule ->
        [ initEndpoint name Endpoint.In "signal"
        , initEndpoint name Endpoint.In "cv"
        , initEndpoint name Endpoint.Out "signal"
        ]
      EnvelopeModule ->
        [ initEndpoint name Endpoint.In "gate"
        , initEndpoint name Endpoint.In "trig"
        , initEndpoint name Endpoint.Out "cv"
        ]
  ) |> Array.fromList

initEndpoint : String -> Endpoint.Direction -> String -> Endpoint
initEndpoint name direction label =
  let
    midfix = case direction of
      Endpoint.In ->
        "-endpoint-in-"
      Endpoint.Out ->
        "-endpoint-out-"
  in
    Endpoint.init (name ++ midfix ++ label) direction label

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias AudioModule =
  { type_ : Type
  , position : Position
  , name : String
  , controls : Array Control
  , endpoints : Array Endpoint
  }

type alias Prototype =
  { type_ : Type
  , controls : Array Control
  , endpoints : Array Endpoint
  }

type Msg
  = EndpointDelegate Int Endpoint.Msg
  | ControlDelegate Int Control.Msg

type Type
  = ControllerModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule

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
  (Control -> (Control, Cmd msg))
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
    , viewEndpointBank Nothing prototype.endpoints Endpoint.In
    , viewControlBank Nothing prototype.controls
    , viewEndpointBank Nothing prototype.endpoints Endpoint.Out
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
    [ viewEndpointBank (Just delegate) audioModule.endpoints Endpoint.In
    , viewControlBank (Just delegate) audioModule.controls
    , viewEndpointBank (Just delegate) audioModule.endpoints Endpoint.Out
    ]

viewControlBank : Maybe (Msg -> msg) -> Array Control -> Html.Html msg
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

viewEndpointBank :
  Maybe (Msg -> msg)
  -> Array Endpoint
  -> Endpoint.Direction
  -> Html.Html msg
viewEndpointBank delegate endpoints direction =
  Html.div
    [ Attributes.class "endpoint-bank" ]
    ( ( Html.text <| case direction of
          Endpoint.In -> "in:"
          Endpoint.Out -> "out:"
      ) :: (
        endpoints
        |> Array.filter (\e -> e.direction == direction)
        |> Array.indexedMap (viewEndpoint delegate)
        |> Array.toList
      )
    )

viewEndpoint : Maybe (Msg -> msg) -> Int -> Endpoint -> Html.Html msg
viewEndpoint delegate index endpoint =
  case delegate of
    Nothing ->
      Endpoint.view Nothing endpoint
    Just d ->
      Endpoint.view (Just <| d << EndpointDelegate index) endpoint
