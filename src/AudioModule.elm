module AudioModule exposing
  ( AudioModule
  , Archetype
  , Msg(..)
  , Type(..)
  , init
  , initArchetype
  , at
  , translated
  , update
  , view
  , viewArchetype
  )

import Html
import Html.Attributes as Attributes
import Array exposing (Array)
import AudioModule.Control as Control
import AudioModule.Control exposing (Control)
import AudioModule.Endpoint as Endpoint
import AudioModule.Endpoint exposing (Endpoint)
import MouseEvent exposing (Position)

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init : (Msg -> msg) -> Archetype msg -> Int -> AudioModule msg
init delegate archetype index =
  let
    id = archetype.id ++ "-" ++ (String.fromInt index)
  in
    { id = id
    , delegate = delegate
    , position = (0, 0)
    , controls = initControls (Just delegate) archetype.type_ id
    , endpoints = initEndpoints (Just delegate) archetype.type_ id
    }

initArchetype : Type -> String -> Archetype msg
initArchetype type_ id =
  { id = id
  , type_ = type_
  , controls = initControls Nothing type_ id
  , endpoints = initEndpoints Nothing type_ id
  }

initControls : Maybe (Msg -> msg) -> Type -> String -> Array (Control msg)
initControls delegate type_ id =
  ( case type_ of
    ControllerModule ->
      [ ]
    DestinationModule ->
      [ ]
    ConstantModule ->
      [ Control.initKnob (id ++ "-knob") ]
    VCOModule ->
      [ Control.initControlGroup
        [ Control.initRadio "sine" id (id ++ "-radio-sin")
          |> Control.labeled "sin"
        , Control.initRadio "square" id (id ++ "-radio-sqr")
          |> Control.labeled "sqr"
        , Control.initRadio "sawtooth" id (id ++ "-radio-saw")
          |> Control.labeled "saw"
        , Control.initRadio "triangle" id (id ++ "-radio-tri")
          |> Control.labeled "tri"
        ]
        id
      ]
    VCAModule ->
      [ ]
    EnvelopeModule ->
      [ Control.initNumber (id ++ "-number-A")
        |> Control.labeled "A"
      , Control.initNumber (id ++ "-number-D")
        |> Control.labeled "D"
      , Control.initNumber (id ++ "-number-S")
        |> Control.labeled "S"
      , Control.initNumber (id ++ "-number-R")
        |> Control.labeled "R"
      ]
  )
  |> Array.fromList
  |> Array.indexedMap (initControlDelegate delegate)

initControlDelegate : Maybe (Msg -> msg) -> Int -> Control msg -> Control msg
initControlDelegate delegate index =
  case delegate of
    Nothing ->
      identity
    Just d ->
      Control.withDelegate (d << ControlDelegate index)

initEndpoints : Maybe (Msg -> msg) -> Type -> String -> Array (Endpoint msg)
initEndpoints delegate type_ name =
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
  )
  |> Array.fromList
  |> Array.indexedMap (initEndpointDelegate delegate)

initEndpointDelegate : Maybe (Msg -> msg) -> Int -> Endpoint msg -> Endpoint msg
initEndpointDelegate delegate index =
  case delegate of
    Nothing ->
      identity
    Just d ->
      Endpoint.withDelegate (d << EndpointDelegate index)

initEndpoint : String -> Endpoint.Direction -> String -> Endpoint msg
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
type alias AudioModule msg =
  { id : String
  , delegate : (Msg -> msg)
  , position : Position
  , controls : Array (Control msg)
  , endpoints : Array (Endpoint msg)
  }

type alias Archetype msg =
  { id : String
  , type_ : Type
  , controls : Array (Control msg)
  , endpoints : Array (Endpoint msg)
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

at : Position -> AudioModule msg -> AudioModule msg
at position audioModule =
    { audioModule | position = position }

translated : (Float, Float) -> AudioModule msg -> AudioModule msg
translated (dx, dy) audioModule =
  { audioModule
    | position = audioModule.position
    |> Tuple.mapBoth (\x -> x + dx) (\y -> y + dy)
  }

mapControlWithCmd :
  (Control msg -> (Control msg, Cmd msg))
  -> Int
  -> AudioModule msg
  -> (AudioModule msg, Cmd msg)
mapControlWithCmd transform index audioModule =
  case (Array.get index audioModule.controls) of
    Nothing ->
      ( audioModule, Cmd.none )
    Just control ->
      let
        ( ctrl, cmd ) = transform control
      in
        ( { audioModule | controls = Array.set index ctrl audioModule.controls }
        , cmd
        )

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> AudioModule msg -> (AudioModule msg, Cmd msg)
update msg audioModule =
  case msg of
    ControlDelegate index controlMsg ->
      mapControlWithCmd
        ( Control.update controlMsg )
        index
        audioModule
    _ ->
      ( audioModule, Cmd.none )

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
viewArchetype : List (Html.Attribute msg) -> Archetype msg -> Html.Html msg
viewArchetype extraAttributes prototype =
  Html.div
    ( Attributes.class "module-wrapper" :: extraAttributes )
    [ Html.div [ Attributes.class "prototype-click-shield"] []
    , viewEndpointBank prototype.endpoints Endpoint.In
    , viewControlBank prototype.controls
    , viewEndpointBank prototype.endpoints Endpoint.Out
    ]

view : List (Html.Attribute msg) -> AudioModule msg -> Html.Html msg
view extraAttributes audioModule =
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
  Html.div
    [ Attributes.class "endpoint-bank" ]
    ( ( Html.text <| case direction of
          Endpoint.In -> "in:"
          Endpoint.Out -> "out:"
      ) :: (
        endpoints
        |> Array.filter (\e -> e.direction == direction)
        |> Array.map Endpoint.view
        |> Array.toList
      )
    )
