module AudioModule exposing
  ( AudioModule
  , Prototype
  , Msg
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
import AudioModule.Control as Control
import Array exposing (Array)

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
      Array.fromList
        [ Control.knobControl ("knob-" ++ name) ]
    VCOModule ->
      Array.fromList
        [ Control.radioControl ("radio-sin-" ++ name) ("osc-" ++ name) "sine"
          |> Control.labeled "sin"
          |> Control.checked
        , Control.radioControl ("radio-sqr-" ++ name) ("osc-" ++ name) "square"
          |> Control.labeled "sqr"
        , Control.radioControl ("radio-saw-" ++ name) ("osc-" ++ name) "sawtooth"
          |> Control.labeled "saw"
        , Control.radioControl ("radio-tri-" ++ name) ("osc-" ++ name) "triangle"
          |> Control.labeled "tri"
        ]
    VCAModule ->
      Array.empty
    EnvelopeModule ->
      Array.fromList
        [ Control.numberControl ("number-A-" ++ name)
          |> Control.labeled "A"
        , Control.numberControl ("number-D-" ++ name)
          |> Control.labeled "D"
        , Control.numberControl ("number-S-" ++ name)
          |> Control.labeled "S"
        , Control.numberControl ("number-R-" ++ name)
          |> Control.labeled "R"
        ]

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
  = EndpointClickedPlaceholder
  | Delegate Int Control.Msg

type Type
  = ControllerModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule

type alias Position = (Float, Float)

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
    EndpointClickedPlaceholder ->
      Debug.log "caught endpoint placeholder" (audioModule, Cmd.none)
    Delegate index controlMsg ->
      mapControlWithCmd
        (Control.update (delegate << Delegate index) controlMsg)
        index
        audioModule

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
viewPrototype : List (Html.Attribute msg) -> Prototype -> Html.Html msg
viewPrototype extraAttributes prototype =
  Html.div
    ( Attributes.class "module-wrapper" :: extraAttributes )
    [ Html.div [ Attributes.class "prototype-click-shield"] []
    , viewEndpointsIn Nothing prototype.type_
    , viewControls Nothing prototype.controls
    , viewEndpointsOut Nothing prototype.type_
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
    [ viewEndpointsIn (Just delegate) audioModule.type_
    , viewControls (Just delegate) audioModule.controls
    , viewEndpointsOut (Just delegate) audioModule.type_
    ]

viewControls : Maybe (Msg -> msg) -> Array Control.Control -> Html.Html msg
viewControls maybeDelegate controls =
  let
    controlView = case maybeDelegate of
      Nothing ->
        (\_ control ->
          Control.view Nothing control)
      Just delegate ->
        (\index control ->
          Control.view (Just <| delegate << Delegate index) control)
  in
    Html.div
      [ Attributes.class "control-bank" ]
      ( Array.indexedMap controlView controls |> Array.toList )

viewEndpointsIn : Maybe (Msg -> msg) -> Type -> Html.Html msg
viewEndpointsIn delegate type_ =
  let
    elements = case type_ of
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
    div = Html.div [ Attributes.class "endpoint-bank" ]
  in
    if List.isEmpty elements
    then div []
    else div <| Html.text "in:" :: elements

viewEndpointsOut : Maybe (Msg -> msg) -> Type -> Html.Html msg
viewEndpointsOut delegate type_ =
  let
    elements = case type_ of
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
    div = Html.div [ Attributes.class "endpoint-bank" ]
  in
    if List.isEmpty elements
    then div []
    else div <| Html.text "out:" :: elements

viewEndpoint : Maybe (Msg -> msg) -> String -> Html.Html msg
viewEndpoint maybeDelegate label =
  let
    events = case maybeDelegate of
      Nothing ->
        []
      Just delegate ->
        [ Events.onClick (delegate EndpointClickedPlaceholder) ]
  in
    Html.div
      ( Attributes.class "endpoint-wrapper" :: events )
      [ Html.div [ Attributes.class "endpoint-jack", Attributes.class "grabbable" ] []
      , Html.label [ Attributes.class "endpoint-label" ] [ Html.text label ]
      ]
