module AudioModule exposing
  ( AudioModule
  , Endpoint
  , OperationTranslators
  , PrototypeTranslators
  , Position
  , Direction(..)
  , Msg(..)
  , Type(..)
  , init
  , initPrototype
  , at
  , translated
  , dragged
  , notDragged
  , opposite
  , oppositeOf
  , mapEndpoint
  , update
  , view
  , viewPrototype
  )

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Array exposing (Array)
import AudioModule.Control as Control
import AudioModule.Control exposing (Control)
import MouseEvent
import Svg.Attributes exposing (transform)

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init :  OperationTranslators msg -> Type -> String -> AudioModule msg
init translators =
  initGeneric <| Operation (0, 0) NotDragged translators

initPrototype : PrototypeTranslators msg -> Type -> String -> AudioModule msg
initPrototype translators type_ =
  initGeneric (Prototype type_ translators) type_

initGeneric : Mode msg -> Type -> String -> AudioModule msg
initGeneric mode type_ id =
  { id = id
  , mode = mode
  , controls = initControls type_ id mode
  , endpoints = initEndpoints type_ id
  }

initControls : Type -> String -> Mode msg -> Array (Control msg)
initControls type_ parentId =
  ( case type_ of
    ControllerModule ->
      [ ]
    DestinationModule ->
      [ ]
    ConstantModule ->
      [ Control.initKnob (parentId ++ "-knob") ]
    VCOModule ->
      [ ( Control.initControlGroup
          [ Control.initRadio "sine" parentId (parentId ++ "-radio-sin")
          |> Control.labeled "sin"
          , Control.initRadio "square" parentId (parentId ++ "-radio-sqr")
          |> Control.labeled "sqr"
          , Control.initRadio "sawtooth" parentId (parentId ++ "-radio-saw")
          |> Control.labeled "saw"
          , Control.initRadio "triangle" parentId (parentId ++ "-radio-tri")
          |> Control.labeled "tri"
          ]
          parentId
        )
      ]
    VCAModule ->
      [ ]
    EnvelopeModule ->
      [ Control.initNumber (parentId ++ "-number-A")
      |> Control.labeled "A"
      , Control.initNumber (parentId ++ "-number-D")
      |> Control.labeled "D"
      , Control.initNumber (parentId ++ "-number-S")
      |> Control.labeled "S"
      , Control.initNumber (parentId ++ "-number-R")
      |> Control.labeled "R"
      ]
  )
  |> Array.fromList
  |> initControlTranslators

initControlTranslators : Array (Control msg) -> Mode msg -> Array (Control msg)
initControlTranslators controls mode =
  case mode of
    Prototype _ _ ->
      controls
    Operation _ _ translators ->
      Array.indexedMap
        (\idx control ->
          Control.withTranslators
            { loopback = translators.loopback << (ControlDelegate idx)
            , input = translators.loopback << (Input idx)
            }
            control
        )
        controls

initEndpoints : Type -> String -> Array Endpoint
initEndpoints type_ parentId =
  let
    initialize direction label =
      { id = String.concat
      [ parentId
      , case direction of
        In -> "-endpoint-in-"
        Out -> "-endpoint-out-"
      , label
      ]
      , direction = direction
      , label = label
      , midpoint = ( 0, 0 )
      }
  in ( case type_ of
    ControllerModule ->
      [ initialize Out "freq"
      , initialize Out "gate"
      , initialize Out "trig"
      ]
    DestinationModule ->
      [ initialize In "signal"
      ]
    ConstantModule ->
      [ initialize Out "cv"
      ]
    VCOModule ->
      [ initialize In "freq"
      , initialize In "detune"
      , initialize Out "signal"
      ]
    VCAModule ->
      [ initialize In "signal"
      , initialize In "cv"
      , initialize Out "signal"
      ]
    EnvelopeModule ->
      [ initialize In "gate"
      , initialize In "trig"
      , initialize Out "cv"
      ]
  )
  |> Array.fromList

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias AudioModule msg =
  { id : String
  , mode : Mode msg
  , controls : Array (Control msg)
  , endpoints : Array Endpoint
  }

type Mode msg
  = Prototype Type (PrototypeTranslators msg)
  | Operation Position DragState (OperationTranslators msg)

type DragState = Dragged | NotDragged

type alias OperationTranslators msg =
  { loopback : Msg -> msg
  , startDrag : MouseEvent.MouseInfo -> msg
  , createHalfConnection : Int -> MouseEvent.MouseInfo -> msg
  , hoverEndpoint : Int -> msg
  , unhoverEndpoint : Int -> msg
  }

type alias PrototypeTranslators msg =
  { createAudioModule : Type -> String -> MouseEvent.MouseInfo -> msg
  }

type Msg
  = ControlDelegate Int Control.Msg
  | Input Int String

type Type
  = ControllerModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule

type alias Endpoint =
  { id : String
  , direction : Direction
  , label : String
  , midpoint : Position
  }

type Direction = In | Out

type alias Position = (Float, Float)

dragged : AudioModule msg -> AudioModule msg
dragged audioModule =
  case audioModule.mode of
    Operation position _ messages ->
      { audioModule | mode = Operation position Dragged messages }
    _ ->
      audioModule


notDragged : AudioModule msg -> AudioModule msg
notDragged audioModule =
  case audioModule.mode of
    Operation position _ messages ->
      { audioModule | mode = Operation position NotDragged messages }
    _ ->
      audioModule

at : Position -> AudioModule msg -> AudioModule msg
at position audioModule =
  case audioModule.mode of
    Operation _ dragState messages ->
      { audioModule | mode = Operation position dragState messages }
    _ ->
      audioModule

translated : (Float, Float) -> AudioModule msg -> AudioModule msg
translated (dx, dy) audioModule =
  case audioModule.mode of
    Operation position dragState messages ->
      { audioModule
        | mode = Operation
          (Tuple.mapBoth (\x -> x + dx) (\y -> y + dy) position)
          dragState
          messages
      }
    _ ->
      audioModule

opposite : Direction -> Direction
opposite direction =
  case direction of
    In -> Out
    Out -> In

oppositeOf : Endpoint -> Direction
oppositeOf { direction } =
  opposite direction

mapEndpoint : ( Endpoint -> Endpoint ) -> Int -> AudioModule msg -> AudioModule msg
mapEndpoint transform index audioModule =
  case (Array.get index audioModule.endpoints) of
    Nothing ->
      audioModule
    Just endpoint ->
      { audioModule
      | endpoints = Array.set index ( transform endpoint) audioModule.endpoints
      }

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> AudioModule msg -> (AudioModule msg, Cmd msg)
update msg audioModule =
  case audioModule.mode of
    Prototype _ _ ->
      ( audioModule, Cmd.none )
    Operation _ _ _ ->
      case msg of
        ControlDelegate index controlMsg ->
          updateControl index controlMsg audioModule
        Input index value ->
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
view : AudioModule msg -> Html.Html msg
view audioModule =
  case audioModule.mode of
    Prototype type_ msgs ->
      viewPrototype type_ msgs audioModule
    Operation position dragState msgs ->
      viewOperational position dragState msgs audioModule

viewPrototype :
  Type
  -> PrototypeTranslators msg
  -> AudioModule msg
  -> Html.Html msg
viewPrototype type_ translators prototype =
  Html.div
    [ Attributes.class "audio-module"
    , Attributes.id (prototype.id ++ "-proto")
    ]
    [ Html.div
      [ Attributes.class "prototype-click-shield"
      , MouseEvent.onCustom
        "mousedown"
        ( translators.createAudioModule type_ prototype.id )
      ]
      [ ]
    , viewEndpointBank Nothing In prototype.endpoints
    , viewControlBank prototype.controls
    , viewEndpointBank Nothing Out prototype.endpoints
    ]

viewOperational :
  Position
  -> DragState
  -> OperationTranslators msg
  -> AudioModule msg
  -> Html.Html msg
viewOperational position dragState translators audioModule =
  let
    pxFromFloat = \float -> ( String.fromInt << round <| float ) ++ "px"
    ( xpx, ypx ) = Tuple.mapBoth pxFromFloat pxFromFloat position
  in
    Html.div
    [ Attributes.style "left" xpx
    , Attributes.style "top" ypx
    , Attributes.classList
      [ ("audio-module", True)
      , ( "grabbing"
        , case dragState of
          Dragged -> True
          NotDragged -> False
        )
      ]
    , Attributes.id audioModule.id
    , MouseEvent.onCustom "mousedown" translators.startDrag
    ]
    [ viewEndpointBank (Just translators) In audioModule.endpoints
    , viewControlBank audioModule.controls
    , viewEndpointBank (Just translators) Out audioModule.endpoints
    ]

viewControlBank : Array (Control msg) -> Html.Html msg
viewControlBank controls =
  Html.div
    [ Attributes.class "control-bank" ]
    ( Array.map Control.view controls |> Array.toList )

viewEndpointBank :
  Maybe (OperationTranslators msg)
  -> Direction
  -> Array Endpoint
  -> Html.Html msg
viewEndpointBank translators direction endpoints =
  Array.toIndexedList endpoints
  |> List.filter (\(_, e) -> e.direction == direction)
  |> List.map (viewEndpoint translators)
  |> \elements -> Html.div
    [ Attributes.class "endpoint-bank" ]
    ( if List.isEmpty elements
      then []
      else
        ( Html.text <| case direction of
            In -> "in:"
            Out -> "out:"
        ) :: elements
    )

viewEndpoint :
  Maybe (OperationTranslators msg)
  -> (Int, Endpoint)
  -> Html.Html msg
viewEndpoint translators (index, endpoint) =
  Html.div
    [ Attributes.class "endpoint-wrapper" ]
    [ Html.div
      ( translators
      |> Maybe.map
        (\{ createHalfConnection, hoverEndpoint, unhoverEndpoint } ->
          [ MouseEvent.onCustom "mousedown" ( createHalfConnection index )
          , Events.onMouseEnter ( hoverEndpoint index )
          , Events.onMouseLeave ( unhoverEndpoint index )
          ]
        )
      |> Maybe.withDefault []
      |> List.append
        [ Attributes.class "endpoint-jack"
        , Attributes.class "grabbable"
        , Attributes.id endpoint.id
        ]
      )
      [ ]
    , Html.label
      [ Attributes.class "endpoint-label" ]
      [ Html.text endpoint.label ]
    ]
