module AudioModuleMap exposing (main)

import Browser
import Html
import Html.Attributes as Attributes
import Svg
import Svg.Attributes
import Dict exposing (Dict)
import Browser.Events
import Task
import MouseEvent
import AudioModule exposing (AudioModule, Msg(..), Type(..))
import AudioModule.Endpoint exposing (Endpoint)

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : () -> ( Model, Cmd Msg )
init _ =
  ( { audioModules = Dict.empty
    , prototypes =
      [ initPrototype ConstantModule "const"
      , initPrototype VCOModule "osc"
      , initPrototype VCAModule "amp"
      , initPrototype EnvelopeModule "env"
      ]
    , dragState = Nothing
    , nextId = 0
    , lines = []
    , hovered = []
    }
    , Cmd.none
  )

initPrototype : AudioModule.Type -> String -> AudioModule.Prototype Msg
initPrototype type_ id =
  AudioModule.initPrototype (prototypeTranslators type_ id) type_ id

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Model =
  { audioModules : Dict Id (AudioModule Msg)
  , dragState : Maybe DragState
  , prototypes : List (AudioModule.Prototype Msg)
  , nextId : Id
  , lines : List Line
  , hovered : List (Endpoint Msg)
  }

type Msg
  = CreateAudioModule AudioModule.Type String MouseEvent.MouseInfo
  | CreateHalfConnection MouseEvent.MouseInfo
  | StartDrag Draggable MouseEvent.MouseInfo
  | ContinueDrag MouseEvent.MouseInfo
  | EndDrag MouseEvent.MouseInfo
  | AudioModuleDelegate Id AudioModule.Msg
  | IgnoreMouseEvent MouseEvent.MouseInfo
  | IgnoreMsg AudioModule.Msg

type alias DragState =
  { dragged : Draggable
  , lastPoint : Point
  }

type Draggable
  = AudioModuleId Id
  | HalfConnection

type alias Line =
  { endOne : Point
  , endTwo : Point
  }

type alias Id = Int

type alias Point = (Float, Float)

type alias Delta = (Float, Float)

with : Id -> (AudioModule Msg) -> Model -> Model
with id audioModule model =
  { model | audioModules = model.audioModules |> Dict.insert id audioModule }

without : Id -> Model -> Model
without id model =
  { model | audioModules = model.audioModules |> Dict.remove id }

withDraggedId : Id -> Point -> Model -> Model
withDraggedId id start model =
  { model
  | dragState = Just { dragged = (AudioModuleId id), lastPoint = start }
  }
  |> mapAudioModule AudioModule.dragged id

withDraggedLine : Point -> Model -> Model
withDraggedLine start model =
  { model
  | dragState = Just { dragged = HalfConnection, lastPoint = start }
  }

withNothingDragged : Model -> Model
withNothingDragged model =
  { model | dragState = Nothing }
  |> case model.dragState of
    Nothing ->
      identity
    Just { dragged } ->
      case dragged of
        AudioModuleId id ->
          mapAudioModule AudioModule.notDragged id
        _ ->
          identity

withNextId : Model -> Model
withNextId model =
  { model | nextId = model.nextId + 1 }

mapAudioModule : (AudioModule Msg -> AudioModule Msg) -> Id -> Model -> Model
mapAudioModule transform id model =
  case (Dict.get id model.audioModules) of
    Nothing ->
      model
    Just audioModule ->
      with id (transform audioModule) model

--------------------------------------------------------------------------------
-- Subscriptions ---------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions { dragState } =
  case dragState of
    Nothing ->
      Sub.none
    Just _ ->
      Sub.batch
        [ ContinueDrag
          |> Browser.Events.onMouseMove << MouseEvent.messageDecoder
        , EndDrag
          |> Browser.Events.onMouseUp << MouseEvent.messageDecoder
        ]

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateAudioModule type_ htmlId mouseInfo ->
      insertAndStartDrag
        ( newAudioModule model.nextId type_ htmlId (position mouseInfo) )
        mouseInfo
        model
    CreateHalfConnection mouseInfo ->
      createHalfConnection mouseInfo model
    StartDrag draggable mouseInfo ->
      onStartDrag draggable mouseInfo model
    ContinueDrag mouseInfo ->
      onContinueDrag mouseInfo model
    EndDrag mouseInfo ->
      onEndDrag mouseInfo model
    IgnoreMouseEvent _ ->
      ( model, Cmd.none )
    IgnoreMsg _ ->
      ( model, Cmd.none )
    AudioModuleDelegate id audioModuleMsg ->
      case (Dict.get id model.audioModules) of
        Nothing ->
          ( model, Cmd.none )
        Just audioModule ->
          let
            ( newModule, cmd ) = AudioModule.update audioModuleMsg audioModule
          in
            ( { model
              | audioModules = Dict.insert id newModule model.audioModules
              }
            , cmd
            )

translators : Id -> AudioModule.Translators Msg
translators id =
  { loopback = AudioModuleDelegate id
  , mouseDown = StartDrag (AudioModuleId id)
  , endpointTranslators =
    { mouseDown = CreateHalfConnection
    }
  }

prototypeTranslators : AudioModule.Type -> String -> AudioModule.Translators Msg
prototypeTranslators type_ id =
  { loopback = IgnoreMsg
  , mouseDown = CreateAudioModule type_ id
  , endpointTranslators =
    { mouseDown = IgnoreMouseEvent
    }
  }

position : MouseEvent.MouseInfo -> AudioModule.Position
position { pageX, pageY, offsetX, offsetY} =
  ( pageX - offsetX
  , pageY - offsetY
  )

point : MouseEvent.MouseInfo -> Point
point { pageX, pageY} =
  ( pageX, pageY )

delta : Point -> Point -> Delta
delta (x1, y1) (x2, y2) =
  (x2 - x1, y2 - y1)

newAudioModule : Id -> Type -> String -> AudioModule.Position -> AudioModule Msg
newAudioModule id type_ htmlId start =
  AudioModule.init
    ( translators id )
    type_
    ( String.concat [htmlId, "-", String.fromInt id ] )
  |> AudioModule.at start

insertAndStartDrag :
  AudioModule Msg
  -> MouseEvent.MouseInfo
  -> Model
  -> (Model, Cmd Msg)
insertAndStartDrag audioModule mouseInfo model =
  ( model
    |> with model.nextId audioModule
    |> withNextId
  , Task.perform audioModule.translators.mouseDown (Task.succeed mouseInfo)
  )

createHalfConnection : MouseEvent.MouseInfo -> Model -> (Model, Cmd Msg)
createHalfConnection mouseInfo model =
  let
    line = createLine (point mouseInfo)
  in
  ( { model | lines = line :: model.lines }
  , Task.perform (StartDrag HalfConnection) (Task.succeed mouseInfo)
  )

onStartDrag : Draggable -> MouseEvent.MouseInfo -> Model -> ( Model, Cmd msg )
onStartDrag draggable mouseInfo model =
  case draggable of
    AudioModuleId id ->
      ( withDraggedId id (point mouseInfo) model, Cmd.none )
    HalfConnection ->
      ( withDraggedLine (point mouseInfo) model, Cmd.none )

onContinueDrag : MouseEvent.MouseInfo -> Model -> ( Model, Cmd msg )
onContinueDrag mouseInfo model =
  case model.dragState of
    Nothing ->
      ( model, Cmd.none )
    Just { dragged, lastPoint } ->
      case dragged of
        AudioModuleId id ->
          ( dragAudioModule mouseInfo lastPoint id model, Cmd.none )
        HalfConnection ->
          ( dragLine mouseInfo model, Cmd.none )

dragAudioModule : MouseEvent.MouseInfo -> Point -> Id -> Model -> Model
dragAudioModule mouseInfo lastPoint id model =
  { model
  | dragState = Just
    { dragged = AudioModuleId id
    , lastPoint = point mouseInfo
    }
  }
  |> mapAudioModule
    (AudioModule.translated <| delta lastPoint (point mouseInfo))
    id

dragLine : MouseEvent.MouseInfo -> Model -> Model
dragLine mouseInfo model =
  case (List.head model.lines) of
    Nothing ->
      model
    Just line ->
      { model
      | lines = ( adjustLine (point mouseInfo) line ) :: cdr model.lines
      }

onEndDrag : MouseEvent.MouseInfo -> Model -> ( Model, Cmd msg )
onEndDrag mouseInfo model =
  ( { model | lines = cdr model.lines }
    |> withNothingDragged
  , Cmd.none
  )

createLine : Point -> Line
createLine start =
  { endOne = start, endTwo = start }

adjustLine : Point -> Line -> Line
adjustLine end line =
  { line | endTwo = end }

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Model -> Html.Html Msg
view model =
  Html.div
    [ Attributes.id "audio-module-map" ]
    ( viewPrototypeBank model
      :: viewConnectionMap model
      :: viewAudioModules model
    )

viewPrototypeBank : Model -> Html.Html Msg
viewPrototypeBank model =
  Html.div
    [ Attributes.id "prototype-bank" ]
    <| List.map AudioModule.viewPrototype model.prototypes

viewConnectionMap : Model -> Html.Html Msg
viewConnectionMap model =
  Svg.svg
    [ Attributes.id "connection-map" ]
    ( List.map
      (\{ endOne, endTwo } ->
          Svg.line
            [ Svg.Attributes.strokeLinecap "round"
            , Svg.Attributes.x1 (String.fromFloat << Tuple.first <| endOne)
            , Svg.Attributes.y1 (String.fromFloat << Tuple.second <| endOne)
            , Svg.Attributes.x2 (String.fromFloat << Tuple.first <| endTwo)
            , Svg.Attributes.y2 (String.fromFloat << Tuple.second <| endTwo)
            ]
            []
      )
      model.lines
    )

viewAudioModules : Model -> List (Html.Html Msg)
viewAudioModules model =
  model.audioModules
  |> Dict.values
  |> List.map AudioModule.view

--------------------------------------------------------------------------------
-- Utility ---------------------------------------------------------------------
cdr : List a -> List a
cdr list =
  Maybe.withDefault [] ( List.tail list )
