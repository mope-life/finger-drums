module AudioModuleMap exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html
import Html.Attributes as Attributes
import Svg
import Svg.Attributes
import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)
import Task
import MouseEvent
import AudioModule exposing (AudioModule, Msg(..), Type(..))

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
      [ AudioModule.initPrototype prototypeTranslators ConstantModule "const"
      , AudioModule.initPrototype prototypeTranslators VCOModule "osc"
      , AudioModule.initPrototype prototypeTranslators VCAModule "amp"
      , AudioModule.initPrototype prototypeTranslators EnvelopeModule "env"
      ]
    , dragState = Nothing
    , nextId = 0
    , lines = []
    , hovered = Set.empty
    }
    , Cmd.none
  )

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Model =
  { audioModules : Dict Id (AudioModule Msg)
  , prototypes : List (AudioModule Msg)
  , dragState : Maybe DragState
  , nextId : Id
  , lines : List Line
  , hovered : Set (Id, Int)
  }

type Msg
  = CreateAudioModule AudioModule.Type String MouseEvent.MouseInfo
  | CreateHalfConnection Id Int MouseEvent.MouseInfo
  | SnapAndStartDrag HalfConnection MouseEvent.MouseInfo (Result Dom.Error Dom.Element)
  | StartDrag Draggable MouseEvent.MouseInfo
  | ContinueDrag MouseEvent.MouseInfo
  | EndDrag MouseEvent.MouseInfo
  | AudioModuleDelegate Id AudioModule.Msg
  | HoverEndpoint Id Int
  | UnhoverEndpoint Id Int

type alias DragState =
  { dragged : Draggable
  , lastPoint : Point
  }

type Draggable
  = AudioModuleId Id
  | NewConnection HalfConnection

type alias HalfConnection =
  { id : Id
  , index : Int
  }

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

withDragged : Draggable -> Point -> Model -> Model
withDragged draggable start model =
  { model
  | dragState = Just { dragged = draggable, lastPoint = start }
  }
  |> case draggable of
    AudioModuleId id ->
      mapAudioModule AudioModule.dragged id
    NewConnection _ ->
      identity

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

audioModuleTranslators : Id -> AudioModule.OperationTranslators Msg
audioModuleTranslators id =
  { loopback = AudioModuleDelegate id
  , startDrag = StartDrag (AudioModuleId id)
  , createHalfConnection = CreateHalfConnection id
  , hoverEndpoint = HoverEndpoint id
  , unhoverEndpoint = UnhoverEndpoint id
  }

prototypeTranslators : AudioModule.PrototypeTranslators Msg
prototypeTranslators =
  { createAudioModule = CreateAudioModule
  }

endpointAt : Id -> Int -> Model -> Maybe AudioModule.Endpoint
endpointAt id index model =
  Dict.get id model.audioModules
  |> Maybe.map (\am -> am.endpoints)
  |> Maybe.andThen (Array.get index)

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
    CreateHalfConnection id index mouseInfo ->
      case endpointAt id index model of
        Nothing ->
          ( model, Cmd.none )
        Just endpoint ->
          ( model
          , Task.attempt
            ( SnapAndStartDrag { id = id , index = index } mouseInfo )
            ( Dom.getElement endpoint.id )
          )
    SnapAndStartDrag halfConnection mouseInfo result ->
      case result of
        Err _ ->
          ( model, Cmd.none )
        Ok element ->
          snapAndStartDrag halfConnection element mouseInfo model
    StartDrag draggable mouseInfo ->
      onStartDrag draggable mouseInfo model
    ContinueDrag mouseInfo ->
      onContinueDrag mouseInfo model
    EndDrag mouseInfo ->
      onEndDrag mouseInfo model
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
    HoverEndpoint id index ->
      ( { model | hovered = Set.insert (id, index) model.hovered }, Cmd.none )
    UnhoverEndpoint id index ->
      ( { model | hovered = Set.remove (id, index) model.hovered }, Cmd.none )

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
    ( audioModuleTranslators id )
    type_
    ( String.concat [htmlId, "-", String.fromInt id ] )
  |> AudioModule.at start

snapAndStartDrag :
  HalfConnection
  -> Dom.Element
  -> MouseEvent.MouseInfo
  -> Model
  -> ( Model, Cmd Msg )
snapAndStartDrag halfConnection { element } mouseInfo model =
  let
    xMid = element.x + (element.width / 2)
    yMid = element.y + (element.height / 2)
    line = createLine (xMid, yMid)
  in
    ( { model | lines = line :: model.lines }
    , Task.perform
      ( StartDrag <| NewConnection halfConnection )
      ( Task.succeed mouseInfo )
    )

insertAndStartDrag :
  AudioModule Msg
  -> MouseEvent.MouseInfo
  -> Model
  -> (Model, Cmd Msg)
insertAndStartDrag audioModule mouseInfo model =
  ( model
    |> with model.nextId audioModule
    |> withNextId
  , Task.perform
    (StartDrag <| AudioModuleId model.nextId)
    (Task.succeed mouseInfo)
  )

onStartDrag : Draggable -> MouseEvent.MouseInfo -> Model -> ( Model, Cmd msg )
onStartDrag draggable mouseInfo model =
  ( withDragged draggable (point mouseInfo) model, Cmd.none )

onContinueDrag : MouseEvent.MouseInfo -> Model -> ( Model, Cmd msg )
onContinueDrag mouseInfo model =
  case model.dragState of
    Nothing ->
      ( model, Cmd.none )
    Just { dragged, lastPoint } ->
      case dragged of
        AudioModuleId id ->
          ( dragAudioModule mouseInfo lastPoint id model, Cmd.none )
        NewConnection _ ->
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
    ( List.map AudioModule.view model.prototypes )

viewConnectionMap : Model -> Html.Html Msg
viewConnectionMap model =
  Svg.svg [ Attributes.id "connection-map" ] ( List.map viewLine model.lines )

viewLine : Line -> Html.Html Msg
viewLine { endOne, endTwo } =
  Svg.line
    [ Svg.Attributes.strokeLinecap "round"
    , Svg.Attributes.x1 (String.fromFloat << Tuple.first <| endOne)
    , Svg.Attributes.y1 (String.fromFloat << Tuple.second <| endOne)
    , Svg.Attributes.x2 (String.fromFloat << Tuple.first <| endTwo)
    , Svg.Attributes.y2 (String.fromFloat << Tuple.second <| endTwo)
    ]
    []

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
