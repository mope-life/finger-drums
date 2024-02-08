module AudioModuleMap exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html
import Html.Attributes as Attributes
import Svg
import Svg.Attributes
import Json.Decode as Decode
import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)
import Task
import MouseEvent
import AudioModule exposing (AudioModule, PrototypeModule, Type(..), Mode(..))
import AudioModule.Translators exposing (Translators)
import AudioModule.Endpoint exposing (Endpoint)
import Utility exposing (..)

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
  let
    initialModules =
      [ createFixedModule KeyboardModule
      ]
      |> List.indexedMap (\i f -> (i, f i) )
      |> Dict.fromList
    prototypes =
      [ createPrototypeModule ConstantModule
      , createPrototypeModule VCOModule
      , createPrototypeModule VCAModule
      , createPrototypeModule EnvelopeModule
      ]
      |> Array.fromList
      |> Array.indexedMap (\i f -> f i )
    model =
      { audioModules = initialModules
      , prototypes = prototypes
      , connections = []
      , dragState = Nothing
      , hovered = Set.empty
      , nextId = Dict.size initialModules
      }
    cmd = initializeEndpoints model
  in
    ( model, cmd )

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Model =
  { audioModules : Dict Id AudioModule
  , prototypes : Array PrototypeModule
  , connections : List Connection
  , dragState : Maybe DragState
  , hovered : Set EndpointId
  , nextId : Id
  }

type Msg
  = CreateAudioModule Type MouseEvent.MouseInfo
  | CreateHalfConnection Id Int MouseEvent.MouseInfo
  | StartDrag Draggable MouseEvent.MouseInfo
  | ContinueDrag MouseEvent.MouseInfo
  | EndDrag
  | HoverEndpoint Id Int
  | UnhoverEndpoint Id Int
  | UpdateEndpointCoordinates EndpointId (Result Dom.Error Dom.Element)
  | ControlClick String
  | ControlInput Id Int String
  | FocusResult (Result Dom.Error ())

type alias DragState =
  { dragged : Draggable
  , lastPoint : Vec2
  }

type Draggable
  = AudioModuleId Id
  | HalfConnection EndpointId Line

type alias Connection =
  { idIn : EndpointId
  , idOut : EndpointId
  }

type alias Line =
  { endOne : Vec2
  , endTwo : Vec2
  }

type alias Id = Int

type alias EndpointId = ( Id, Int )

type alias EndpointIdPair =
  { id : EndpointId
  , endpoint : Endpoint
  }

with : Id -> AudioModule -> Model -> Model
with id audioModule model =
  { model | audioModules = model.audioModules |> Dict.insert id audioModule }

without : Id -> Model -> Model
without id model =
  { model | audioModules = model.audioModules |> Dict.remove id }

withDragged : Draggable -> Vec2 -> Model -> Model
withDragged draggable start model =
  { model
  | dragState = Just { dragged = draggable, lastPoint = start }
  }
  |> case draggable of
    AudioModuleId id -> mapAudioModule AudioModule.dragged id
    HalfConnection _ _ -> identity

withNothingDragged : Model -> Model
withNothingDragged model =
  { model | dragState = Nothing }
  |> case model.dragState of
    Nothing -> identity
    Just { dragged } -> case dragged of
      AudioModuleId id -> mapAudioModule AudioModule.notDragged id
      _ -> identity

withNextId : Model -> Model
withNextId model =
  { model | nextId = model.nextId + 1 }

withHovered : EndpointId -> Model -> Model
withHovered endpointId model =
  { model | hovered = Set.insert endpointId model.hovered }

withoutHovered : EndpointId -> Model -> Model
withoutHovered endpointId model =
  { model | hovered = Set.remove endpointId model.hovered }

mapAudioModule : (AudioModule -> AudioModule) -> Id -> Model -> Model
mapAudioModule transform id model =
  case (Dict.get id model.audioModules) of
    Nothing -> model
    Just audioModule -> with id (transform audioModule) model

mapEndpoint : (Endpoint -> Endpoint) -> EndpointId -> Model -> Model
mapEndpoint transform ( id, index ) =
  mapAudioModule ( AudioModule.mapEndpoint transform index ) id

endpointAt : EndpointId -> Model -> Maybe Endpoint
endpointAt (id, index) model =
  Dict.get id model.audioModules
  |> Maybe.map ( \am -> am.endpoints )
  |> Maybe.andThen ( Array.get index )

--------------------------------------------------------------------------------
-- Subscriptions ---------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions { dragState } =
  case dragState of
    Nothing -> Sub.none
    Just _ -> Sub.batch
      [ ContinueDrag
      |> Browser.Events.onMouseMove << MouseEvent.messageDecoder
      , EndDrag
      |> Browser.Events.onMouseUp << Decode.succeed
      ]

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateAudioModule type_ mouseInfo ->
      insertAndStartDrag
        ( createFloatingModule (positionFromMouseInfo mouseInfo) type_ model.nextId )
        mouseInfo
        model
    CreateHalfConnection id index mouseInfo ->
      snapAndStartDrag ( id, index ) mouseInfo model
    StartDrag draggable mouseInfo ->
      onStartDrag draggable ( mousePoint mouseInfo ) model
    ContinueDrag mouseInfo ->
      onContinueDrag ( mousePoint mouseInfo ) model
    EndDrag ->
      onEndDrag model
    HoverEndpoint id index ->
      ( withHovered ( id, index ) model, Cmd.none )
    UnhoverEndpoint id index ->
      ( withoutHovered ( id, index ) model, Cmd.none )
    UpdateEndpointCoordinates endpointId result ->
      case result of
        Err _ -> ( model, Cmd.none )
        Ok domElement -> updateEndpointCoordinate domElement endpointId model
    ControlClick htmlId ->
      ( model
      , Task.attempt FocusResult ( Dom.focus htmlId )
      )
    ControlInput id index value ->
      ( mapAudioModule (AudioModule.updateControlValue index value) id model
      , Cmd.none
      )
    FocusResult _ ->
      ( model, Cmd.none )

initializeEndpoints : Model -> Cmd Msg
initializeEndpoints model =
  model.audioModules
  |> Dict.toList
  |> List.map ( apply2 fetchAudioModuleEndpointMidpoints )
  |> Cmd.batch

fetchAudioModuleEndpointMidpoints : Id -> AudioModule -> Cmd Msg
fetchAudioModuleEndpointMidpoints id audioModule =
  audioModule.endpoints
  |> Array.indexedMap ( fetchEndpointMidpoint id )
  |> Array.toList
  |> Cmd.batch

fetchEndpointMidpoint : Id -> Int -> Endpoint -> Cmd Msg
fetchEndpointMidpoint id index endpoint =
  Task.attempt
    ( UpdateEndpointCoordinates ( id, index ) )
    ( Dom.getElement endpoint.htmlId )

positionFromMouseInfo : MouseEvent.MouseInfo -> Vec2
positionFromMouseInfo { clientX, clientY, offsetX, offsetY } =
  ( clientX - offsetX
  , clientY - offsetY
  )

dimensionsFromMouseInfo : MouseEvent.MouseInfo -> Vec2
dimensionsFromMouseInfo { targetWidth, targetHeight } =
  ( targetWidth, targetHeight )

midpointFromMouseInfo : MouseEvent.MouseInfo -> Vec2
midpointFromMouseInfo mouseInfo =
  midpointOf mouseInfo positionFromMouseInfo dimensionsFromMouseInfo

positionFromDomElement : Dom.Element -> Vec2
positionFromDomElement domElement =
  ( domElement.element.x, domElement.element.y )

dimensionsFromDomElement : Dom.Element -> Vec2
dimensionsFromDomElement domElement =
  ( domElement.element.width, domElement.element.height )

midpointFromDomElement : Dom.Element -> Vec2
midpointFromDomElement domElement =
  midpointOf domElement positionFromDomElement dimensionsFromDomElement

midpointOf : t -> ( t -> Vec2 ) -> ( t -> Vec2 ) -> Vec2
midpointOf thing getPosition getDimensions =
  let
    ( x, y ) = ( getPosition thing )
    ( width, height ) = ( getDimensions thing )
  in
    ( x + width / 2, y + height / 2 )

mousePoint : MouseEvent.MouseInfo -> Vec2
mousePoint { clientX, clientY } =
  ( clientX, clientY )

delta : Vec2 -> Vec2 -> Vec2
delta (x1, y1) (x2, y2) =
  (x2 - x1, y2 - y1)

createPrototypeModule : Type -> Id -> PrototypeModule
createPrototypeModule type_ id =
  AudioModule.initPrototype type_ ( "proto-" ++ ( String.fromInt id ) )

createFloatingModule : Vec2 -> Type -> Id -> AudioModule
createFloatingModule start type_=
  AudioModule.init
    ( Floating { dragState = AudioModule.NotDragged , position = start } )
    type_
    << createId

createFixedModule : Type -> Id -> AudioModule
createFixedModule type_ =
  AudioModule.init Fixed type_ << createId

createId : Id -> String
createId id =
  "module-" ++ ( String.fromInt id )

createTranslators : Id -> Translators Msg
createTranslators id =
  { startDrag = StartDrag (AudioModuleId id)
  , createHalfConnection = CreateHalfConnection id
  , hoverEndpoint = HoverEndpoint id
  , unhoverEndpoint = UnhoverEndpoint id
  , controlClick = ControlClick
  , controlInput = ControlInput id
  }

snapAndStartDrag : EndpointId -> MouseEvent.MouseInfo -> Model -> ( Model, Cmd Msg )
snapAndStartDrag endpointId mouseInfo =
  onStartDrag
    ( HalfConnection
      endpointId
      ( createLine ( midpointFromMouseInfo mouseInfo ) )
    )
    ( mousePoint mouseInfo )

insertAndStartDrag : AudioModule -> MouseEvent.MouseInfo -> Model -> (Model, Cmd Msg)
insertAndStartDrag audioModule mouseInfo model =
  onStartDrag
    ( AudioModuleId model.nextId )
    ( mousePoint mouseInfo )
    ( model
    |> with model.nextId audioModule
    |> withNextId
    )

onStartDrag : Draggable -> Vec2 -> Model -> ( Model, Cmd Msg )
onStartDrag draggable pnt model =
  ( withDragged draggable pnt model, Cmd.none )

onContinueDrag : Vec2 -> Model -> ( Model, Cmd Msg )
onContinueDrag pnt model =
  case model.dragState of
    Nothing -> ( model, Cmd.none )
    Just { dragged, lastPoint } -> case dragged of
      AudioModuleId id ->
        dragAudioModule pnt lastPoint id model
      HalfConnection halfConnection line ->
        dragLine pnt halfConnection line model

dragAudioModule : Vec2 -> Vec2 -> Id -> Model -> ( Model, Cmd Msg )
dragAudioModule thisPoint lastPoint id model =
  ( { model
    | dragState = Just
      { dragged = AudioModuleId id
      , lastPoint = thisPoint
      }
    }
    |> mapAudioModule
      (AudioModule.translated <| delta lastPoint thisPoint )
      id
  , Dict.get id model.audioModules
    |> Maybe.map ( fetchAudioModuleEndpointMidpoints id )
    |> Maybe.withDefault Cmd.none
  )

dragLine : Vec2 -> EndpointId -> Line -> Model -> ( Model, Cmd Msg )
dragLine thisPoint endpointId line model =
  ( { model
    | dragState = Just
      { dragged = HalfConnection endpointId (adjustLine thisPoint line)
      , lastPoint = thisPoint
      }
    }
  , Cmd.none
  )

onEndDrag : Model -> ( Model, Cmd Msg )
onEndDrag model =
  ( case model.dragState of
    Nothing -> model
    Just { dragged } ->
      ( withNothingDragged model
      |> case dragged of
        AudioModuleId _ -> identity
        HalfConnection halfConnection _ -> findConnection halfConnection
      )
  , Cmd.none
  )

findConnection : EndpointId -> Model -> Model
findConnection halfConnection model =
  List.foldl
    (\endpointId maybeConnection -> case maybeConnection of
      Just _ -> maybeConnection
      Nothing -> attemptConnection halfConnection endpointId model
    )
    Nothing
    ( Set.toList model.hovered )
  |> Maybe.map (\conn -> push conn model.connections)
  |> Maybe.map (\connections -> { model | connections = connections } )
  |> Maybe.withDefault model

attemptConnection : EndpointId -> EndpointId -> Model -> Maybe Connection
attemptConnection id1 id2 model =
  Maybe.map2
    (\e1 e2 -> ( { id = id1, endpoint = e1 }, { id = id2, endpoint = e2 } ) )
    ( endpointAt id1 model )
    ( endpointAt id2 model )
  |> Maybe.andThen orderConnection
  |> Maybe.andThen validateConnection
  |> Maybe.map ( \( p1, p2 ) -> { idIn = p1.id, idOut = p2.id } )

orderConnection :
  ( EndpointIdPair, EndpointIdPair )
  -> Maybe ( EndpointIdPair, EndpointIdPair )
orderConnection ( p1, p2 ) =
  if ( p1.endpoint.direction == p2.endpoint.direction )
  then Nothing
  else if ( p1.endpoint.direction == AudioModule.Endpoint.In )
  then Just ( p1, p2 )
  else Just ( p2, p1 )

validateConnection :
  ( EndpointIdPair, EndpointIdPair )
  -> Maybe ( EndpointIdPair, EndpointIdPair )
validateConnection ( p1, p2 ) =
  -- TODO validate the connection
  Just ( p1, p2 )

createLine : Vec2 -> Line
createLine start =
  { endOne = start, endTwo = start }

adjustLine : Vec2 -> Line -> Line
adjustLine end line =
  { line | endTwo = end }

updateEndpointCoordinate : Dom.Element -> EndpointId -> Model -> ( Model, Cmd Msg)
updateEndpointCoordinate domElement endpointId model =
  ( mapEndpoint
    (\endpoint -> { endpoint | midpoint = midpointFromDomElement domElement } )
    endpointId
    model
  , Cmd.none
  )

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Model -> Html.Html Msg
view model =
  Html.div
    [ Attributes.id "audio-module-map" ]
    ( viewConnectionMap model
    :: viewPrototypeBank model.prototypes
    :: viewAudioModules model.audioModules
    )

viewConnectionMap : Model -> Html.Html Msg
viewConnectionMap model =
  model.connections
  |> List.filterMap
    (\c -> Maybe.map2
      (\eIn eOut -> { endOne = eIn.midpoint, endTwo = eOut.midpoint } )
      ( endpointAt c.idIn model )
      ( endpointAt c.idOut model )
    )
  |> ( case model.dragState of
      Nothing -> identity
      Just { dragged } -> case dragged of
        HalfConnection _ line -> push line
        _ -> identity
    )
  |> List.map viewLine
  |> Svg.svg [ Attributes.id "connection-map" ]

viewLine : Line -> Html.Html Msg
viewLine { endOne, endTwo } =
  Svg.line
    [ Svg.Attributes.strokeLinecap "round"
    , endOne |> Tuple.first |> String.fromFloat |> Svg.Attributes.x1
    , endOne |> Tuple.second |> String.fromFloat |> Svg.Attributes.y1
    , endTwo |> Tuple.first |> String.fromFloat |> Svg.Attributes.x2
    , endTwo |> Tuple.second |> String.fromFloat |> Svg.Attributes.y2
    ]
    [ ]

viewPrototypeBank : Array PrototypeModule -> Html.Html Msg
viewPrototypeBank =
  Html.div
    [ Attributes.id "prototype-bank" ]
    << Array.toList
    << Array.map ( AudioModule.viewPrototype CreateAudioModule)

viewAudioModules : Dict Id AudioModule -> List (Html.Html Msg)
viewAudioModules audioModules =
  let
    ( floating, fixed ) = collectAudioModules audioModules
  in
    viewControllerBank fixed
    :: floating

collectAudioModules : Dict Id AudioModule -> ( ( List (Html.Html Msg) ), ( List (Html.Html Msg) ) )
collectAudioModules =
  List.foldl
    (\(id, audioModule) ->
      case audioModule.mode of
        Floating posinfo ->
          Tuple.mapFirst (push <| AudioModule.viewFloating posinfo (createTranslators id) audioModule)
        Fixed ->
          Tuple.mapSecond (push <| AudioModule.viewFixed (createTranslators id) audioModule)
    )
    ( [ ], [ ] )
    << Dict.toList

viewControllerBank : List ( Html.Html Msg ) -> Html.Html Msg
viewControllerBank =
  Html.div [ Attributes.id "controller-bank" ]

