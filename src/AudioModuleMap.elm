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
import Html.Attributes exposing (draggable)

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
      , createFixedModule DestinationModule
      ]
      |> List.indexedMap (\i f -> (i, f i) )
      |> Dict.fromList
    prototypes =
      [ createPrototypeModule ConstantModule
      , createPrototypeModule VCOModule
      , createPrototypeModule VCAModule
      , createPrototypeModule EnvelopeModule
      ]
      |> List.indexedMap (\i f -> f i )
      |> Array.fromList
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
  { audioModules : Dict AudioModuleId AudioModule
  , prototypes : Array PrototypeModule
  , connections : List Connection
  , dragState : Maybe DragState
  , hovered : Set EndpointId
  , nextId : AudioModuleId
  }

type Msg
  = CreateAudioModule Type MouseEvent.MouseInfo
  | UpdateEndpointMidpoint EndpointId (Result Dom.Error Dom.Element)
  | FocusResult (Result Dom.Error ())
  | Reinitialize
  -- Drag messages
  | StartDrag Draggable MouseEvent.MouseInfo
  | ContinueDrag MouseEvent.MouseInfo
  | EndDrag
  -- Endpoint messages
  | CreateHalfConnection EndpointId MouseEvent.MouseInfo
  | HoverEndpoint EndpointId
  | UnhoverEndpoint EndpointId
  -- Control messages
  | ClickControl ControlId String
  | InputControl ControlId String

type alias DragState =
  { dragged : Draggable
  , lastPoint : Vec2
  }

type Draggable
  = FloatingModule AudioModuleId
  | HalfConnection EndpointId Line

type alias Connection =
  { idOut : EndpointId
  , idIn : EndpointId
  }

type alias Line =
  { endOne : Vec2
  , endTwo : Vec2
  }

type alias AudioModuleId = Int
type alias EndpointId = ( AudioModuleId, Int )
type alias ControlId = ( AudioModuleId, Int )

type alias EndpointIdPair =
  { id : EndpointId
  , endpoint : Endpoint
  }

with : AudioModuleId -> AudioModule -> Model -> Model
with id audioModule model =
  { model | audioModules = model.audioModules |> Dict.insert id audioModule }

without : AudioModuleId -> Model -> Model
without id model =
  { model | audioModules = model.audioModules |> Dict.remove id }

withDragged : Draggable -> Vec2 -> Model -> Model
withDragged draggable start model =
  { model | dragState = Just { dragged = draggable, lastPoint = start } }

withNothingDragged : Model -> Model
withNothingDragged model =
  { model | dragState = Nothing }

withNextId : Model -> Model
withNextId model =
  { model | nextId = model.nextId + 1 }

withHovered : EndpointId -> Model -> Model
withHovered endpointId model =
  { model | hovered = Set.insert endpointId model.hovered }

withoutHovered : EndpointId -> Model -> Model
withoutHovered endpointId model =
  { model | hovered = Set.remove endpointId model.hovered }

mapAudioModule : (AudioModule -> AudioModule) -> AudioModuleId -> Model -> Model
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
  let
    defaultSubs =
      [ Browser.Events.onResize (\_ _ -> Reinitialize )
      ]
    mouseSubs =
      [ ContinueDrag
      |> Browser.Events.onMouseMove << MouseEvent.messageDecoder
      , EndDrag
      |> Browser.Events.onMouseUp << Decode.succeed
      ]
      ++ defaultSubs
  in
    Sub.batch ( ifAnything mouseSubs defaultSubs dragState )

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateAudioModule type_ mouseInfo ->
      ( insertAndStartDrag
        ( createFloatingModule (positionFromMouseInfo mouseInfo) type_ model.nextId )
        mouseInfo
        model
      , Cmd.none
      )
    UpdateEndpointMidpoint endpointId result ->
      ( case result of
        Err _ -> model
        Ok domElement -> updateEndpointCoordinate domElement endpointId model
      , Cmd.none
      )
    FocusResult _ ->
      ( model, Cmd.none )
    Reinitialize ->
      ( model, initializeEndpoints model )
    StartDrag draggable mouseInfo ->
      ( withDragged draggable ( mousePoint mouseInfo ) model, Cmd.none )
    ContinueDrag mouseInfo ->
      onContinueDrag ( mousePoint mouseInfo ) model
    EndDrag ->
      ( endDrag model, Cmd.none )
    HoverEndpoint endpointId ->
      ( withHovered endpointId model, Cmd.none )
    UnhoverEndpoint endpointId ->
      ( withoutHovered endpointId model, Cmd.none )
    CreateHalfConnection endpointId mouseInfo ->
      ( snapAndStartDrag endpointId mouseInfo model, Cmd.none )
    ClickControl _ htmlId ->
      ( model, Task.attempt FocusResult ( Dom.focus htmlId ) )
    InputControl ( id, index ) value ->
      ( mapAudioModule (AudioModule.updateControlValue index value) id model
      , Cmd.none
      )

initializeEndpoints : Model -> Cmd Msg
initializeEndpoints model =
  model.audioModules
  |> Dict.toList
  |> List.map ( apply2 fetchAudioModuleEndpointMidpoints )
  |> Cmd.batch

fetchAudioModuleEndpointMidpoints : AudioModuleId -> AudioModule -> Cmd Msg
fetchAudioModuleEndpointMidpoints id audioModule =
  audioModule.endpoints
  |> Array.indexedMap (\index -> fetchEndpointMidpoint ( id, index ) )
  |> Array.toList
  |> Cmd.batch

fetchEndpointMidpoint : EndpointId -> Endpoint -> Cmd Msg
fetchEndpointMidpoint endpointId endpoint =
  Task.attempt
    ( UpdateEndpointMidpoint endpointId )
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

createPrototypeModule : Type -> AudioModuleId -> PrototypeModule
createPrototypeModule type_ id =
  AudioModule.initPrototype type_ ( "proto-" ++ ( String.fromInt id ) )

createFloatingModule : Vec2 -> Type -> AudioModuleId -> AudioModule
createFloatingModule start type_=
  AudioModule.init ( Floating start ) type_ << createId

createFixedModule : Type -> AudioModuleId -> AudioModule
createFixedModule type_ =
  AudioModule.init Fixed type_ << createId

createId : AudioModuleId -> String
createId id =
  "module-" ++ ( String.fromInt id )

createTranslators : AudioModuleId -> Translators Msg
createTranslators id =
  { startDrag = StartDrag (FloatingModule id)
  , controlTranslators = \index ->
    { controlClick = ClickControl (id, index)
    , controlInput = InputControl (id, index)
    }
  , endpointTranslators = \index ->
    { createHalfConnection = CreateHalfConnection (id, index)
    , hoverEndpoint = HoverEndpoint (id, index)
    , unhoverEndpoint = UnhoverEndpoint (id, index)
    }
  }

snapAndStartDrag : EndpointId -> MouseEvent.MouseInfo -> Model -> Model
snapAndStartDrag endpointId mouseInfo =
  withDragged
    ( HalfConnection
      endpointId
      ( createLine ( midpointFromMouseInfo mouseInfo ) )
    )
    ( mousePoint mouseInfo )

insertAndStartDrag : AudioModule -> MouseEvent.MouseInfo -> Model -> Model
insertAndStartDrag audioModule mouseInfo model =
  withDragged
    ( FloatingModule model.nextId )
    ( mousePoint mouseInfo )
    ( model
    |> with model.nextId audioModule
    |> withNextId
    )

onContinueDrag : Vec2 -> Model -> ( Model, Cmd Msg )
onContinueDrag point model =
  case model.dragState of
    Nothing -> ( model, Cmd.none )
    Just { dragged, lastPoint } -> case dragged of
      FloatingModule id ->
        dragAudioModule point lastPoint id model
      HalfConnection halfConnection line ->
        dragLine point halfConnection line model

dragAudioModule : Vec2 -> Vec2 -> AudioModuleId -> Model -> ( Model, Cmd Msg )
dragAudioModule thisPoint lastPoint id model =
  ( { model
    | dragState = Just
      { dragged = FloatingModule id
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

endDrag : Model -> Model
endDrag model =
  ( case model.dragState of
    Nothing -> model
    Just dragState -> findConnection dragState model
  )
  |> withNothingDragged

findConnection : DragState -> Model -> Model
findConnection { dragged } model =
  case dragged of
    FloatingModule _ -> model
    HalfConnection halfConnection _ ->
      findFirstValid
        (\hoveredId -> attemptConnection halfConnection hoveredId model )
        ( Set.toList model.hovered )
      |> Maybe.map (\c -> { model | connections = push c model.connections } )
      |> Maybe.withDefault model

attemptConnection : EndpointId -> EndpointId -> Model -> Maybe Connection
attemptConnection id1 id2 model =
  let
    toPair = \endpointId ->
      endpointAt endpointId model
      |> Maybe.map (\endpoint -> { id = endpointId, endpoint = endpoint } )
  in
    Maybe.map2 Tuple.pair ( toPair id1 ) ( toPair id2 )
    |> Maybe.andThen orderConnection
    >> Maybe.andThen validateConnection
    >> Maybe.map (\(pOut, pIn) -> { idOut = pOut.id, idIn = pIn.id } )

orderConnection :
  ( EndpointIdPair, EndpointIdPair )
  -> Maybe ( EndpointIdPair, EndpointIdPair )
orderConnection ( p1, p2 ) =
  ( if ( p1.endpoint.direction == p2.endpoint.direction )
    then Nothing
    else if ( p1.endpoint.direction == AudioModule.Endpoint.Out )
    then Just ( p1, p2 )
    else Just ( p2, p1 )
  )

validateConnection :
  ( EndpointIdPair, EndpointIdPair )
  -> Maybe ( EndpointIdPair, EndpointIdPair )
validateConnection =
  -- TODO validate the connection
  Just << identity

createLine : Vec2 -> Line
createLine start =
  { endOne = start, endTwo = start }

adjustLine : Vec2 -> Line -> Line
adjustLine end line =
  { line | endTwo = end }

updateEndpointCoordinate : Dom.Element -> EndpointId -> Model -> Model
updateEndpointCoordinate domElement =
  mapEndpoint
    (\endpoint -> { endpoint | midpoint = midpointFromDomElement domElement } )

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Model -> Html.Html Msg
view model =
  Html.div
    [ Attributes.id "audio-module-map"
    , Attributes.classList
      [ ( "dragging", ( ifAnything True False model.dragState ) ) ]
    ]
    ( viewConnectionMap model
    :: viewPrototypeBank model.prototypes
    :: viewAudioModules model.audioModules
    )

viewConnectionMap : Model -> Html.Html Msg
viewConnectionMap model =
  model.connections
  |> List.filterMap
    (\c -> Maybe.map2
      (\eOut eIn -> { endOne = eOut.midpoint, endTwo = eIn.midpoint } )
      ( endpointAt c.idOut model )
      ( endpointAt c.idIn model )
    )
  |>( case model.dragState of
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

viewAudioModules : Dict AudioModuleId AudioModule -> List (Html.Html Msg)
viewAudioModules audioModules =
  let
    ( floating, fixed ) = collectAudioModules audioModules
  in
    viewControllerBank fixed
    :: floating

collectAudioModules : Dict AudioModuleId AudioModule -> ( ( List (Html.Html Msg) ), ( List (Html.Html Msg) ) )
collectAudioModules =
  List.foldr
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

