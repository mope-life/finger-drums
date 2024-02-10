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
    cmd = fetchAllEndpointDeltas initialModules
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
  -- Drag messages
  | StartDrag Draggable MouseEvent.MouseInfo
  | ContinueDrag MouseEvent.MouseInfo
  | EndDrag
  -- Endpoint messages
  | CreateHalfConnection EndpointId MouseEvent.MouseInfo
  | HoverEndpoint EndpointId
  | UnhoverEndpoint EndpointId
  | FetchEndpointDeltas
  | SetEndpointDelta EndpointId (Result Dom.Error (List Dom.Element))
  -- Control messages
  | ClickControl ControlId String
  | SetControlValue ControlId String
  | Focus (Result Dom.Error ())

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
  { model | audioModules = wrapAndMap wrapDict transform id model.audioModules }

endpointAt : EndpointId -> Model -> Maybe Endpoint
endpointAt (id, index) model =
  Dict.get id model.audioModules
  |> Maybe.map ( \am -> am.endpoints )
  |> Maybe.andThen ( Array.get index )

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

--------------------------------------------------------------------------------
-- Subscriptions ---------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions { dragState } =
  let
    defaultSubs =
      [ Browser.Events.onResize (\_ _ -> FetchEndpointDeltas )
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
      let
        id = model.nextId
        position = positionFromMouseInfo mouseInfo
        audioModule = createFloatingModule position type_ id
      in
        ( insertAndStartDrag audioModule mouseInfo model
        , fetchEndpointDeltas id audioModule
        )
    StartDrag draggable mouseInfo ->
      ( withDragged draggable ( mousePoint mouseInfo ) model, Cmd.none )
    ContinueDrag mouseInfo ->
      ( continueDrag ( mousePoint mouseInfo ) model, Cmd.none )
    EndDrag ->
      ( endDrag model, Cmd.none )
    HoverEndpoint endpointId ->
      ( withHovered endpointId model, Cmd.none )
    UnhoverEndpoint endpointId ->
      ( withoutHovered endpointId model, Cmd.none )
    FetchEndpointDeltas ->
      ( model, fetchAllEndpointDeltas model.audioModules )
    SetEndpointDelta endpointId result ->
      ( setEndpointDelta endpointId result model, Cmd.none )
    CreateHalfConnection endpointId mouseInfo ->
      ( snapAndStartDrag endpointId mouseInfo model, Cmd.none )
    ClickControl _ htmlId ->
      ( model, Task.attempt Focus ( Dom.focus htmlId ) )
    SetControlValue controlId value ->
      ( setControlValue controlId value model, Cmd.none )
    Focus _ -> ( model, Cmd.none )

fetchAllEndpointDeltas : Dict AudioModuleId AudioModule -> Cmd Msg
fetchAllEndpointDeltas audioModules =
  Dict.toList audioModules
  |> List.map ( apply2 fetchEndpointDeltas )
  |> Cmd.batch

fetchEndpointDeltas : AudioModuleId -> AudioModule -> Cmd Msg
fetchEndpointDeltas id audioModule =
  audioModule.endpoints
  |> Array.indexedMap
    (\index endpoint ->
      SetEndpointDelta (id, index)
      |> fetchElements [ endpoint.htmlId, audioModule.htmlId ]
    )
  |> Array.toList
  |> Cmd.batch

setEndpointDelta : EndpointId -> Result Dom.Error ( List Dom.Element ) -> Model -> Model
setEndpointDelta (id, index) result =
  case result of
    Ok [ endpointElement, audioModuleElement ] ->
      mapAudioModule
        ( AudioModule.updateEndpointDelta endpointElement audioModuleElement
          index
        )
        id
    _ -> identity

snapAndStartDrag : EndpointId -> MouseEvent.MouseInfo -> Model -> Model
snapAndStartDrag endpointId mouseInfo =
  let
    start = midpointFromMouseInfo mouseInfo
    newLine = { endOne = start, endTwo = start }
  in
    withDragged ( HalfConnection endpointId newLine ) ( mousePoint mouseInfo )

insertAndStartDrag : AudioModule -> MouseEvent.MouseInfo -> Model -> Model
insertAndStartDrag audioModule mouseInfo model =
  with model.nextId audioModule model
  |> withDragged ( FloatingModule model.nextId ) ( mousePoint mouseInfo )
  |> withNextId

continueDrag : Vec2 -> Model -> Model
continueDrag point model =
  case model.dragState of
    Nothing -> model
    Just { dragged, lastPoint } -> case dragged of
      FloatingModule id ->
        dragAudioModule point lastPoint id model
      HalfConnection halfConnection line ->
        dragLine point halfConnection line model

dragAudioModule : Vec2 -> Vec2 -> AudioModuleId -> Model -> Model
dragAudioModule thisPoint lastPoint id model =
  { model | dragState = Just
    { dragged = FloatingModule id
    , lastPoint = thisPoint
    }
  }
  |> mapAudioModule (AudioModule.translated <| delta lastPoint thisPoint ) id

dragLine : Vec2 -> EndpointId -> Line -> Model -> Model
dragLine thisPoint endpointId line model =
  { model | dragState = Just
    { dragged = HalfConnection endpointId { line | endTwo = thisPoint }
    , lastPoint = thisPoint
    }
  }

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
    |> Maybe.andThen validateConnection
    |> Maybe.map makeConnection

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

-- TODO: validate the connection
validateConnection :
  ( EndpointIdPair, EndpointIdPair )
  -> Maybe ( EndpointIdPair, EndpointIdPair )
validateConnection = Just

makeConnection : ( EndpointIdPair, EndpointIdPair ) -> Connection
makeConnection ( pOut, pIn ) =
  { idOut = pOut.id, idIn = pIn.id }

setControlValue : ControlId -> String -> Model -> Model
setControlValue ( id, index ) value =
  mapAudioModule (AudioModule.updateControlValue value index) id

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
    viewControllerBank fixed :: floating

viewControllerBank : List ( Html.Html Msg ) -> Html.Html Msg
viewControllerBank =
  Html.div [ Attributes.id "controller-bank" ]

collectAudioModules :
  Dict AudioModuleId AudioModule
  -> ( ( List (Html.Html Msg) ), ( List (Html.Html Msg) ) )
collectAudioModules =
  List.foldr
    (\(id, audioModule) ->
      case audioModule.mode of
        Floating posinfo -> Tuple.mapFirst
          ( AudioModule.viewFloating posinfo (createTranslators id) audioModule
          |> push
          )
        Fixed -> Tuple.mapSecond
          ( AudioModule.viewFixed (createTranslators id) audioModule
          |> push
          )
    )
    ( [ ], [ ] )
  << Dict.toList

createTranslators : AudioModuleId -> Translators Msg
createTranslators id =
  { startDrag = StartDrag (FloatingModule id)
  , controlTranslators = \index ->
    { controlClick = ClickControl (id, index)
    , controlInput = SetControlValue (id, index)
    }
  , endpointTranslators = \index ->
    { createHalfConnection = CreateHalfConnection (id, index)
    , hoverEndpoint = HoverEndpoint (id, index)
    , unhoverEndpoint = UnhoverEndpoint (id, index)
    }
  }
