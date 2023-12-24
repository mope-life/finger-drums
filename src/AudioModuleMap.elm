module AudioModuleMap exposing (main)

import Browser
import Html
import Html.Attributes as Attributes
import Svg
import Dict exposing (Dict)
import Platform.Cmd as Cmd
import Draggable
import Draggable.Events
import Browser.Events
import MouseEvent exposing (Point, Position)
import AudioModule exposing (AudioModule, Msg(..))
import AudioModule.Endpoint exposing (Msg(..))
import Svg.Attributes
import AudioModule.Endpoint as Endpoint

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
    , archetypes =
      [ AudioModule.initArchetype AudioModule.ConstantModule "const"
      , AudioModule.initArchetype AudioModule.VCOModule "osc"
      , AudioModule.initArchetype AudioModule.VCAModule "amp"
      , AudioModule.initArchetype AudioModule.EnvelopeModule "env"
      ]
    , draggedId = Nothing
    , nextId = 0
    , drag = Draggable.init
    , lineActive = False
    , lines = []
    }
    , Cmd.none
  )

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Model =
  { audioModules : Dict Id (AudioModule Msg)
  , draggedId : Maybe Id
  , archetypes : List (AudioModule.Archetype Msg)
  , nextId : Id
  , drag : Draggable.State Id
  , lineActive : Bool
  , lines : List Line
  }

type Msg
  = CreateAndStartDrag (AudioModule.Archetype Msg) (Draggable.Msg Id) Position
  | OnDragStart Id
  | OnDragEnd
  | OnDragBy Draggable.Delta
  | DragMsg (Draggable.Msg Id)
  | AudioModuleDelegate Id AudioModule.Msg
  | EndpointMouseDownContinue Point
  | EndpointMouseDownEnd Point

type alias Line =
  { endOne : Point
  , endTwo : Point
  }

type alias Id = Int

with : Id -> AudioModule Msg -> Model -> Model
with id audioModule model =
  { model | audioModules = model.audioModules |> Dict.insert id audioModule }

without : Id -> Model -> Model
without id model =
  { model | audioModules = model.audioModules |> Dict.remove id }

withDragged : Id -> Model -> Model
withDragged id model =
  { model | draggedId = Just id }

withNothingDragged : Model -> Model
withNothingDragged model =
  { model | draggedId = Nothing }

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

maybeMapAudioModule :
  (AudioModule Msg -> AudioModule Msg)
  -> Maybe Id
  -> Model
  -> Model
maybeMapAudioModule transform maybeId model =
  case maybeId of
    Nothing ->
      model
    Just id ->
      mapAudioModule transform id model

mapAudioModuleWithCmd :
  (AudioModule Msg -> (AudioModule Msg, Cmd msg))
  -> Id
  -> Model
  -> (Model, Cmd msg)
mapAudioModuleWithCmd transform id model =
  case (Dict.get id model.audioModules) of
    Nothing ->
      (model, Cmd.none)
    Just audioModule ->
      let
        (am, cmd) = transform audioModule
      in
        (with id am model, cmd)

--------------------------------------------------------------------------------
-- Subscriptions ---------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions { drag, lineActive } =
  Sub.batch
  [ Draggable.subscriptions DragMsg drag
  , if lineActive
    then
      EndpointMouseDownContinue
      |> Browser.Events.onMouseMove << MouseEvent.pointMessageDecoder
    else
      Sub.none
  , if lineActive
    then
      EndpointMouseDownEnd
      |> Browser.Events.onMouseUp << MouseEvent.pointMessageDecoder
    else
      Sub.none
  ]

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateAndStartDrag archetype dragMsg position ->
      model
      |> insertNewAudioModule archetype position
      |> Draggable.update dragConfig dragMsg
    DragMsg dragMsg ->
      Draggable.update dragConfig dragMsg model
    OnDragStart id ->
      ( withDragged id model, Cmd.none )
    OnDragEnd ->
      ( withNothingDragged model, Cmd.none )
    OnDragBy delta ->
      ( doDrag delta model, Cmd.none )
    AudioModuleDelegate id moduleMsg ->
      case moduleMsg of
        AudioModule.EndpointDelegate id2 endpointMsg ->
          case endpointMsg of
            Endpoint.MouseDown point ->
              ( { model
                | lineActive = True
                , lines = createLine point :: model.lines
                }
              , Cmd.none
              )
        _ ->
          mapAudioModuleWithCmd (AudioModule.update moduleMsg) id model
    EndpointMouseDownContinue point ->
      case (List.head model.lines) of
        Nothing ->
          ( model, Cmd.none )
        Just line ->
          ( { model | lines = adjustLine point line :: Maybe.withDefault [] (List.tail model.lines) }, Cmd.none )
    EndpointMouseDownEnd (x, y) ->
      Debug.log ("done: " ++ String.fromFloat x ++ ", " ++ String.fromFloat y)
        ( { model | lineActive = False } , Cmd.none )

insertNewAudioModule : AudioModule.Archetype Msg -> Position -> Model -> Model
insertNewAudioModule archetype position model =
  let
    index = model.nextId
  in
    model
    |> withNextId
    |>( AudioModule.init (AudioModuleDelegate index) archetype index
        |> AudioModule.at position
        |> with model.nextId
      )

createLine : Point -> Line
createLine point =
  { endOne = point, endTwo = point }

adjustLine : Point -> Line -> Line
adjustLine point line =
  { line | endTwo = point }

doDrag : Draggable.Delta -> Model -> Model
doDrag delta model =
  maybeMapAudioModule (AudioModule.translated delta) model.draggedId model

dragConfig : Draggable.Config Int Msg
dragConfig =
  Draggable.customConfig
    [ Draggable.Events.onDragBy OnDragBy
    , Draggable.Events.onDragStart OnDragStart
    , Draggable.Events.onDragEnd OnDragEnd
    ]

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Model -> Html.Html Msg
view model =
  Html.div
    [ Attributes.id "audio-module-map" ]
    ( viewArchetypeBank model
      :: viewConnectionMap model
      :: viewAudioModules model
    )

viewArchetypeBank : Model -> Html.Html Msg
viewArchetypeBank model =
  Html.div
    [ Attributes.id "archetype-bank" ]
    ( List.map
      (\archetype ->
        AudioModule.viewArchetype
          [ archetypeDragTrigger archetype model.nextId ]
          archetype
      )
      model.archetypes
    )

viewConnectionMap : Model -> Html.Html Msg
viewConnectionMap model =
  Svg.svg
    [Attributes.id "connection-map"]
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
  let
    extraAttributes id =
      audioModuleDragTrigger id ::
        if audioModuleIsDragged id model
        then [Attributes.class "dragging"]
        else []
    viewModule (id, audioModule) =
      AudioModule.view (extraAttributes id) audioModule
  in
    model.audioModules |> Dict.toList |> List.map viewModule

archetypeDragTrigger : AudioModule.Archetype Msg -> Id -> Html.Attribute Msg
archetypeDragTrigger archetype index =
  Draggable.customMouseTrigger index MouseEvent.positionDecoder (CreateAndStartDrag archetype)

audioModuleDragTrigger : Id -> Html.Attribute Msg
audioModuleDragTrigger id =
  Draggable.mouseTrigger id DragMsg

audioModuleIsDragged : Id -> Model -> Bool
audioModuleIsDragged id model =
  case Maybe.map (\i -> i == id) model.draggedId of
    Nothing ->
      False
    Just bool ->
      bool
