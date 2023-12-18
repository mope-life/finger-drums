module AudioModuleMap exposing (main)

import Browser
import Html
import Html.Attributes as Attributes
import Svg
import Dict exposing (Dict)
import Platform.Cmd as Cmd
import Json.Decode as Decode
import Draggable
import Draggable.Events
import Html.Attributes as Attributes
import AudioModule exposing (AudioModule)
import AudioModule exposing (Position)

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
    , draggedId = Nothing
    , prototypes =
      [ AudioModule.init AudioModule.ConstantModule |> AudioModule.asPrototype
      , AudioModule.init AudioModule.VCOModule |> AudioModule.asPrototype
      , AudioModule.init AudioModule.VCAModule |> AudioModule.asPrototype
      , AudioModule.init AudioModule.EnvelopeModule |> AudioModule.asPrototype
      ]
    , nextId = 0
    , drag = Draggable.init
    }
    , Cmd.none
  )

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Model =
  { audioModules : Dict Id AudioModule
  , prototypes : List AudioModule
  , draggedId : Maybe Id
  , nextId : Id
  , drag : Draggable.State Id
  }

type Msg
  = CreateAndStartDrag (Draggable.Msg Id) CreateInfo
  | OnDragStart Id
  | OnDragEnd
  | OnDragBy Draggable.Delta
  | DragMsg (Draggable.Msg Id)
  | AudioModuleMsg Id AudioModule.Msg

type alias Id = Int

type alias CreateInfo =
  { type_ : AudioModule.Type
  , position : AudioModule.Position
  }

type alias ClickInfo =
  { offsetX : Float
  , offsetY : Float
  , pageX : Float
  , pageY : Float
  }

with : Id -> AudioModule -> Model -> Model
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

mapAudioModule : (AudioModule -> AudioModule) -> Id -> Model -> Model
mapAudioModule transform id model =
  case (Dict.get id model.audioModules) of
    Nothing ->
      model
    Just audioModule ->
      with id (transform audioModule) model

maybeMapAudioModule : (AudioModule -> AudioModule) -> Maybe Id -> Model -> Model
maybeMapAudioModule transform maybeId model =
  case maybeId of
    Nothing ->
      model
    Just id ->
      mapAudioModule transform id model

--------------------------------------------------------------------------------
-- Subscriptions ---------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions { drag } =
  Draggable.subscriptions DragMsg drag

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateAndStartDrag dragMsg createInfo ->
      model
      |> insertNewAudioModule createInfo
      |> Draggable.update dragConfig dragMsg
    DragMsg dragMsg ->
      Draggable.update dragConfig dragMsg model
    OnDragStart id ->
      ( startDragging id model, Cmd.none )
    OnDragEnd ->
      ( endDragging model, Cmd.none )
    OnDragBy delta ->
      ( doDrag delta model, Cmd.none )
    AudioModuleMsg id moduleMsg ->
      ( mapAudioModule (AudioModule.update moduleMsg) id model, Cmd.none )

insertNewAudioModule : CreateInfo -> Model -> Model
insertNewAudioModule { type_, position} model =
  model
  |> withNextId
  |> ( AudioModule.init type_ |> AudioModule.at position |> with model.nextId )

startDragging : Id -> Model -> Model
startDragging id model =
  model
  |> withDragged id
  |> mapAudioModule AudioModule.dragging id

endDragging : Model -> Model
endDragging model =
  model
  |> withNothingDragged
  |> maybeMapAudioModule AudioModule.notDragging model.draggedId

doDrag : Draggable.Delta -> Model -> Model
doDrag delta model =
  maybeMapAudioModule (AudioModule.movedBy delta) model.draggedId model

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
    ( viewPrototypeBank model
      :: viewConnectionMap model
      :: viewAudioModules model
    )

viewPrototypeBank : Model -> Html.Html Msg
viewPrototypeBank model =
  let
    mouseTrigger = \audioModule ->
      Draggable.customMouseTrigger
      model.nextId
      (creationDecoder audioModule.type_)
      CreateAndStartDrag
    viewPrototype prototype =
      AudioModule.viewBasic [mouseTrigger prototype] prototype
  in
    Html.div
      [ Attributes.id "prototype-bank" ]
      ( List.map viewPrototype model.prototypes)

viewConnectionMap : Model -> Html.Html Msg
viewConnectionMap _ =
  Svg.svg [Attributes.id "connection-map"] []

viewAudioModules : Model -> List (Html.Html Msg)
viewAudioModules model =
  let
    mouseTrigger id =
      Draggable.mouseTrigger id DragMsg
    viewAudioModule (id, audioModule) =
      AudioModule.view (AudioModuleMsg id) [mouseTrigger id] audioModule
  in
    model.audioModules |> Dict.toList |> List.map viewAudioModule

creationDecoder : AudioModule.Type -> Decode.Decoder CreateInfo
creationDecoder type_ =
  Decode.map (CreateInfo type_) positionDecoder

positionDecoder : Decode.Decoder Position
positionDecoder =
  Decode.map
    ( \clickInfo ->
        ( clickInfo.pageX - clickInfo.offsetX
        , clickInfo.pageY - clickInfo.offsetY
        )
    )
    mouseDownDecoder

mouseDownDecoder : Decode.Decoder ClickInfo
mouseDownDecoder =
  Decode.map4 ClickInfo
    ( Decode.field "offsetX" Decode.float)
    ( Decode.field "offsetY" Decode.float)
    ( Decode.field "pageX" Decode.float)
    ( Decode.field "pageY" Decode.float)
