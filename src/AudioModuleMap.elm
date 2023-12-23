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
      [ AudioModule.initPrototype AudioModule.ConstantModule
      , AudioModule.initPrototype AudioModule.VCOModule
      , AudioModule.initPrototype AudioModule.VCAModule
      , AudioModule.initPrototype AudioModule.EnvelopeModule
      ]
    , draggedId = Nothing
    , nextId = 0
    , drag = Draggable.init
    }
    , Cmd.none
  )

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Model =
  { audioModules : Dict Id AudioModule
  , draggedId : Maybe Id
  , prototypes : List AudioModule.Prototype
  , nextId : Id
  , drag : Draggable.State Id
  }

type Msg
  = CreateAndStartDrag AudioModule.Type (Draggable.Msg Id) AudioModule.Position
  | OnDragStart Id
  | OnDragEnd
  | OnDragBy Draggable.Delta
  | DragMsg (Draggable.Msg Id)
  | Delegate Id AudioModule.Msg

type alias Id = Int

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

mapAudioModuleWithCmd :
  (AudioModule -> (AudioModule, Cmd msg))
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
subscriptions { drag } =
  Draggable.subscriptions DragMsg drag

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateAndStartDrag type_ dragMsg position ->
      model
      |> insertNewAudioModule type_ position
      |> Draggable.update dragConfig dragMsg
    DragMsg dragMsg ->
      Draggable.update dragConfig dragMsg model
    OnDragStart id ->
      ( withDragged id model, Cmd.none )
    OnDragEnd ->
      ( withNothingDragged model, Cmd.none )
    OnDragBy delta ->
      ( doDrag delta model, Cmd.none )
    Delegate id moduleMsg ->
      mapAudioModuleWithCmd
        (AudioModule.update (Delegate id) moduleMsg) id model

insertNewAudioModule : AudioModule.Type -> AudioModule.Position -> Model -> Model
insertNewAudioModule type_ position model =
  model
  |> withNextId
  |>( AudioModule.init type_ (String.fromInt model.nextId)
    |> AudioModule.at position
    |> with model.nextId
    )

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
    ( viewPrototypeBank model
      :: viewConnectionMap model
      :: viewAudioModules model
    )

viewPrototypeBank : Model -> Html.Html Msg
viewPrototypeBank model =
  Html.div
    [ Attributes.id "prototype-bank" ]
    ( List.map
      (\proto -> AudioModule.viewPrototype
        [prototypeDragTrigger model.nextId proto.type_]
        proto
      )
      model.prototypes
    )

viewConnectionMap : Model -> Html.Html Msg
viewConnectionMap _ =
  Svg.svg [Attributes.id "connection-map"] []

viewAudioModules : Model -> List (Html.Html Msg)
viewAudioModules model =
  let
    extraAttributes id =
      (audioModuleDragTrigger id) ::
        if audioModuleIsDragged id model
        then [Attributes.class "dragging"]
        else []
    viewModule (id, audioModule) =
      AudioModule.view (Delegate id) (extraAttributes id) audioModule
  in
    model.audioModules |> Dict.toList |> List.map viewModule

prototypeDragTrigger : Id -> AudioModule.Type -> Html.Attribute Msg
prototypeDragTrigger id type_ =
  Draggable.customMouseTrigger id positionDecoder (CreateAndStartDrag type_)

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

positionDecoder : Decode.Decoder AudioModule.Position
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

type alias ClickInfo =
  { offsetX : Float
  , offsetY : Float
  , pageX : Float
  , pageY : Float
  }
