module AudioModuleBank exposing (main)

import Browser
import Html exposing (Html, text, div)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onMouseDown)
import Dict exposing (Dict)
import Platform.Cmd as Cmd
import Draggable
import Draggable.Events exposing (onDragBy, onDragStart, onDragEnd)
import Html.Attributes exposing (style)


{- MAIN -}
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : () -> ( Model, Cmd Msg )
init _ = (
    { audioModules = Dict.empty
    , draggedModule = Nothing
    , nextId = 0
    , drag = Draggable.init
    } 
    , Cmd.none
  )


{- MODEL -}
type alias Model =
  { audioModules : Dict Id AudioModule
  , draggedModule : Maybe AudioModule
  , nextId : Id
  , drag : Draggable.State Id
  }

type alias Id = Int

type alias AudioModule =
  { type_ : AudioModuleType
  , id : Id
  , position : (Float, Float)
  }

type AudioModuleType
  = ControllerModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule
--| More to come!

type Msg
  = Click AudioModuleType
  | OnDragStart Id
  | OnDragEnd
  | OnDragBy Draggable.Delta
  | DragMsg (Draggable.Msg Id)


{- SUBSCRIPTIONS -}
subscriptions : Model -> Sub Msg
subscriptions { drag } =
  Draggable.subscriptions DragMsg drag


{- UPDATE -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Click moduleType ->
      (insertNewAudioModule moduleType model
      , Cmd.none)
    OnDragStart id ->
      (startDragging id model
      , Cmd.none)
    OnDragEnd ->
      (endDragging model
      , Cmd.none)
    OnDragBy delta ->
      (repositionDraggedModule delta model
      , Cmd.none)
    DragMsg dragMsg ->
      Draggable.update dragConfig dragMsg model

repositionDraggedModule : Draggable.Delta -> Model -> Model
repositionDraggedModule delta model =
  { model | draggedModule =
      Maybe.map (repositionModule delta) model.draggedModule }

repositionModule : Draggable.Delta ->  AudioModule -> AudioModule
repositionModule (deltaX, deltaY) audioModule =
  let
    (x, y) = audioModule.position
  in
    { audioModule | position = (x + deltaX, y + deltaY) }

startDragging : Id -> Model -> Model
startDragging id model =
  { model
    | draggedModule = Dict.get id model.audioModules
    , audioModules = Dict.remove id model.audioModules
  }

endDragging : Model -> Model
endDragging model =
  case model.draggedModule of
    Nothing -> model
    Just audioModule ->
      model
      |> insertAudioModule audioModule
      |> \m -> { m | draggedModule = Nothing }

insertAudioModule : AudioModule -> Model -> Model
insertAudioModule audioModule model =
  { model
    | audioModules = Dict.insert audioModule.id audioModule model.audioModules
  }

insertNewAudioModule : AudioModuleType -> Model -> Model
insertNewAudioModule moduleType model =
  model
  |> insertAudioModule (createAudioModule moduleType model.nextId)
  |> \m -> { m | nextId = m.nextId + 1 }

createAudioModule : AudioModuleType -> Id -> AudioModule
createAudioModule moduleType id =
  { type_ = moduleType
  , id = id
  , position = (0, 0)
  }

dragConfig : Draggable.Config Int Msg
dragConfig =
  Draggable.customConfig
    [ onDragBy OnDragBy
    , onDragStart OnDragStart
    , onDragEnd OnDragEnd
    ]


{- VIEW -}
view : Model -> Html Msg
view model =
  div [id "module-prototype-bank"]
    ( List.concat
      [ List.map prototypeElement creatablePrototypes
      , viewAudioModules model
      ]
    )

viewAudioModules : Model -> List (Html Msg)
viewAudioModules model =
  Dict.values model.audioModules
  |> \list -> (
    case model.draggedModule of
      Nothing -> list
      Just audioModule -> audioModule :: list
    )
  |> List.map audioModuleElement

audioModuleElement : AudioModule -> Html Msg
audioModuleElement audioModule =
  let
    (x, y) = audioModule.position
  in
    div
      [ class "module-wrapper"
      , class "grabbable"
      , style "position" "absolute"
      , style "left" ((String.fromInt (round x)) ++ "px")
      , style "top" ((String.fromInt (round y)) ++ "px")
      , Draggable.mouseTrigger audioModule.id DragMsg
      ]
      [ ]

creatablePrototypes : List (AudioModuleType, String)
creatablePrototypes =
  [ ( ConstantModule, "Const")
  , ( VCOModule, "VCO")
  , ( VCAModule, "VCA")
  , ( EnvelopeModule, "Envelope")
  ]

prototypeElement : (AudioModuleType, String) -> Html Msg
prototypeElement ( moduleType, name ) =
  div
    [ class "module-prototype"
    , class "grabbable"
    , onMouseDown (Click moduleType)
    ]
    [ text name ]
