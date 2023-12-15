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
init _ =
  ( { audioModules = Dict.empty
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
  = CreateAndStartDrag (Draggable.Msg Id) CreateInfo
  | OnDragStart Id
  | OnDragEnd
  | OnDragBy Draggable.Delta
  | DragMsg (Draggable.Msg Id)

type alias ClickInfo =
  { offsetX : Float
  , offsetY : Float
  , pageX : Float
  , pageY : Float
  }

type alias CreateInfo =
  { clickInfo : ClickInfo
  , typeClicked : AudioModuleType
  }

{- SUBSCRIPTIONS -}
subscriptions : Model -> Sub Msg
subscriptions { drag } =
  Draggable.subscriptions DragMsg drag


{- UPDATE -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    CreateAndStartDrag dragMsg {clickInfo, typeClicked} ->
      let newModule = createAudioModule typeClicked clickInfo model.nextId
      in
        Draggable.update
        dragConfig
        dragMsg
        ( model
          |> insertAudioModule newModule
          |> \m -> { m | nextId = m.nextId + 1 }
        )
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
  { model 
    | draggedModule = Maybe.map (repositionModule delta) model.draggedModule
  }

repositionModule : Draggable.Delta ->  AudioModule -> AudioModule
repositionModule (deltaX, deltaY) audioModule =
  let (x, y) = audioModule.position
  in { audioModule | position = ( x + deltaX, y + deltaY ) }

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

createAudioModule : AudioModuleType -> ClickInfo -> Id -> AudioModule
createAudioModule moduleType clickInfo id =
  { type_ = moduleType
  , id = id
  , position =
    ( clickInfo.pageX - clickInfo.offsetX
    , clickInfo.pageY - clickInfo.offsetY
    )
  }

dragConfig : Draggable.Config Int Msg
dragConfig =
  Draggable.customConfig
    [ Draggable.Events.onDragBy OnDragBy
    , Draggable.Events.onDragStart OnDragStart
    , Draggable.Events.onDragEnd OnDragEnd
    ]

{- VIEW -}
view : Model -> Html.Html Msg
view model =
  Html.div [ Attributes.id "audio-module-map" ]
  (List.concat [
    [ Html.div [Attributes.id "prototype-bank"]
      (List.map
        (viewPrototype model.nextId)
        [ ( ConstantModule, "Const")
        , ( VCOModule, "VCO")
        , ( VCAModule, "VCA")
        , ( EnvelopeModule, "Envelope")
        ]
      )
    , Svg.svg [Attributes.id "connection-map"]
      []
    ]
    , viewAudioModules model
  ] )

viewAudioModules : Model -> List (Html.Html Msg)
viewAudioModules model =
  let list = Dict.values model.audioModules |> List.map (viewAudioModule False)
  in case model.draggedModule of
    Nothing -> list
    Just audioModule -> (viewAudioModule True audioModule) :: list

viewAudioModule : Bool -> AudioModule -> Html.Html Msg
viewAudioModule dragging audioModule =
  let
    (x, y) = audioModule.position
    attrs =
      [ Attributes.style "left" ((String.fromInt (round x)) ++ "px")
      , Attributes.style "top" ((String.fromInt (round y)) ++ "px")
      , Attributes.style "position" "absolute"
      , Draggable.mouseTrigger audioModule.id DragMsg
      ]
  in
    viewGenericModule audioModule.type_
    ( if dragging then Attributes.style "z-index" "1000" :: attrs
      else attrs
    )
    []

viewPrototype : Id -> (AudioModuleType, String) -> Html.Html Msg
viewPrototype nextId ( moduleType, name ) =
  viewGenericModule moduleType
    [ Draggable.customMouseTrigger
      nextId
      (mouseDownDecoder moduleType)
      CreateAndStartDrag
    ]
    [ Html.div [ Attributes.class "prototype-click-shield"] [] ]

viewGenericModule :
  AudioModuleType
  -> List (Html.Attribute Msg)
  -> List (Html.Html Msg)
  -> Html.Html Msg
viewGenericModule moduleType extraAttributes extraElements  =
  Html.div
    ( List.concat [
      [ Attributes.class "module-wrapper"
      , Attributes.class "grabbable"
      ]
      , extraAttributes
    ] )
    ( List.concat [
      [ Html.div [Attributes.class "endpoint-bank"] (endpointsInFor moduleType)
      , Html.div [Attributes.class "control-bank"] []
      , Html.div [Attributes.class "endpoint-bank"] (endpointsOutFor moduleType)
      ]
      , extraElements
    ] )

endpointsInFor : AudioModuleType -> List (Html.Html Msg)
endpointsInFor audioModuleType =
  case audioModuleType of
    ControllerModule ->
      []
    DestinationModule ->
      [ viewEndpoint "signal_in" ]
    ConstantModule ->
      []
    VCOModule ->
      [ viewEndpoint "freq_in"
      , viewEndpoint "detune_in"
      ]
    VCAModule ->
      [ viewEndpoint "signal_in"
      , viewEndpoint "cv_in"
      ]
    EnvelopeModule ->
      [ viewEndpoint "gate_in"
      , viewEndpoint "trigger_in"
      ]

endpointsOutFor : AudioModuleType -> List (Html.Html Msg)
endpointsOutFor audioModuleType =
  case audioModuleType of
    ControllerModule ->
      [ viewEndpoint "freq_out"
      , viewEndpoint "gate_out"
      , viewEndpoint "trigger_out"
      ]
    DestinationModule ->
      []
    ConstantModule ->
      [ viewEndpoint "cv_out" ]
    VCOModule ->
      [ viewEndpoint "signal_out" ]
    VCAModule ->
      [ viewEndpoint "signal_out" ]
    EnvelopeModule ->
      [ viewEndpoint "level_out" ]

viewEndpoint : String -> Html.Html Msg
viewEndpoint label =
  Html.div [ Attributes.class "endpoint-wrapper" ]
  [ Html.div
    [ Attributes.class "endpoint-jack", Attributes.class "grabbable" ] []
  , Html.label
    [ Attributes.class "endpoint-label" ] [ Html.text label ]
  ]

mouseDownDecoder : AudioModuleType -> Decode.Decoder CreateInfo
mouseDownDecoder type_ =
  (Decode.map4 ClickInfo
    ( Decode.field "offsetX" Decode.float)
    ( Decode.field "offsetY" Decode.float)
    ( Decode.field "pageX" Decode.float)
    ( Decode.field "pageY" Decode.float)
  )
  |> Decode.map (\ci -> { clickInfo = ci, typeClicked = type_} )
