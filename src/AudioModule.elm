module AudioModule exposing
  ( AudioModule
  , Msg
  , Type(..)
  , Position
  , init
  , at
  , movedBy
  , dragging
  , notDragging
  , asPrototype
  , update
  , view
  , viewBasic
  )

import Html
import Html.Attributes as Attributes
import Html.Events as Events

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init : Type -> AudioModule
init type_ =
  { type_ = type_
  , position = (0, 0)
  , isDragging = False
  , isPrototype = False
  }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias AudioModule =
  { type_ : Type
  , position : Position
  , isDragging : Bool
  , isPrototype : Bool
  }

type Msg = Placeholder

type Type
  = ControllerModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule

type alias Position = (Float, Float)

at : (Float, Float) -> AudioModule -> AudioModule
at (x, y) audioModule =
  { audioModule | position = ( x, y ) }

movedBy : (Float, Float) -> AudioModule -> AudioModule
movedBy (dx, dy) audioModule =
  { audioModule
  | position = audioModule.position
  |> Tuple.mapBoth (\x -> x + dx) (\y -> y + dy)
  }

dragging : AudioModule -> AudioModule
dragging audioModule =
  { audioModule | isDragging = True }

notDragging : AudioModule -> AudioModule
notDragging audioModule =
  { audioModule | isDragging = False }

asPrototype : AudioModule -> AudioModule
asPrototype audioModule =
  { audioModule | isPrototype = True }

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> AudioModule -> AudioModule
update msg audioModule =
  case msg of
    Placeholder -> audioModule

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : (Msg -> msg) -> List (Html.Attribute msg) -> AudioModule -> Html.Html msg
view msgWrapper extraAttributes audioModule =
  let
    eventAttributes = viewEvents msgWrapper audioModule
  in
    viewBasic (List.append eventAttributes extraAttributes) audioModule

viewBasic : List (Html.Attribute msg) -> AudioModule -> Html.Html msg
viewBasic extraAttributes audioModule =
  let
    pixels = (\s -> s ++ "px") << String.fromInt << round
    (xpx, ypx) = Tuple.mapBoth pixels pixels audioModule.position
    inputs = endpointsInFor audioModule.type_ |> \ list -> if List.isEmpty list then [] else (Html.text "in:") :: list
    outputs = endpointsOutFor audioModule.type_ |> \ list -> if List.isEmpty list then [] else (Html.text "out:") :: list
    controls = List.map htmlForControl (controlsFor audioModule.type_)

  in
    Html.div
    ( [ Just (Attributes.style "left" xpx)
      , Just (Attributes.style "top" ypx)
      , Just (Attributes.class "module-wrapper")
      , if audioModule.isDragging
        then Just (Attributes.class "dragging")
        else Nothing
      ] |> List.filterMap identity |> List.append extraAttributes
    )
    ( [ Just (Html.div [Attributes.class "endpoint-bank"] inputs)
      , Just (Html.div [Attributes.class "control-bank"] controls)
      , Just (Html.div [Attributes.class "endpoint-bank"] outputs)
      , if audioModule.isPrototype
        then Just ( Html.div [ Attributes.class "prototype-click-shield"] [] )
        else Nothing
      ] |> List.filterMap identity
    )

viewEvents : (Msg -> msg) -> AudioModule -> List (Html.Attribute msg)
viewEvents msgWrapper _ =
  [ Events.onClick (msgWrapper Placeholder) ]

endpointsInFor : Type -> List (Html.Html msg)
endpointsInFor type_ =
  case type_ of
    ControllerModule ->
      []
    DestinationModule ->
      [ viewEndpoint "signal" ]
    ConstantModule ->
      []
    VCOModule ->
      [ viewEndpoint "freq"
      , viewEndpoint "detune"
      ]
    VCAModule ->
      [ viewEndpoint "signal"
      , viewEndpoint "cv"
      ]
    EnvelopeModule ->
      [ viewEndpoint "gate"
      , viewEndpoint "trig"
      ]

endpointsOutFor : Type -> List (Html.Html msg)
endpointsOutFor type_ =
  case type_ of
    ControllerModule ->
      [ viewEndpoint "freq"
      , viewEndpoint "gate"
      , viewEndpoint "trig"
      ]
    DestinationModule ->
      []
    ConstantModule ->
      [ viewEndpoint "cv" ]
    VCOModule ->
      [ viewEndpoint "signal" ]
    VCAModule ->
      [ viewEndpoint "signal" ]
    EnvelopeModule ->
      [ viewEndpoint "level" ]

viewEndpoint : String -> Html.Html msg
viewEndpoint label =
  Html.div [ Attributes.class "endpoint-wrapper" ]
  [ Html.div [ Attributes.class "endpoint-jack", Attributes.class "grabbable" ] []
  , Html.label [ Attributes.class "endpoint-label" ] [ Html.text label ]
  ]


type alias Control =
  { innerControl : InnerControl
  , label : Maybe String
  }

type InnerControl
  = Radio String String Bool
  | Number Float Range NumericInput

type alias Range =
  { max : Float
  , min : Float
  , step : Float
  }

type NumericInput
  = Knob
  | Field

controlsFor : Type -> List Control
controlsFor type_ =
  case type_ of
    ControllerModule ->
      []
    DestinationModule ->
      []
    ConstantModule ->
      [ { innerControl = Number 0 (inputRange (Just 1) (Just 0) 0.001) Knob
        , label = Nothing
      } ]
    VCOModule ->
      [ { innerControl = Radio "osc-type" "sine" True, label = Just "sin" }
      , { innerControl = Radio "osc-type" "square" False, label = Just "sqr" }
      , { innerControl = Radio "osc-type" "sawtooth" False, label = Just "saw" }
      , { innerControl = Radio "osc-type" "triangle" False, label = Just "tri" }
      ]
    VCAModule ->
      []
    EnvelopeModule ->
      [ { innerControl = Number 0 (inputRange Nothing (Just 0) 0.001) Field
        , label = Just "attack"
        }
      , { innerControl = Number 0 (inputRange Nothing (Just 0) 0.001) Field
        , label = Just "decay"
        }
      , { innerControl = Number 0 (inputRange (Just 1) (Just 0) 0.001) Field
        , label = Just "sustain"
        }
      , { innerControl = Number 0 (inputRange Nothing (Just 0) 0.001) Field
        , label = Just "release"
        }
      ]

inputRange : Maybe Float -> Maybe Float -> Float -> Range
inputRange maybeMax maybeMin step =
  { max = Maybe.withDefault 3.4028234663852886e38 maybeMax
  , min = Maybe.withDefault -3.4028234663852886e38 maybeMin
  , step = step
  }

htmlForControl : Control -> Html.Html msg
htmlForControl { innerControl, label } =
  case label of
    Nothing ->
      innerHtmlForControl innerControl
    Just string ->
      Html.label [][ Html.text string, innerHtmlForControl innerControl ]

innerHtmlForControl : InnerControl -> Html.Html msg
innerHtmlForControl innerControl =
  case innerControl of
    Radio name value checked ->
      Html.input
        [ Attributes.type_ "radio"
        , Attributes.name name
        , Attributes.value value
        , Attributes.checked checked
        ]
        []
    Number value range input ->
      let
        commonAttrs =
          [ Attributes.min (String.fromFloat range.min)
          , Attributes.max (String.fromFloat range.max)
          , Attributes.step (String.fromFloat range.step)
          , Attributes.value (String.fromFloat value)
          ]
      in
      case input of
        Knob ->
          Html.node "knob-control" commonAttrs []
        Field ->
          Html.input (Attributes.type_ "number" :: commonAttrs) []
