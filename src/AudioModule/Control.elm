module AudioModule.Control exposing
  ( Control
  , Msg
  , initControlGroup
  , initRadio
  , initKnob
  , initNumber
  , labeled
  , update
  , view
  )

import Html
import Html.Attributes as Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Browser.Dom as Dom
import Array exposing (Array)

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
initControlGroup :
  List (Control msg)
  -> String
  -> Translators msg
  -> Control msg
initControlGroup controls id =
  let
    initialValue = case (List.head controls) of
      Nothing ->
        ""
      Just control ->
        control.value
  in
    ControlGroup ( Array.fromList controls )
    |> initGeneric id initialValue

initRadio : String -> String -> String -> Translators msg -> Control msg
initRadio value group id =
  Radio { group = group }
  |> initGeneric id value

initKnob : String -> Translators msg -> Control msg
initKnob id =
  Knob { max = Nothing, min = Nothing, intervals = Nothing }
  |> initGeneric id "0"

initNumber : String -> Translators msg -> Control msg
initNumber id =
  Number { max = Nothing, min = Nothing, step = Nothing }
  |> initGeneric id "0"

initGeneric : String -> String -> Input msg -> Translators msg -> Control msg
initGeneric id initialValue input translators =
  { id = id
  , input = input
  , value = initialValue
  , label = Nothing
  , translators = translators
  }

labeled : String -> Control msg -> Control msg
labeled label control =
  { control | label = Just label }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Control msg =
  { id : String
  , input : Input msg
  , value : String
  , label : Maybe String
  , translators : Translators msg
  }

type Input msg
  = Radio RadioParameters
  | Number NumberParameters
  | Knob KnobParameters
  | ControlGroup (Array (Control msg))

type alias RadioParameters =
  { group : String
  }

type alias NumberParameters =
  { max : Maybe Float
  , min : Maybe Float
  , step : Maybe Float
  }

type alias KnobParameters =
  { max : Maybe Float
  , min : Maybe Float
  , intervals : Maybe Int
  }

type alias Translators msg =
  { loopback : Msg -> msg
  , input : String -> msg
  }

type Msg
  = Value String
  | Click
  | Focus (Result Dom.Error ())

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> Control msg -> (Control msg, Cmd msg)
update msg control =
  case msg of
    Value value ->
      ( { control | value = value }, Cmd.none )
    Click ->
      ( control
      , Task.attempt
        ( control.translators.loopback << Focus )
        ( Dom.focus control.id )
      )
    Focus _ ->
      ( control, Cmd.none )

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Control msg -> Html.Html msg
view control =
  let
    input = viewInput control
  in case control.label of
    Nothing ->
      input
    Just label ->
      Html.label [] [ Html.text label, input ]

viewInput : Control msg -> Html.Html msg
viewInput { id, input, value, translators } =
  case input of
    Radio { group } ->
      Html.input
        ( [ Attributes.id id
          , Attributes.type_ "radio"
          , Attributes.name group
          , Attributes.value value
          , Html.Events.onInput (translators.loopback << Value)
          ]
        )
        []
    Number { max, min, step } ->
      Html.input
        ( [ Attributes.id id
          , Attributes.type_ "number"
          , Attributes.property "value" <| Encode.string value
          , Html.Events.onClick (translators.loopback Click)
          , Html.Events.onInput (translators.loopback << Value)
          ] ++
          ( [ max |> Maybe.map ( Attributes.attribute "max" << String.fromFloat )
            , min |> Maybe.map ( Attributes.attribute "min" << String.fromFloat )
            , step |> Maybe.map ( Attributes.attribute "step" << String.fromFloat )
            ] |> List.filterMap identity
          )
        )
        []
    Knob { max, min, intervals } ->
      Html.node "knob-control"
        ( [ Attributes.id id
          , Attributes.property "value" <| Encode.string value
          , Html.Events.on "input" (knobInputDecoder translators.loopback)
          ] ++
          ( [ max |> Maybe.map ( Attributes.attribute "max" << String.fromFloat )
            , min |> Maybe.map ( Attributes.attribute "min" << String.fromFloat )
            , intervals |> Maybe.map ( Attributes.attribute "intervals" << String.fromInt )
            ] |> List.filterMap identity
          )
        )
        []
    ControlGroup controls ->
      Html.div
        [ Attributes.class "input-group" ]
        ( Array.map view controls |> Array.toList )

knobInputDecoder : (Msg -> msg) -> Decode.Decoder msg
knobInputDecoder delegate =
  Decode.map (delegate << Value << String.fromFloat)
  <| Decode.field "detail" Decode.float

