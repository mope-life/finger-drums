module AudioModule.Control exposing
  ( Control
  , Msg
  , initControlGroup
  , initRadio
  , initKnob
  , initNumber
  , labeled
  , withDelegate
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
initControlGroup : List (Control msg) -> String -> Control msg
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

initRadio : String -> String -> String -> Control msg
initRadio value group id =
  Radio { group = group }
  |> initGeneric id value

initKnob : String -> Control msg
initKnob id =
  Knob { max = Nothing, min = Nothing, intervals = Nothing }
  |> initGeneric id "0"

initNumber : String -> Control msg
initNumber id =
  Number { max = Nothing, min = Nothing, step = Nothing }
  |> initGeneric id "0"

initGeneric : String -> String -> Input msg -> Control msg
initGeneric id initialValue input =
  { id = id
  , input = input
  , value = initialValue
  , label = Nothing
  , delegate = Nothing
  }

labeled : String ->Control msg-> Control  msg
labeled label control =
  { control | label = Just label }

withDelegate : (Msg -> msg) -> Control msg -> Control msg
withDelegate delegate control =
  { control | delegate = Just delegate }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Control msg =
  { id : String
  , input : Input msg
  , value : String
  , label : Maybe String
  , delegate : Maybe (Msg -> msg)
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
      case control.input of
        Number _ ->
          ( control
          , case control.delegate of
            Nothing ->
              Cmd.none
            Just delegate ->
              Task.attempt (delegate << Focus) (Dom.focus control.id)
          )
        _ ->
          ( control, Cmd.none )
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
viewInput { id, input, value, delegate } =
  case input of
    Radio { group } ->
      Html.input
        ( [ id |> Just << Attributes.id
          , "radio" |> Just << Attributes.type_
          , group |> Just << Attributes.name
          , value |> Just << Attributes.value
          , delegate |> Maybe.map (\d -> Html.Events.onInput (d << Value))
          ] |> List.filterMap identity
        )
        []
    Number { max, min, step } ->
      Html.input
        ( [ id |> Just << Attributes.id
          , "number" |> Just << Attributes.type_
          , value |> Just << Attributes.property "value" << Encode.string
          , max |> Maybe.map ( Attributes.attribute "max" << String.fromFloat )
          , min |> Maybe.map ( Attributes.attribute "min" << String.fromFloat )
          , step |> Maybe.map ( Attributes.attribute "step" << String.fromFloat )
          , delegate |> Maybe.map (\d -> Html.Events.onClick (d Click))
          , delegate |> Maybe.map (\d -> Html.Events.onInput (d << Value))
          ] |> List.filterMap identity
        )
        []
    Knob { max, min, intervals } ->
      Html.node "knob-control"
        ( [ id |> Just << Attributes.id
          , value |> Just << Attributes.property "value" << Encode.string
          , max |> Maybe.map ( Attributes.attribute "max" << String.fromFloat )
          , min |> Maybe.map ( Attributes.attribute "min" << String.fromFloat )
          , intervals |> Maybe.map ( Attributes.attribute "intervals" << String.fromInt )
          , delegate |> Maybe.map ( Html.Events.on "input" << knobInputDecoder )
          ] |> List.filterMap identity
        )
        []
    ControlGroup controls ->
      Html.div
        [ Attributes.class "input-group" ]
        ( Array.map view controls |> Array.toList )

knobInputDecoder : (Msg -> msg) -> Decode.Decoder msg
knobInputDecoder delegate =
  Decode.map (delegate << Value << String.fromFloat) <| Decode.field "detail" Decode.float

