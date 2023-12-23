module AudioModule.Control exposing
  ( Control
  , Msg
  , controlGroup
  , radioControl
  , knobControl
  , numberControl
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
controlGroup : List Control -> String -> Control
controlGroup controls id =
  let
    initialValue = case (List.head controls) of
      Nothing ->
        ""
      Just control ->
        control.value
  in
    ControlGroup ( Array.fromList controls )
    |> genericControl id initialValue

radioControl : String -> String -> String -> Control
radioControl value group id =
  Radio { group = group }
  |> genericControl id value

knobControl : String -> Control
knobControl id =
  Knob { max = Nothing, min = Nothing, intervals = Nothing }
  |> genericControl id "0"

numberControl : String -> Control
numberControl id =
  Number { max = Nothing, min = Nothing, step = Nothing }
  |> genericControl id "0"

genericControl : String -> String -> Input -> Control
genericControl id initialValue input =
  { id = id
  , input = input
  , value = initialValue
  , label = Nothing
  }

labeled : String -> Control -> Control
labeled label control =
  { control | label = Just label }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Control =
  { id : String
  , input : Input
  , value : String
  , label : Maybe String
  }

type Input
  = Radio RadioParameters
  | Number NumberParameters
  | Knob KnobParameters
  | ControlGroup (Array Control)

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
update : (Msg -> msg) -> Msg -> Control -> (Control, Cmd msg)
update delegate msg control =
  case msg of
    Value value ->
      Debug.log ("Received " ++ value) ( { control | value = value }, Cmd.none )
    Click ->
      case control.input of
        Number _ ->
          ( control, Task.attempt (delegate << Focus) (Dom.focus control.id) )
        _ ->
          ( control, Cmd.none )
    Focus result ->
      case result of
        Err ( Dom.NotFound string ) ->
          Debug.log string ( control, Cmd.none )
        Ok ( ) ->
          ( control, Cmd.none )

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Maybe (Msg -> msg) -> Control -> Html.Html msg
view delegate control =
  let
    input = viewInput delegate control
  in case control.label of
    Nothing ->
      input
    Just label ->
      Html.label [] [ Html.text label, input ]

viewInput : Maybe (Msg -> msg) -> Control -> Html.Html msg
viewInput delegate { id, input, value } =
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
        ( Array.map (view delegate) controls |> Array.toList )

knobInputDecoder : (Msg -> msg) -> Decode.Decoder msg
knobInputDecoder delegate =
  Decode.map (delegate << Value << String.fromFloat) <| Decode.field "detail" Decode.float

