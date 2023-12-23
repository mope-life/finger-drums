module AudioModule.Control exposing
  ( Control
  , Msg
  , radioControl
  , knobControl
  , numberControl
  , checked
  , unchecked
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

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
radioControl : String -> String -> String -> Control
radioControl id group value =
  Radio group value False
  |> genericControl id

knobControl : String -> Control
knobControl id =
  Knob "0" { max = Nothing, min = Nothing, intervals = Nothing }
  |> genericControl id

numberControl : String -> Control
numberControl id =
  Number "0" { max = Nothing, min = Nothing, step = Nothing }
  |> genericControl id

genericControl : String -> Input -> Control
genericControl id input =
  { input = input
  , label = Nothing
  , id = id
  }

checked : Control -> Control
checked control =
  case control.input of
    Radio group value _ ->
      { control | input = Radio group value True }
    _ ->
      control

unchecked : Control -> Control
unchecked control =
  case control.input of
    Radio group value _ ->
      { control | input = Radio group value False }
    _ ->
      control

labeled : String -> Control -> Control
labeled label control =
  { control | label = Just label }


--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Control =
  { input : Input
  , id : String
  , label : Maybe String
  }

type Input
  = Radio String String Bool
  | Number String NumberParameters
  | Knob String KnobParameters

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
      case control.input of
        Knob _ parameters ->
          ({ control | input = Knob value parameters }, Cmd.none)
        Number _ parameters ->
          ({ control | input = Number value parameters }, Cmd.none)
        _ ->
          (control, Cmd.none)
    Click ->
      case control.input of
        Number _ _ ->
          (control, Task.attempt (delegate << Focus) (Dom.focus control.id))
        _ ->
          (control, Cmd.none)
    Focus result ->
      case result of
        Err (Dom.NotFound string) ->
          Debug.log string (control, Cmd.none)
        Ok () ->
          (control, Cmd.none)

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Maybe (Msg -> msg) -> Control -> Html.Html msg
view delegate control =
  let
    input = viewInput delegate control.id control.input
  in case control.label of
    Nothing ->
      input
    Just label ->
      Html.label [] [ Html.text label, input ]

viewInput : Maybe (Msg -> msg) -> String -> Input -> Html.Html msg
viewInput delegate id input =
  case input of
    Radio name value on ->
      Html.input
        [ Attributes.type_ "radio"
        , Attributes.name name
        , Attributes.value value
        -- , Attributes.checked on
        , Attributes.id id
        ]
        []
    Number value { max, min, step } ->
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
    Knob value { max, min, intervals } ->
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

knobInputDecoder : (Msg -> msg) -> Decode.Decoder msg
knobInputDecoder delegate =
  Decode.map (delegate << Value << String.fromFloat) <| Decode.field "detail" Decode.float

