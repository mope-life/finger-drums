module AudioModule.Control exposing
  ( Control
  , Msg
  , radioControl
  , knobControl
  , numberControl
  , checked
  , unchecked
  , labeled
  , unlabeled
  , update
  , view
  )

import Html
import Html.Attributes as Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
radioControl : String -> String -> Control
radioControl group value =
  { input = Radio group value False
  , label = Nothing
  }

knobControl : Control
knobControl =
  { input = Knob 0 Nothing Nothing Nothing
  , label = Nothing
  }

numberControl : Control
numberControl =
  { input = Number 0 Nothing Nothing Nothing
  , label = Nothing
  }

checked : Control -> Control
checked { input, label } =
  case input of
    Radio group value _ ->
      { input = Radio group value True, label = label }
    _ ->
      { input = input, label = label }

unchecked : Control -> Control
unchecked { input, label } =
  case input of
    Radio group value _ ->
      { input = Radio group value False, label = label }
    _ ->
      { input = input, label = label }

labeled : String -> Control -> Control
labeled label control =
  { control | label = Just label }

unlabeled : Control -> Control
unlabeled control =
  { control | label = Nothing }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Control =
  { input : Input
  , label : Maybe String
  }

type Input
  = Radio String String Bool
  | Number Float (Maybe Float) (Maybe Float) (Maybe Float)
  | Knob Float (Maybe Float) (Maybe Float) (Maybe Int)

type Msg
  = NumbericValue Float

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : (Msg -> msg) -> Msg -> Control -> (Control, Cmd msg)
update delegate msg control =
  case msg of
    NumbericValue value ->
      case control.input of
        Knob _ max min intervals ->
          ({ control | input = Knob value max min intervals }, Cmd.none)
        Number _ max min step ->
          ({ control | input = Number value max min step }, Cmd.none)
        _ ->
          (control, Cmd.none)

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Maybe (Msg -> msg) -> Control -> Html.Html msg
view delegate { input, label } =
  case label of
    Nothing ->
      viewInput delegate input
    Just string ->
      viewLabeledInput delegate input string

viewLabeledInput : Maybe (Msg -> msg) -> Input -> String -> Html.Html msg
viewLabeledInput delegate input label =
  Html.label [] [ Html.text label, viewInput delegate input ]

viewInput : Maybe (Msg -> msg) -> Input -> Html.Html msg
viewInput delegate input =
  case input of
    Radio name value on ->
      Html.input
        [ Attributes.type_ "radio"
        , Attributes.name name
        , Attributes.value value
        , Attributes.checked on
        ]
        []
    Number value max min step ->
      Html.input
        ( [ "number" |> Just << Attributes.type_
          , value |> Just << Attributes.property "value" << Encode.float
          , max |> Maybe.map (Attributes.attribute "max" << String.fromFloat)
          , min |> Maybe.map (Attributes.attribute "min" << String.fromFloat)
          , step |> Maybe.map (Attributes.attribute "step" << String.fromFloat)
          ] |> List.filterMap identity
        )
        []
    Knob value max min intervals ->
      Html.node "knob-control"
        ( [ value |> Just << Attributes.property "value" << Encode.float
          , max |> Maybe.map ( Attributes.attribute "max" << String.fromFloat )
          , min |> Maybe.map ( Attributes.attribute "min" << String.fromFloat )
          , intervals |> Maybe.map ( Attributes.attribute "intervals" << String.fromInt )
          , delegate |> Maybe.map ( Html.Events.on "input" << knobInputDecoder )
          ] |> List.filterMap identity
        )
        []

knobInputDecoder : (Msg -> msg) -> Decode.Decoder msg
knobInputDecoder delegate =
  Decode.map (delegate << NumbericValue) <| Decode.field "detail" Decode.float
