module AudioModule.Control exposing
  ( Control
  , Msg
  , Translators
  , initControlGroup
  , initKeyboard
  , initRadio
  , initKnob
  , initNumber
  , withTranslators
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
initControlGroup : List (Control msg) -> String -> Control msg
initControlGroup controls =
  let
    initialValue = case (List.head controls) of
      Nothing ->
        ""
      Just control ->
        control.value
  in
    ControlGroup ( Array.fromList controls )
    |> initGeneric initialValue

initKeyboard : String -> Control msg
initKeyboard =
  Keyboard
  |> initGeneric ""

initRadio : String -> String -> String -> Control msg
initRadio value group =
  Radio { group = group }
  |> initGeneric value

initKnob : String -> Control msg
initKnob =
  Knob { max = Nothing, min = Nothing, intervals = Nothing }
  |> initGeneric "0"

initNumber : String -> Control msg
initNumber =
  Number { max = Nothing, min = Nothing, step = Nothing }
  |> initGeneric "0"

initGeneric : String -> Input msg -> String -> Control msg
initGeneric initialValue input id =
  { id = id
  , input = input
  , value = initialValue
  , label = Nothing
  , translators = Nothing
  }

withTranslators : Translators msg -> Control msg -> Control msg
withTranslators translators control =
  { control | translators = Just translators }

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
  , translators : Maybe (Translators msg)
  }

type Input msg
  = Radio RadioParameters
  | Number NumberParameters
  | Knob KnobParameters
  | Keyboard
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
      case control.translators of
        Nothing ->
          (control, Cmd.none)
        Just { loopback } ->
          ( control
          , Task.attempt ( loopback << Focus ) ( Dom.focus control.id )
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
          ] ++
          ( case translators of
            Nothing -> []
            Just { loopback } -> [ Html.Events.onInput (loopback << Value) ]
          )
        )
        [ ]
    Number { max, min, step } ->
      Html.input
        ( List.concat
        [ [ Attributes.id id
          , Attributes.type_ "number"
          , Attributes.property "value" <| Encode.string value
          ]
        , [ max |> Maybe.map (Attributes.attribute "max" << String.fromFloat)
          , min |> Maybe.map (Attributes.attribute "min" << String.fromFloat)
          , step |> Maybe.map (Attributes.attribute "step" << String.fromFloat)
          ] |> List.filterMap identity
        , case translators of
          Nothing ->
            [ ]
          Just { loopback } ->
            [ Html.Events.onInput (loopback << Value)
            , Html.Events.onClick (loopback Click)
            ]
        ] )
        [ ]
    Knob { max, min, intervals } ->
      Html.node "knob-control"
        ( List.concat
        [ [ Attributes.id id
          , Attributes.property "value" <| Encode.string value
          ]
        , [ max |> Maybe.map (Attributes.attribute "max" << String.fromFloat)
          , min |> Maybe.map (Attributes.attribute "min" << String.fromFloat)
          , intervals |> Maybe.map ( Attributes.attribute "intervals" << String.fromInt)
          ] |> List.filterMap identity
        , case translators of
          Nothing ->
            [ ]
          Just { loopback } ->
            [ Html.Events.on "input" (knobInputDecoder loopback) ]
        ] )
        [ ]
    Keyboard ->
      Html.node "keys-control" [ Attributes.tabindex -1 ] []
    ControlGroup controls ->
      Html.div
        [ Attributes.class "input-group" ]
        ( Array.map view controls |> Array.toList )

knobInputDecoder : (Msg -> msg) -> Decode.Decoder msg
knobInputDecoder delegate =
  Decode.map (delegate << Value << String.fromFloat)
  <| Decode.field "detail" Decode.float
