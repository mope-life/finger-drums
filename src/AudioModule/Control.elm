module AudioModule.Control exposing
  ( Control
  , initControlGroup
  , initKeyboard
  , initRadio
  , initKnob
  , initNumber
  , labeled
  , view
  )

import Array exposing (Array)
import Json.Decode as Decode
import Json.Encode as Encode
import Html
import Html.Events as Events
import Html.Attributes as Attributes
import AudioModule.Translators exposing (ControlTranslators)

initControlGroup : List Control -> String -> Control
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

initKeyboard : String -> Control
initKeyboard =
  Keyboard
  |> initGeneric ""

initRadio : String -> String -> String -> Control
initRadio value group =
  Radio { group = group }
  |> initGeneric value

initKnob : String -> Control
initKnob =
  Knob { max = Nothing, min = Nothing, intervals = Nothing }
  |> initGeneric "0"

initNumber : String -> Control
initNumber =
  Number { max = Nothing, min = Nothing, step = Nothing }
  |> initGeneric "0"

initGeneric : String -> Input -> String -> Control
initGeneric initialValue input htmlId =
  { htmlId = htmlId
  , input = input
  , value = initialValue
  , label = Nothing
  }

labeled : String -> Control -> Control
labeled label control =
  { control | label = Just label }

type alias Control =
  { htmlId : String
  , input : Input
  , value : String
  , label : Maybe String
  }

type Input
  = Radio RadioParameters
  | Number NumberParameters
  | Knob KnobParameters
  | Keyboard
  | ControlGroup ( Array Control )

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

view : Maybe ( ControlTranslators msg ) -> Control -> Html.Html msg
view maybeTranslators control =
  let
    input = viewInput maybeTranslators control
  in case control.label of
    Nothing -> input
    Just label -> Html.label [ ] [ Html.text label, input ]

viewInput : Maybe ( ControlTranslators msg ) -> Control -> Html.Html msg
viewInput maybeTranslators { htmlId, input, value } =
  case input of
    Radio { group } ->
      Html.input
        ( [ Attributes.id htmlId
          , Attributes.type_ "radio"
          , Attributes.name group
          , Attributes.value value
          ] ++
          ( case maybeTranslators of
            Nothing -> [ ]
            Just { controlInput } -> [ Events.onInput controlInput ]
          )
        )
        [ ]
    Number { max, min, step } ->
      Html.input
        ( List.concat
        [ [ Attributes.id htmlId
          , Attributes.type_ "number"
          , Attributes.property "value" <| Encode.string value
          ]
        , [ max |> Maybe.map ( Attributes.attribute "max" << String.fromFloat )
          , min |> Maybe.map ( Attributes.attribute "min" << String.fromFloat )
          , step |> Maybe.map ( Attributes.attribute "step" << String.fromFloat )
          ] |> List.filterMap identity
        , case maybeTranslators of
          Nothing ->
            [ ]
          Just { controlInput, controlClick } ->
            [ Events.onInput controlInput
            , Events.onClick ( controlClick htmlId )
            ]
        ] )
        [ ]
    Knob { max, min, intervals } ->
      Html.node "knob-control"
        ( List.concat
        [ [ Attributes.id htmlId
          , Attributes.property "value" <| Encode.string value
          ]
        , [ max |> Maybe.map ( Attributes.attribute "max" << String.fromFloat )
          , min |> Maybe.map ( Attributes.attribute "min" << String.fromFloat )
          , intervals |> Maybe.map (  Attributes.attribute "intervals" << String.fromInt )
          ] |> List.filterMap identity
        , case maybeTranslators of
          Nothing ->
            [ ]
          Just { controlInput } ->
            [ Events.on "input" ( knobInputDecoder controlInput ) ]
        ] )
        [ ]
    Keyboard ->
      Html.node "keys-control" [ Attributes.tabindex -1 ] [ ]
    ControlGroup controls ->
      Html.div
        [ Attributes.class "input-group" ]
        ( Array.map ( view maybeTranslators ) controls |> Array.toList )

knobInputDecoder : (String -> msg) -> Decode.Decoder msg
knobInputDecoder delegate =
  Decode.map (delegate << String.fromFloat)
  <| Decode.field "detail" Decode.float
