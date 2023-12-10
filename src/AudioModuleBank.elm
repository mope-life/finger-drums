module AudioModuleBank exposing (main)

import Browser
import Html exposing (Html, text, div)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onMouseDown)


{- MAIN -}
main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { audioModules =
      [ ConstantModule
      , VCAModule
      , VCOModule
      , EnvelopeModule
      ]
      , elementsCreated = 0
    }
  , Cmd.none
  )


{- MODEL -}
type alias Model =
  { audioModules : List AudioModule
  , elementsCreated : Int
  }


type AudioModule
  = ControllerModule
  | DestinationModule
  | ConstantModule
  | VCOModule
  | VCAModule
  | EnvelopeModule
--| More to come!


type alias Msg = AudioModule


{- SUBSCRIPTIONS -}
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


{- UPDATE -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( model
  , Cmd.none
  )


{- VIEW -}
view : Model -> Html Msg
view model =
  div [id "module-prototype-bank"]
    (List.filterMap audioModulePrototype model.audioModules)


audioModulePrototype : AudioModule -> Maybe (Html Msg)
audioModulePrototype audioModule =
  case (audioModuleName audioModule) of
    Nothing
      -> Nothing

    Just string
      -> Just ( div
        [ class "module-prototype"
        , class "grabbable"
        , onMouseDown audioModule
        ]
        [ text string ]
      )


audioModuleName : AudioModule -> Maybe String
audioModuleName audioModule =
  case audioModule of
    ConstantModule -> Just "Const"
    VCOModule -> Just "VCO"
    VCAModule -> Just "VCA"
    EnvelopeModule -> Just "Envelope"

    {- These two modules are special; they are not created from the module
    bank, as there should only be one of each of them -}
    ControllerModule -> Nothing
    DestinationModule -> Nothing
