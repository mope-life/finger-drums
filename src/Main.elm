module Main exposing (main)
import Browser

--MAIN
main = Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

init : () -> ( Model, Cmd msg )
init _ =
    ( {}, Cmd.none )

-- MODEL
type alias Model =
    {
    }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none

-- UPDATE
update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( {}, Cmd.none )

-- VIEW
view : Model -> Browser.Document msg
view model =
    { title = "Hello, world!"
    , body = []
    }
