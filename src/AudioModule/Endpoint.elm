module AudioModule.Endpoint exposing
  ( Endpoint
  , Msg(..)
  , Direction(..)
  , init
  , withDelegate
  , update
  , view
  )

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode
import MouseEvent

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init : String -> Direction -> String -> Endpoint msg
init id direction label =
  { id = id
  , direction = direction
  , label = label
  , delegate = Nothing
  }

withDelegate : (Msg -> msg) -> Endpoint msg -> Endpoint msg
withDelegate delegate endpoint =
  { endpoint | delegate = Just delegate }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Endpoint msg =
  { id : String
  , direction : Direction
  , label : String
  , delegate : Maybe (Msg -> msg)
  }

type Msg
  = MouseDown MouseEvent.Point

type Direction
  = In
  | Out

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : Msg -> Endpoint msg -> (Endpoint msg, Cmd msg)
update msg endpoint =
  case msg of
    MouseDown point ->
      (endpoint, Cmd.none)

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Endpoint msg -> Html.Html msg
view endpoint =
  Html.div
    ( [ Attributes.class "endpoint-wrapper"
      , Attributes.id endpoint.id
      ]
      ++ viewEvents endpoint.delegate
    )
    [ Html.div
      [ Attributes.class "endpoint-jack", Attributes.class "grabbable" ]
      [ ]
    , Html.label
      [ Attributes.class "endpoint-label" ]
      [ Html.text endpoint.label ]
    ]

viewEvents : Maybe (Msg -> msg) -> List (Html.Attribute msg)
viewEvents delegate =
  case delegate of
    Nothing ->
      [ ]
    Just d ->
      [ onMouseDown d ]

onMouseDown : (Msg -> msg) -> Html.Attribute msg
onMouseDown delegate =
  MouseEvent.pointMessageDecoder (delegate << MouseDown)
  |> Decode.andThen
    (\m -> Decode.succeed
      { message = m
      , stopPropagation = True
      , preventDefault = True
      }
    )
  |> Events.custom "mousedown"
