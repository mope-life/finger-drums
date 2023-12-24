module AudioModule.Endpoint exposing
  ( Endpoint
  , Msg(..)
  , Direction(..)
  , init
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
init : String -> Direction -> String -> Endpoint
init id direction label =
  { id = id, direction = direction, label = label }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Endpoint =
  { id : String
  , direction : Direction
  , label : String
  }

type Msg
  = MouseDown MouseEvent.Point

type Direction
  = In
  | Out

--------------------------------------------------------------------------------
-- Update ----------------------------------------------------------------------
update : (Msg -> msg) -> Msg -> Endpoint -> (Endpoint, Cmd msg)
update delegate msg endpoint =
  case msg of
    MouseDown point ->
      (endpoint, Cmd.none)

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Maybe (Msg -> msg) -> Endpoint -> Html.Html msg
view delegate endpoint =
  Html.div
    ( Attributes.class "endpoint-wrapper" :: viewEvents delegate )
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
