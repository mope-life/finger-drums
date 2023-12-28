module AudioModule.Endpoint exposing
  ( Endpoint
  , Direction(..)
  , Translators
  , init
  , view
  )

import Html
import Html.Attributes as Attributes
import MouseEvent

--------------------------------------------------------------------------------
-- Initialization --------------------------------------------------------------
init : Translators msg -> String -> Direction -> String -> Endpoint msg
init translators id direction label =
  { id = id
  , direction = direction
  , label = label
  , translators = translators
  }

--------------------------------------------------------------------------------
-- Model -----------------------------------------------------------------------
type alias Endpoint msg =
  { id : String
  , direction : Direction
  , label : String
  , translators : Translators msg
  }

type alias Translators msg =
  { mouseDown : MouseEvent.MouseInfo -> msg
  }

type Direction = In | Out

--------------------------------------------------------------------------------
-- View ------------------------------------------------------------------------
view : Endpoint msg -> Html.Html msg
view endpoint =
  Html.div
    ( [ Attributes.class "endpoint-wrapper"
      , Attributes.id endpoint.id
      , MouseEvent.onCustom "mousedown" endpoint.translators.mouseDown
      ]
    )
    [ Html.div
      [ Attributes.class "endpoint-jack", Attributes.class "grabbable" ]
      [ ]
    , Html.label
      [ Attributes.class "endpoint-label" ]
      [ Html.text endpoint.label ]
    ]
