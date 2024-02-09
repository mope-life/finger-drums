module AudioModule.Endpoint exposing
  ( Endpoint
  , Direction(..)
  , init
  , view
  )

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import AudioModule.Translators exposing (EndpointTranslators)
import MouseEvent
import Utility exposing (..)

init : Direction -> String -> String -> Endpoint
init direction label htmlId =
  { htmlId = htmlId
  , direction = direction
  , label = label
  , midpoint = ( 0, 0 )
  }

type alias Endpoint =
  { htmlId : String
  , direction : Direction
  , label : String
  , midpoint : Vec2
  }

type Direction = In | Out

view : Maybe ( EndpointTranslators msg ) -> Endpoint -> Html.Html msg
view maybeTranslators endpoint =
  Html.div
    [ Attributes.class "endpoint-wrapper" ]
    [ Html.div
      ( maybeTranslators
      |> Maybe.map
        (\{ createHalfConnection, hoverEndpoint, unhoverEndpoint } ->
          [ MouseEvent.onCustom "mousedown" createHalfConnection
          , Events.onMouseEnter hoverEndpoint
          , Events.onMouseLeave unhoverEndpoint
          ]
        )
      |> Maybe.withDefault []
      |> List.append
        [ Attributes.class "endpoint-jack"
        , Attributes.class "grabbable"
        , Attributes.id endpoint.htmlId
        ]
      )
      [ ]
    , Html.label
      [ Attributes.class "endpoint-label" ]
      [ Html.text endpoint.label ]
    ]
