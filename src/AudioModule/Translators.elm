module AudioModule.Translators exposing ( Translators, EndpointTranslators )

import MouseEvent

type alias Translators msg =
  { startDrag : MouseEvent.MouseInfo -> msg
  , controlClick : String -> msg
  , controlInput : Int -> String -> msg
  , endpointTranslators : Int -> EndpointTranslators msg
  }

type alias EndpointTranslators msg =
  { createHalfConnection : MouseEvent.MouseInfo -> msg
  , hoverEndpoint : msg
  , unhoverEndpoint : msg
  }
