module AudioModule.Translators exposing
  ( Translators
  , EndpointTranslators
  , ControlTranslators
  )

import MouseEvent

type alias Translators msg =
  { startDrag : MouseEvent.MouseInfo -> msg
  , endpointTranslators : Int -> EndpointTranslators msg
  , controlTranslators : Int -> ControlTranslators msg
  }

type alias EndpointTranslators msg =
  { createHalfConnection : MouseEvent.MouseInfo -> msg
  , hoverEndpoint : msg
  , unhoverEndpoint : msg
  }

type alias ControlTranslators msg =
  { controlClick : String -> msg
  , controlInput : String -> msg
  }
