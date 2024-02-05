module AudioModule.Translators exposing ( Translators )

import MouseEvent

type alias Translators msg =
  { startDrag : MouseEvent.MouseInfo -> msg
  , createHalfConnection : Int -> MouseEvent.MouseInfo -> msg
  , hoverEndpoint : Int -> msg
  , unhoverEndpoint : Int -> msg
  , controlClick : String -> msg
  , controlInput : Int -> String -> msg
  }
