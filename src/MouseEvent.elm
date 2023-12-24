module MouseEvent exposing
  ( MouseInfo
  , Point
  , Position
  , mouseEventDecoder
  , positionDecoder
  , pointDecoder
  , pointMessageDecoder
  , positionMessageDecoder
  )

import Json.Decode exposing (Decoder)
import Json.Decode as Decode

type alias MouseInfo =
  { offsetX : Float
  , offsetY : Float
  , pageX : Float
  , pageY : Float
  }

type alias Point = (Float, Float)

type alias Position = (Float, Float)

mouseEventDecoder : Decoder MouseInfo
mouseEventDecoder =
  Decode.map4 MouseInfo
    ( Decode.field "offsetX" Decode.float )
    ( Decode.field "offsetY" Decode.float )
    ( Decode.field "pageX" Decode.float )
    ( Decode.field "pageY" Decode.float )

pointMessageDecoder : (Point -> msg) -> Decoder msg
pointMessageDecoder delegate =
  pointDecoder |> Decode.map delegate

pointDecoder : Decoder Point
pointDecoder =
  mouseEventDecoder
  |> Decode.map (\mouseInfo -> (mouseInfo.pageX, mouseInfo.pageY))

positionMessageDecoder : (Position -> msg) -> Decoder msg
positionMessageDecoder delegate =
  positionDecoder |> Decode.map delegate

positionDecoder : Decoder Position
positionDecoder =
  mouseEventDecoder
  |> Decode.map
    (\mouseEvent ->
      ( mouseEvent.pageX - mouseEvent.offsetX
      , mouseEvent.pageY - mouseEvent.offsetY
      )
    )
