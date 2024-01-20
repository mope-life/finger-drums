module MouseEvent exposing
  ( MouseInfo
  , mouseInfoDecoder
  , messageDecoder
  , customMessageDecoder
  , onCustom
  )

import Json.Decode exposing (Decoder)
import Json.Decode as Decode
import Html
import Html.Events as Events

type alias MouseInfo =
  { offsetX : Float
  , offsetY : Float
  , clientX : Float
  , clientY : Float
  , targetWidth : Float
  , targetHeight : Float
  }

onCustom : String -> (MouseInfo -> msg) -> Html.Attribute msg
onCustom event delegate =
  Events.custom event (customMessageDecoder delegate)

customMessageDecoder :
  (MouseInfo -> msg)
  -> Decoder
    { message : msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }
customMessageDecoder delegate =
  Decode.map
    (\clickInfo ->
      { message = (delegate clickInfo)
      , stopPropagation = True
      , preventDefault = True
      }
    )
    mouseInfoDecoder

messageDecoder : (MouseInfo -> msg) -> Decoder msg
messageDecoder delegate =
  mouseInfoDecoder |> Decode.map delegate

mouseInfoDecoder : Decoder MouseInfo
mouseInfoDecoder =
  Decode.map6 MouseInfo
    ( Decode.field "offsetX" Decode.float )
    ( Decode.field "offsetY" Decode.float )
    ( Decode.field "clientX" Decode.float )
    ( Decode.field "clientY" Decode.float )
    ( Decode.at ["target", "clientWidth"] Decode.float )
    ( Decode.at ["target", "clientHeight"] Decode.float )
