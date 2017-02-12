module MousePosition
    exposing
        ( Offset
        , onMouseMove
        , onMouseDown
        , onMouseUp
        )

import Html
import Html.Events exposing (on)
import Json.Decode exposing (field)
import Json.Decode as Decode


type alias Target =
    { offsetWidth : Int
    , offsetHeight : Int
    }


type alias Offset =
    { offsetX : Int
    , offsetY : Int
    , pageX : Int
    , pageY : Int
    , target : Target
    }


eventToOffsetDecoder : Decode.Decoder Offset
eventToOffsetDecoder =
    Decode.map5
        Offset
        (field "offsetX" Decode.int)
        (field "offsetY" Decode.int)
        (field "pageX" Decode.int)
        (field "pageY" Decode.int)
        (field "target" targetDecoder)


targetDecoder : Decode.Decoder Target
targetDecoder =
    Decode.map2
        Target
        (field "offsetWidth" Decode.int)
        (field "offsetHeight" Decode.int)


offsetWidth : Decode.Decoder Float
offsetWidth =
    field "offsetWidth" Decode.float


offsetHeight : Decode.Decoder Float
offsetHeight =
    field "offsetHeight" Decode.float


target : Decode.Decoder a -> Decode.Decoder a
target decoder =
    field "target" decoder


onMouseMove : (Offset -> msg) -> Html.Attribute msg
onMouseMove target =
    on "mousemove" (Decode.map target eventToOffsetDecoder)


onMouseDown : (Offset -> msg) -> Html.Attribute msg
onMouseDown target =
    on "mousedown" (Decode.map target eventToOffsetDecoder)


onMouseUp : (Offset -> msg) -> Html.Attribute msg
onMouseUp target =
    on "mouseup" (Decode.map target eventToOffsetDecoder)
