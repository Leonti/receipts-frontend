port module Ports exposing (..)

type alias LoadImageParams =
    { url : String
    , authToken : String
    , fileId : String
    }

type alias LoadImageResult =
    { fileId : String
    , imageData : String
    }

port imageLoaded : (LoadImageResult -> msg) -> Sub msg
port loadImage : LoadImageParams -> Cmd msg
