port module Ports exposing (..)

import Models exposing (ReceiptFormData)


type alias LoadImageParams =
    { url : String
    , authToken : String
    , fileId : String
    }


type alias LoadImageResult =
    { fileId : String
    , imageData : String
    }


type alias CreateReceiptParams =
    { receiptDetails : ReceiptFormData
    , fileInputId : String
    , url : String
    , authToken : String
    }


type alias CreateReceiptResult =
    { receiptId : String
    , error : Maybe String
    }


port loadImage : LoadImageParams -> Cmd msg


port imageLoaded : (LoadImageResult -> msg) -> Sub msg


port createReceipt : CreateReceiptParams -> Cmd msg


port receiptCreated : (CreateReceiptResult -> msg) -> Sub msg
