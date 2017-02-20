port module Ports exposing (..)

import Models exposing (ReceiptFormData)


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


type alias FileToUpload =
    { isImage : Bool
    , imageDataUrl : Maybe String
    }


port receiptFileMouseDown : String -> Cmd msg


port receiptFileSelected : (FileToUpload -> msg) -> Sub msg


port createReceipt : CreateReceiptParams -> Cmd msg


port receiptCreated : (CreateReceiptResult -> msg) -> Sub msg


port initDownload : String -> Cmd msg


port showDialog : String -> Cmd msg
