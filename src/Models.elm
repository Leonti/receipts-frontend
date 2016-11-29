module Models exposing (..)

import Json.Decode exposing (field)
import Json.Decode as Json


type alias UserInfo =
    { id : String
    , username : String
    }


type alias Authentication =
    { userId : String
    , token : String
    }


type alias Receipt =
    { id : String
    , userId : String
    , files : List ReceiptFile
    , timestamp : Int
    , total : Maybe Float
    , description : String
    }


type alias ReceiptFormData =
    { total : Maybe Float
    , description : String
    }


type alias ReceiptFile =
    { id : String
    , ext : String
    , metaData : FileMetadata
    , timestamp : Int
    }


type alias FileMetadata =
    { fileType : String
    , length : Int
    , width : Int
    , height : Int
    }


type alias AppConfig =
    { googleClientId : String }


accessTokenDecoder : Json.Decoder String
accessTokenDecoder =
    Json.at [ "access_token" ] Json.string


userInfoDecoder : Json.Decoder UserInfo
userInfoDecoder =
    Json.map2 UserInfo
        (field "id" Json.string)
        (field "userName" Json.string)


receiptDecoder : Json.Decoder Receipt
receiptDecoder =
    Json.map6 Receipt
        (field "id" Json.string)
        (field "userId" Json.string)
        (field "files" (Json.list receiptFileDecoder))
        (field "timestamp" Json.int)
        (field "total" (nullOr Json.float))
        (field "description" Json.string)


receiptsDecoder : Json.Decoder (List Receipt)
receiptsDecoder =
    Json.list receiptDecoder


receiptFileDecoder : Json.Decoder ReceiptFile
receiptFileDecoder =
    Json.map4 ReceiptFile
        (field "id" Json.string)
        (field "ext" Json.string)
        (field "metaData" fileMetadataDecoder)
        (field "timestamp" Json.int)


fileMetadataDecoder : Json.Decoder FileMetadata
fileMetadataDecoder =
    Json.map4 FileMetadata
        (field "fileType" Json.string)
        (field "length" Json.int)
        (field "width" Json.int)
        (field "height" Json.int)


appConfigDecoder : Json.Decoder AppConfig
appConfigDecoder =
    Json.map AppConfig
        (field "googleClientId" Json.string)


nullOr : Json.Decoder a -> Json.Decoder (Maybe a)
nullOr decoder =
    Json.oneOf
        [ Json.null Nothing
        , Json.map Just decoder
        ]
