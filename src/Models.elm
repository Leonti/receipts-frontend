module Models exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode as Json

type alias UserInfo =
    { id : String
    , username : String
    }

type alias Receipt =
    { id : String
    , userId : String
    , files : List ReceiptFile
    , timestamp : Int
    , total : Maybe Float
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

accessTokenDecoder : Json.Decoder String
accessTokenDecoder = Json.at["access_token"] Json.string

userInfoDecoder : Json.Decoder UserInfo
userInfoDecoder =
    Json.object2 UserInfo
        ("id" := Json.string)
        ("userName" := Json.string)

receiptDecoder : Json.Decoder Receipt
receiptDecoder =
    Json.object6 Receipt
        ("id" := Json.string)
        ("userId" := Json.string)
        ("files" := Json.list receiptFileDecoder)
        ("timestamp" := Json.int)
        ("total" := nullOr Json.float)
        ("description" := Json.string)

receiptsDecoder : Json.Decoder (List Receipt)
receiptsDecoder = Json.list receiptDecoder

receiptFileDecoder : Json.Decoder ReceiptFile
receiptFileDecoder =
    Json.object4 ReceiptFile
        ("id" := Json.string)
        ("ext" := Json.string)
        ("metaData" := fileMetadataDecoder)
        ("timestamp" := Json.int)

fileMetadataDecoder : Json.Decoder FileMetadata
fileMetadataDecoder =
    Json.object4 FileMetadata
        ("fileType" := Json.string)
        ("length" := Json.int)
        ("width" := Json.int)
        ("height" := Json.int)

nullOr : Json.Decoder a -> Json.Decoder (Maybe a)
nullOr decoder =
    Json.oneOf
    [ Json.null Nothing
    , Json.map Just decoder
    ]
