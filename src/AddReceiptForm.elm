module AddReceiptForm exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Task
import Models exposing (Authentication, ReceiptFormData)
import Ports
import Api


fileInputId : String
fileInputId =
    "receipt-file"


type alias Model =
    { authentication : Authentication
    , fileSelected : Bool
    , currentTime : Float
    , uploading : Bool
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    let
        model =
            { authentication = authentication
            , fileSelected = False
            , currentTime = 0
            , uploading = False
            }

        timeCmd =
            Task.perform CurrentTime Time.now

        cmds =
            Cmd.batch [ timeCmd ]
    in
        ( model, cmds )


type Msg
    = CurrentTime Time
    | UploadReceipt
    | ReceiptFileInputStart
    | ReceiptFileChange Ports.FileToUpload
    | ReceiptUploaded Ports.CreateReceiptResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime time ->
            ( { model
                | currentTime = Time.inMilliseconds time
              }
            , Cmd.none
            )

        UploadReceipt ->
            ( { model
                | uploading = True
              }
            , Ports.createReceipt <| createReceiptParams model
            )

        ReceiptFileChange fileToUpload ->
            ( { model
                | fileSelected = True
              }
            , Cmd.none
            )

        ReceiptUploaded result ->
            ( { model
                | uploading = False
              }
            , Cmd.none
            )

        ReceiptFileInputStart ->
            ( model, Ports.receiptFileMouseDown fileInputId )


createReceiptParams : Model -> Ports.CreateReceiptParams
createReceiptParams model =
    { receiptDetails = receiptFormData model.currentTime
    , fileInputId = fileInputId
    , url = Api.createReceiptUrl model.authentication.userId
    , authToken = model.authentication.token
    }


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Ports.receiptFileSelected ReceiptFileChange
        , Ports.receiptCreated ReceiptUploaded
        ]


receiptFormData : Time.Time -> ReceiptFormData
receiptFormData time =
    { total = Nothing
    , description = ""
    , transactionTime = round <| Time.inMilliseconds time
    , tags = []
    }


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "file", id fileInputId, onMouseDown ReceiptFileInputStart ] []
        , button [ onClick UploadReceipt, disabled <| not model.fileSelected ] [ text "Create receipt" ]
        ]
