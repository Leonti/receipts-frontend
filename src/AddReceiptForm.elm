module AddReceiptForm exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (Authentication)
import Html.Events exposing (..)
import Html.App as App
import ReceiptForm
import Ports


type alias Model =
    { receiptFormModel : ReceiptForm.Model
    , fileSelected : Bool
    , previewDataUrl : Maybe String
    , uploading : Bool
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    let
        ( receiptFormModel, receiptFormCmd ) =
            ReceiptForm.init

        model =
            { receiptFormModel = receiptFormModel
            , fileSelected = False
            , previewDataUrl = Nothing
            , uploading = False
            }

        cmds =
            Cmd.batch [ Cmd.map ReceiptFormMsg receiptFormCmd ]
    in
        ( model, cmds )


type Msg
    = UploadReceipt
    | ReceiptFileInputStart
    | ReceiptFileChange Ports.FileToUpload
    | ReceiptUploaded Ports.CreateReceiptResult
    | ReceiptFormMsg ReceiptForm.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UploadReceipt ->
            ( { model
                | uploading = True
              }
            , Cmd.none
            )

        ReceiptFileChange fileToUpload ->
            case fileToUpload.imageDataUrl of
                Just imageDataUrl ->
                    ( { model
                        | previewDataUrl = Just imageDataUrl
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        ReceiptUploaded result ->
            ( { model
                | uploading = False
              }
            , Cmd.none
            )

        ReceiptFileInputStart ->
            ( model, Ports.receiptFileMouseDown "receipt-file" )

        ReceiptFormMsg message ->
            let
                ( receiptFormModel, receiptFormCmd ) =
                    ReceiptForm.update message model.receiptFormModel
            in
                ( { model
                    | receiptFormModel = receiptFormModel
                  }
                , Cmd.map ReceiptFormMsg receiptFormCmd
                )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Ports.receiptFileSelected ReceiptFileChange
        , Ports.receiptCreated ReceiptUploaded
        ]


view : Model -> Html Msg
view model =
    div []
        [ App.map ReceiptFormMsg (ReceiptForm.view model.receiptFormModel)
        , input [ type' "file", id "receipt-file", onMouseDown ReceiptFileInputStart ] []
        , button [ onClick UploadReceipt ] [ text "Create receipt" ]
        ]
