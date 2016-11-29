module AddReceiptForm exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (Authentication)
import ReceiptForm
import Ports
import Api


fileInputId : String
fileInputId =
    "receipt-file"


type alias Model =
    { authentication : Authentication
    , receiptFormModel : ReceiptForm.Model
    , fileSelected : Bool
    , maybePreviewDataUrl : Maybe String
    , uploading : Bool
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    let
        ( receiptFormModel, receiptFormCmd ) =
            ReceiptForm.init

        model =
            { authentication = authentication
            , receiptFormModel = receiptFormModel
            , fileSelected = False
            , maybePreviewDataUrl = Nothing
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
            , Ports.createReceipt <| createReceiptParams model
            )

        ReceiptFileChange fileToUpload ->
            case fileToUpload.imageDataUrl of
                Just imageDataUrl ->
                    ( { model
                        | fileSelected = True
                        , maybePreviewDataUrl = Just imageDataUrl
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
            ( model, Ports.receiptFileMouseDown fileInputId )

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


createReceiptParams : Model -> Ports.CreateReceiptParams
createReceiptParams model =
    { receiptDetails = ReceiptForm.formData model.receiptFormModel
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


view : Model -> Html Msg
view model =
    div []
        [ imagePreview model.maybePreviewDataUrl
        , Html.map ReceiptFormMsg (ReceiptForm.view model.receiptFormModel)
        , input [ type_ "file", id fileInputId, onMouseDown ReceiptFileInputStart ] []
        , button [ onClick UploadReceipt, disabled <| not model.fileSelected ] [ text "Create receipt" ]
        ]


imagePreview : Maybe String -> Html Msg
imagePreview maybePreviewDataUrl =
    case maybePreviewDataUrl of
        Just previewDataUrl ->
            img [ src previewDataUrl ] []

        Nothing ->
            Html.text ""
