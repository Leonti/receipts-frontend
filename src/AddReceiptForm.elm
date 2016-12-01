module AddReceiptForm exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Task
import Models exposing (Authentication, ReceiptFormData)
import ReceiptForm
import Ports
import Api


fileInputId : String
fileInputId =
    "receipt-file"


type alias Model =
    { authentication : Authentication
    , maybeReceiptFormModel : Maybe ReceiptForm.Model
    , fileSelected : Bool
    , currentTime : Maybe Float
    , maybePreviewDataUrl : Maybe String
    , uploading : Bool
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    let
        model =
            { authentication = authentication
            , maybeReceiptFormModel = Nothing
            , fileSelected = False
            , currentTime = Nothing
            , maybePreviewDataUrl = Nothing
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
    | ReceiptFormMsg ReceiptForm.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime time ->
            let
                receiptFormData : ReceiptFormData
                receiptFormData =
                    { total = Just 1.38
                    , description = ""
                    , timestamp = round <| Time.inMilliseconds time
                    , tags = []
                    }

                ( receiptFormModel, receiptFormCmd ) =
                    ReceiptForm.init receiptFormData
            in
                ( { model
                    | currentTime = Just <| Time.inMilliseconds time
                    , maybeReceiptFormModel = Just receiptFormModel
                  }
                , Cmd.map ReceiptFormMsg receiptFormCmd
                )

        UploadReceipt ->
            case model.maybeReceiptFormModel of
                Just currentReceiptFormModel ->
                    ( { model
                        | uploading = True
                      }
                    , Ports.createReceipt <| createReceiptParams model currentReceiptFormModel
                    )

                Nothing ->
                    ( model, Cmd.none )

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
            case model.maybeReceiptFormModel of
                Just currentReceiptFormModel ->
                    let
                        ( receiptFormModel, receiptFormCmd ) =
                            ReceiptForm.update message currentReceiptFormModel
                    in
                        ( { model
                            | maybeReceiptFormModel = Just receiptFormModel
                          }
                        , Cmd.map ReceiptFormMsg receiptFormCmd
                        )

                Nothing ->
                    ( model, Cmd.none )


createReceiptParams : Model -> ReceiptForm.Model -> Ports.CreateReceiptParams
createReceiptParams model receiptFormModel =
    { receiptDetails = ReceiptForm.formData receiptFormModel
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
        , (receiptFormView model.maybeReceiptFormModel)
        , input [ type_ "file", id fileInputId, onMouseDown ReceiptFileInputStart ] []
        , button [ onClick UploadReceipt, disabled <| not model.fileSelected ] [ text "Create receipt" ]
        ]


receiptFormView : Maybe ReceiptForm.Model -> Html Msg
receiptFormView maybeReceiptFormModel =
    case maybeReceiptFormModel of
        Just receiptFormModel ->
            Html.map ReceiptFormMsg (ReceiptForm.view receiptFormModel)

        Nothing ->
            div [] []


imagePreview : Maybe String -> Html Msg
imagePreview maybePreviewDataUrl =
    case maybePreviewDataUrl of
        Just previewDataUrl ->
            img [ src previewDataUrl ] []

        Nothing ->
            Html.text ""
