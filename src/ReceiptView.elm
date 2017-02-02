module ReceiptView exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Ports
import Api
import Models exposing (Receipt, ReceiptFile, Authentication)
import ReceiptForm
import Material.Spinner as Spinner


type alias Model =
    { authentication : Authentication
    , receipt : Receipt
    , loadingImage : Bool
    , imageData : String
    , receiptFormModel : ReceiptForm.Model
    }


init : Authentication -> Receipt -> ( Model, Cmd Msg )
init authentication receipt =
    let
        ( receiptFormModel, receiptFormCmd ) =
            ReceiptForm.init
                { total = receipt.total
                , description = receipt.description
                , timestamp = receipt.timestamp
                , tags = receipt.tags
                }

        model =
            { authentication = authentication
            , receipt = receipt
            , loadingImage = False
            , imageData = ""
            , receiptFormModel = receiptFormModel
            }
    in
        case toImageParams model of
            Just imageParams ->
                update (LoadImage imageParams) model

            Nothing ->
                (model ! [])


toImageParams : Model -> Maybe Ports.LoadImageParams
toImageParams model =
    Maybe.map
        (\file ->
            { url = Api.receiptFileUrl model.authentication.userId model.receipt.id file.id file.ext
            , authToken = model.authentication.token
            , fileId = file.id
            }
        )
    <|
        toFile model.receipt


toFile : Receipt -> Maybe ReceiptFile
toFile receipt =
    case receipt.files of
        [ original ] ->
            Just original

        [ original, resized ] ->
            Just resized

        original :: resized :: _ ->
            Just resized

        [] ->
            Nothing


type Msg
    = LoadImage Ports.LoadImageParams
    | LoadImageSucceed Ports.LoadImageResult
    | ReceiptFormMsg ReceiptForm.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadImage loadImageParams ->
            ( { model
                | loadingImage = True
              }
            , Ports.loadImage loadImageParams
            )

        LoadImageSucceed loadImageResult ->
            ( { model
                | loadingImage = False
                , imageData = loadImageResult.imageData
              }
            , Cmd.none
            )

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
    Ports.imageLoaded LoadImageSucceed


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Receipt view:" ]
        , div [] [ text model.receipt.id ]
        , Spinner.spinner
            [ Spinner.active model.loadingImage
            , Spinner.singleColor True
            ]
        , Html.map ReceiptFormMsg (ReceiptForm.view model.receiptFormModel)
        , receiptImageView model
        ]


receiptImageView : Model -> Html Msg
receiptImageView model =
    let
        imageDataUrl =
            "data:image/jpeg;base64," ++ model.imageData

        imageBaseStyle =
            [ ( "width", "100%" ) ]

        imageStyle =
            if model.loadingImage then
                imageBaseStyle ++ [ ( "display", "none" ) ]
            else
                imageBaseStyle
    in
        div []
            [ img
                [ Html.Attributes.src imageDataUrl
                , Html.Attributes.style imageStyle
                ]
                []
            ]
