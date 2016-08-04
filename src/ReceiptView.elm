module ReceiptView exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Ports
import Api
import Models exposing (Receipt, ReceiptFile, Authentication)


imageId : String
imageId =
    "receipt-view-image"


type alias Model =
    { authentication : Authentication
    , receipt : Receipt
    , loadingImage : Bool
    }


init : Authentication -> Receipt -> ( Model, Cmd Msg )
init authentication receipt =
    let
        model =
            { authentication = authentication
            , receipt = receipt
            , loadingImage = False
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
            , imageId = imageId
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
              }
            , Cmd.none
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
        , span [] [ text <| toString model.loadingImage ]
        , img [ Html.Attributes.id imageId ] []
        ]
