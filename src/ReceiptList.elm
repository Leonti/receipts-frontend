module ReceiptList exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Api
import Models exposing (UserInfo, Receipt, Authentication)
import ReceiptView


type alias Model =
    { userId : String
    , token : String
    , receipts : List Receipt
    , openedReceiptView : Maybe ReceiptView.Model
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    update Fetch
        { userId = authentication.userId
        , token = authentication.token
        , receipts = []
        , openedReceiptView = Nothing
        }


type Msg
    = Fetch
    | FetchSucceed (List Receipt)
    | FetchFail Api.Error
    | ReceiptViewMsg ReceiptView.Msg
    | OpenReceiptView Receipt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( model, Api.fetchReceipts model.token model.userId FetchFail FetchSucceed )

        FetchSucceed receipts ->
            ( { model | receipts = List.take 10 receipts }, Cmd.none )

        FetchFail error ->
            ( model, Cmd.none )

        OpenReceiptView receipt ->
            let
                ( receiptViewModel, receiptViewCmd ) =
                    ReceiptView.init model.userId model.token receipt
            in
                ( { model
                    | openedReceiptView = Just receiptViewModel
                  }
                , Cmd.map ReceiptViewMsg receiptViewCmd
                )

        ReceiptViewMsg message ->
            case model.openedReceiptView of
                Just openedReceiptView ->
                    let
                        ( receiptViewModel, receiptViewCmd ) =
                            ReceiptView.update message openedReceiptView
                    in
                        ( { model
                            | openedReceiptView = Just receiptViewModel
                          }
                        , Cmd.map ReceiptViewMsg receiptViewCmd
                        )

                Nothing ->
                    ( model, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Sub.map ReceiptViewMsg (ReceiptView.subscriptions)


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Receipts:" ]
        , div [] (List.map (\receipt -> receiptRow model.userId model.token receipt) model.receipts)
        , receiptView model
        ]


receiptView : Model -> Html Msg
receiptView model =
    case model.openedReceiptView of
        Just openedReceiptView ->
            App.map ReceiptViewMsg (ReceiptView.view openedReceiptView)

        Nothing ->
            div [] []


receiptRow : String -> String -> Receipt -> Html Msg
receiptRow userId authToken receipt =
    div []
        [ div []
            [ text receipt.id
            , button [ onClick <| OpenReceiptView receipt ] [ text "View Receipt" ]
            ]
        ]
