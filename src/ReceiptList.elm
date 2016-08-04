module ReceiptList exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Api
import Models exposing (UserInfo, Receipt, Authentication)
import ReceiptView


type alias Model =
    { authentication : Authentication
    , receipts : List Receipt
    , openedReceiptView : Maybe ReceiptView.Model
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    update Fetch
        { authentication = authentication
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
            ( model, Api.fetchReceipts model.authentication FetchFail FetchSucceed )

        FetchSucceed receipts ->
            ( { model | receipts = List.take 10 receipts }, Cmd.none )

        FetchFail error ->
            ( model, Cmd.none )

        OpenReceiptView receipt ->
            let
                ( receiptViewModel, receiptViewCmd ) =
                    ReceiptView.init model.authentication receipt
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
        , div [] (List.map (\receipt -> receiptRow receipt) model.receipts)
        , receiptView model
        ]


receiptView : Model -> Html Msg
receiptView model =
    case model.openedReceiptView of
        Just openedReceiptView ->
            App.map ReceiptViewMsg (ReceiptView.view openedReceiptView)

        Nothing ->
            div [] []


receiptRow : Receipt -> Html Msg
receiptRow receipt =
    div []
        [ div []
            [ text receipt.id
            , button [ onClick <| OpenReceiptView receipt ] [ text "View Receipt" ]
            ]
        ]
