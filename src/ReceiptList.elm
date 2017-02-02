module ReceiptList exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Events exposing (..)
import Api
import Models exposing (UserInfo, Receipt, Authentication)
import ReceiptView
import Material.List as Lists
import Material.Options as Options exposing (when, css)
import Material.Spinner as Spinner
import Material.Grid as Grid


type alias Model =
    { authentication : Authentication
    , receipts : List Receipt
    , openedReceiptView : Maybe ReceiptView.Model
    , loadingReceipts : Bool
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    update Fetch
        { authentication = authentication
        , receipts = []
        , openedReceiptView = Nothing
        , loadingReceipts = False
        }


type Msg
    = Fetch
    | FetchResult (Result Api.Error (List Receipt))
    | ReceiptViewMsg ReceiptView.Msg
    | OpenReceiptView Receipt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( { model | loadingReceipts = True }, Api.fetchReceipts model.authentication FetchResult )

        FetchResult (Ok receipts) ->
            ( { model
                | receipts = List.take 10 receipts
                , loadingReceipts = False
              }
            , Cmd.none
            )

        FetchResult (Err error) ->
            ( { model | loadingReceipts = False }, Cmd.none )

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
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 6 ] [ receiptList model ]
        , Grid.cell [ Grid.size Grid.All 6 ] [ receiptView model ]
        ]


receiptList : Model -> Html Msg
receiptList model =
    if model.loadingReceipts then
        Spinner.spinner
            [ Spinner.active True
            , Spinner.singleColor True
            ]
    else
        Lists.ul []
            (List.map (\receipt -> receiptRow receipt) model.receipts)


receiptView : Model -> Html Msg
receiptView model =
    case model.openedReceiptView of
        Just openedReceiptView ->
            Html.map ReceiptViewMsg (ReceiptView.view openedReceiptView)

        Nothing ->
            div [] []


receiptRow : Receipt -> Html Msg
receiptRow receipt =
    Lists.li []
        [ Lists.content
            [ Options.attribute <| Html.Events.onClick (OpenReceiptView receipt) ]
            [ text receipt.id ]
        ]
