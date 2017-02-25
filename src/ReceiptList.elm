module ReceiptList exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes
import Api
import Models exposing (UserInfo, Receipt, Authentication)
import ReceiptView
import FormatUtils
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
                | receipts = receipts
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

                        updatedOpenedReceiptView =
                            if ReceiptView.isReceiptClosed message then
                                Nothing
                            else
                                Just receiptViewModel
                    in
                        ( { model
                            | openedReceiptView = updatedOpenedReceiptView
                            , receipts = updateReceipts message model.receipts
                          }
                        , Cmd.map ReceiptViewMsg receiptViewCmd
                        )

                Nothing ->
                    ( model, Cmd.none )


updateReceipts : ReceiptView.Msg -> List Receipt -> List Receipt
updateReceipts msg receipts =
    case ReceiptView.updatedReceipt msg of
        Just receipt ->
            List.map
                (\r ->
                    if r.id == receipt.id then
                        receipt
                    else
                        r
                )
                receipts

        Nothing ->
            receipts


subscriptions : Sub Msg
subscriptions =
    Sub.none


view : Model -> Html Msg
view model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 4 ] [ receiptList model ]
        , Grid.cell [ Grid.size Grid.All 8 ] [ receiptView model ]
        ]


receiptList : Model -> Html Msg
receiptList model =
    if model.loadingReceipts then
        Spinner.spinner
            [ Spinner.active True
            , Spinner.singleColor True
            ]
    else
        div [ Html.Attributes.class "receipt-list" ]
            [ Lists.ul []
                ((List.map (\receipt -> receiptRow receipt) model.receipts) ++ [ emptyRow ])
            ]


receiptView : Model -> Html Msg
receiptView model =
    case model.openedReceiptView of
        Just openedReceiptView ->
            Html.map ReceiptViewMsg (ReceiptView.view openedReceiptView)

        Nothing ->
            div [] []


receiptRow : Receipt -> Html Msg
receiptRow receipt =
    let
        totalElement =
            case receipt.total of
                Just total ->
                    span
                        [ Html.Attributes.class "total-present" ]
                        [ text <| FormatUtils.formatMoney total ]

                Nothing ->
                    span
                        [ Html.Attributes.class "add-details" ]
                        [ text "Add details" ]
    in
        Lists.li [ Lists.withBody, Options.attribute <| Html.Events.onClick (OpenReceiptView receipt) ]
            [ Lists.content
                []
                [ totalElement
                , Lists.body [] [ text receipt.description ]
                ]
            , Lists.content2 []
                [ Lists.info2 [] [ text <| FormatUtils.formatTimestamp receipt.transactionTime ]
                ]
            ]


emptyRow : Html Msg
emptyRow =
    Lists.li [ Lists.withBody ]
        [ Lists.content
            []
            [ span [] []
            , Lists.body [] []
            ]
        ]
