module ReceiptList exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes
import Api
import Models exposing (UserInfo, Receipt, Authentication)
import ReceiptView
import FormatUtils
import Time exposing (Time, millisecond)
import Material
import Material.List as Lists
import Material.Options as Options exposing (when, css)
import Material.Spinner as Spinner
import Material.Grid as Grid
import Material.Textfield as Textfield


type alias Model =
    { authentication : Authentication
    , receipts : List Receipt
    , openedReceiptView : Maybe ReceiptView.Model
    , loadingReceipts : Bool
    , query : String
    , typingTime : Time
    , isTyping : Bool
    , mdl : Material.Model
    }


typingCheckInterval : Time
typingCheckInterval =
    millisecond * 200


typingThreshold : Time
typingThreshold =
    millisecond * 800


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    update Fetch
        { authentication = authentication
        , receipts = []
        , openedReceiptView = Nothing
        , loadingReceipts = False
        , query = ""
        , typingTime = 0
        , isTyping = False
        , mdl = Material.model
        }


type Msg
    = Fetch
    | FetchResult (Result Api.Error (List Receipt))
    | ReceiptViewMsg ReceiptView.Msg
    | OpenReceiptView Receipt
    | Search String
    | TypingCheck Time
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( { model | loadingReceipts = True }
            , Api.fetchReceipts model.authentication model.query FetchResult
            )

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

        Search query ->
            ( { model
                | query = query
                , typingTime = 0
                , isTyping = True
              }
            , Cmd.none
            )

        TypingCheck time ->
            let
                updatedTypingTime =
                    model.typingTime + typingCheckInterval
            in
                if updatedTypingTime >= typingThreshold then
                    update Fetch
                        { model
                            | typingTime = 0
                            , isTyping = Debug.log "Typing is done" False
                        }
                else
                    ( { model | typingTime = updatedTypingTime }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isTyping then
        Time.every typingCheckInterval TypingCheck
    else
        Sub.none


view : Model -> Html Msg
view model =
    Grid.grid []
        [ Grid.cell [ Grid.size Grid.All 4 ]
            [ receiptFilters model
            , receiptList model
            ]
        , Grid.cell [ Grid.size Grid.All 8 ] [ receiptView model ]
        ]


receiptFilters : Model -> Html Msg
receiptFilters model =
    div [ Html.Attributes.class "receipt-filters" ]
        [ Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Textfield.value model.query
            , Options.onInput Search
            , Textfield.label "Search ..."
            , Options.cs "search-input"
            ]
            []
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
                ((List.map
                    (\receipt ->
                        receiptRow receipt (isReceiptSelected model receipt)
                    )
                    model.receipts
                 )
                    ++ [ emptyRow ]
                )
            ]


isReceiptSelected : Model -> Receipt -> Bool
isReceiptSelected model receipt =
    case model.openedReceiptView of
        Just openedReceiptViewModel ->
            (ReceiptView.openedReceipt openedReceiptViewModel).id == receipt.id

        Nothing ->
            False


receiptView : Model -> Html Msg
receiptView model =
    case model.openedReceiptView of
        Just openedReceiptView ->
            Html.map ReceiptViewMsg (ReceiptView.view openedReceiptView)

        Nothing ->
            div [] []


receiptRow : Receipt -> Bool -> Html Msg
receiptRow receipt isSelected =
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

        rowClass =
            if isSelected then
                "receipt-row selected"
            else
                "receipt-row"
    in
        Lists.li
            [ Lists.withBody
            , Options.attribute <| Html.Events.onClick (OpenReceiptView receipt)
            , Options.cs rowClass
            ]
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
