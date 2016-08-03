module AuthenticatedUserView exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Models exposing (Authentication)
import ReceiptList
import Backup
import AddReceiptForm


type alias Model =
    { authentication : Authentication
    , receiptListModel : ReceiptList.Model
    , backupModel : Backup.Model
    , maybeAddReceiptFormModel : Maybe AddReceiptForm.Model
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    let
        ( receiptListModel, receiptListCmd ) =
            ReceiptList.init authentication

        ( backupModel, backupCmd ) =
            Backup.init authentication
    in
        ( { authentication = authentication
          , receiptListModel = receiptListModel
          , backupModel = backupModel
          , maybeAddReceiptFormModel = Nothing
          }
        , Cmd.batch
            [ Cmd.map ReceiptListMsg receiptListCmd
            , Cmd.map BackupMsg backupCmd
            ]
        )


type Msg
    = ReceiptListMsg ReceiptList.Msg
    | BackupMsg Backup.Msg
    | AddReceiptFormMsg AddReceiptForm.Msg
    | ShowNewReceiptForm
    | HideNewReceiptForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiptListMsg message ->
            let
                ( receiptListModel, receiptListCmd ) =
                    ReceiptList.update message model.receiptListModel
            in
                ( { model
                    | receiptListModel = receiptListModel
                  }
                , Cmd.map ReceiptListMsg receiptListCmd
                )

        BackupMsg message ->
            let
                ( backupModel, backupCmd ) =
                    Backup.update message model.backupModel
            in
                ( { model
                    | backupModel = backupModel
                  }
                , Cmd.map BackupMsg backupCmd
                )

        AddReceiptFormMsg message ->
            case model.maybeAddReceiptFormModel of
                Just currentAddReceiptFormModel ->
                    let
                        ( addReceiptFormModel, addReceiptFormCmd ) =
                            AddReceiptForm.update message currentAddReceiptFormModel
                    in
                        ( { model
                            | maybeAddReceiptFormModel = Just addReceiptFormModel
                          }
                        , Cmd.map AddReceiptFormMsg addReceiptFormCmd
                        )

                Nothing ->
                    ( model, Cmd.none )

        ShowNewReceiptForm ->
            let
                ( addReceiptFormModel, addReceiptFormCmd ) =
                    AddReceiptForm.init model.authentication
            in
                ( { model
                    | maybeAddReceiptFormModel = Just addReceiptFormModel
                  }
                , Cmd.map AddReceiptFormMsg addReceiptFormCmd
                )

        HideNewReceiptForm ->
            ( { model | maybeAddReceiptFormModel = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ App.map ReceiptListMsg (ReceiptList.view model.receiptListModel)
        , App.map BackupMsg (Backup.view model.backupModel)
        , addReceiptFormView model.maybeAddReceiptFormModel
        ]


addReceiptFormView : Maybe AddReceiptForm.Model -> Html Msg
addReceiptFormView maybeAddReceiptFormModel =
    case maybeAddReceiptFormModel of
        Just addReceiptFormModel ->
            div []
                [ button [ onClick HideNewReceiptForm ] [ text "Close new receipt form" ]
                , App.map AddReceiptFormMsg (AddReceiptForm.view addReceiptFormModel)
                ]

        Nothing ->
            button [ onClick ShowNewReceiptForm ] [ text "Add new receipt" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ReceiptListMsg ReceiptList.subscriptions
        , Sub.map AddReceiptFormMsg AddReceiptForm.subscriptions
        ]
