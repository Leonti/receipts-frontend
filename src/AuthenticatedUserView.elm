module AuthenticatedUserView exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.App as App
import Models exposing (Authentication)
import ReceiptList
import Backup
import AddReceiptForm


type alias Model =
    { authentication : Authentication
    , receiptListModel : ReceiptList.Model
    , backupModel : Backup.Model
    , addReceiptFormModel : AddReceiptForm.Model
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    let
        ( receiptListModel, receiptListCmd ) =
            ReceiptList.init authentication

        ( backupModel, backupCmd ) =
            Backup.init authentication

        ( addReceiptFormModel, addReceiptFormCmd ) =
            AddReceiptForm.init authentication
    in
        ( { authentication = authentication
          , receiptListModel = receiptListModel
          , backupModel = backupModel
          , addReceiptFormModel = addReceiptFormModel
          }
        , Cmd.batch
            [ Cmd.map ReceiptListMsg receiptListCmd
            , Cmd.map BackupMsg backupCmd
            , Cmd.map AddReceiptFormMsg addReceiptFormCmd
            ]
        )


type Msg
    = ReceiptListMsg ReceiptList.Msg
    | BackupMsg Backup.Msg
    | AddReceiptFormMsg AddReceiptForm.Msg


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
            let
                ( addReceiptFormModel, addReceiptFormCmd ) =
                    AddReceiptForm.update message model.addReceiptFormModel
            in
                ( { model
                    | addReceiptFormModel = addReceiptFormModel
                  }
                , Cmd.map AddReceiptFormMsg addReceiptFormCmd
                )


view : Model -> Html Msg
view model =
    div []
        [ App.map ReceiptListMsg (ReceiptList.view model.receiptListModel)
        , App.map BackupMsg (Backup.view model.backupModel)
        , App.map AddReceiptFormMsg (AddReceiptForm.view model.addReceiptFormModel)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ReceiptListMsg ReceiptList.subscriptions
        , Sub.map AddReceiptFormMsg AddReceiptForm.subscriptions
        ]
