module AuthenticatedUserView exposing (Model, Msg, init, update, view, drawerView, subscriptions)

import Html exposing (..)
import Html.Events exposing (..)
import Models exposing (Authentication)
import ReceiptList
import Backup
import AddReceiptForm
import Material
import Material.Icon as Icon
import Material.Button as Button
import Material.Options as Options


type alias Model =
    { authentication : Authentication
    , receiptListModel : ReceiptList.Model
    , backupModel : Backup.Model
    , maybeAddReceiptFormModel : Maybe AddReceiptForm.Model
    , mdl : Material.Model
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
          , mdl = Material.model
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
    | Mdl (Material.Msg Msg)


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

        Mdl msg_ ->
            Material.update Mdl msg_ model


view : Model -> Html Msg
view model =
    div []
        [ Html.map ReceiptListMsg (ReceiptList.view model.receiptListModel)
        , addReceiptFormView model model.maybeAddReceiptFormModel
        ]


drawerView : Model -> Html Msg
drawerView model =
    Html.map BackupMsg (Backup.view model.backupModel)


addReceiptFormView : Model -> Maybe AddReceiptForm.Model -> Html Msg
addReceiptFormView model maybeAddReceiptFormModel =
    case maybeAddReceiptFormModel of
        Just addReceiptFormModel ->
            div []
                [ button [ onClick HideNewReceiptForm ] [ text "Close new receipt form" ]
                , Html.map AddReceiptFormMsg (AddReceiptForm.view addReceiptFormModel)
                ]

        Nothing ->
            Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.fab
                , Button.colored
                , Options.onClick ShowNewReceiptForm
                ]
                [ Icon.i "add" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ReceiptListMsg ReceiptList.subscriptions
        , Sub.map AddReceiptFormMsg AddReceiptForm.subscriptions
        ]
