module Backup exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Api
import Models exposing (Authentication)
import Ports exposing (initDownload)
import Material
import Material.Button as Button
import Material.Options as Options


type alias Model =
    { authentication : Authentication
    , mdl : Material.Model
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    ( { authentication = authentication
      , mdl = Material.model
      }
    , Cmd.none
    )


type Msg
    = DownloadBackup
    | BackupUrlResult (Result Api.Error String)
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadBackup ->
            ( model, Api.fetchBackupUrl model.authentication BackupUrlResult )

        BackupUrlResult (Ok backupUrl) ->
            ( model, initDownload backupUrl )

        BackupUrlResult (Err error) ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


view : Model -> Html Msg
view model =
    div []
        [ Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Button.ripple
            , Options.onClick DownloadBackup
            ]
            [ text "Download Backup" ]
        ]
