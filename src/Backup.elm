module Backup exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Api
import Models exposing (Authentication)
import Ports exposing (initDownload)


type alias Model =
    { userId : String
    , token : String
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    ( { userId = authentication.userId
      , token = authentication.token
      }
    , Cmd.none
    )


type Msg
    = DownloadBackup
    | BackupUrlSucceed String
    | BackupUrlFail Api.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadBackup ->
            ( model, Api.fetchBackupUrl model.token model.userId BackupUrlFail BackupUrlSucceed )

        BackupUrlSucceed backupUrl ->
            ( model, initDownload backupUrl )

        BackupUrlFail error ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick DownloadBackup ] [ text "Download backup" ]
        ]
