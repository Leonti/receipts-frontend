module Backup exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Api
import Models exposing (Authentication)
import Ports exposing (initDownload)


type alias Model =
    { authentication : Authentication
    }


init : Authentication -> ( Model, Cmd Msg )
init authentication =
    ( { authentication = authentication
      }
    , Cmd.none
    )


type Msg
    = DownloadBackup
    | BackupUrlResult (Result Api.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadBackup ->
            ( model, Api.fetchBackupUrl model.authentication BackupUrlResult )

        BackupUrlResult (Ok backupUrl) ->
            ( model, initDownload backupUrl )

        BackupUrlResult (Err error) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick DownloadBackup ] [ text "Download backup" ]
        ]
