module Backup exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Api
import Models exposing (Authentication)


type alias Model =
    { userId : String
    , token : String
    }


emptyModel : Model
emptyModel =
    { userId = ""
    , token = ""
    }


init : Maybe Authentication -> ( Model, Cmd Msg )
init maybeAuthentication =
    case maybeAuthentication of
        Just authentication ->
            ( { emptyModel
                | userId = authentication.userId
                , token = authentication.token
              }
            , Cmd.none
            )

        Nothing ->
            ( emptyModel, Cmd.none )


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
            ( model, Cmd.none )

        BackupUrlFail error ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Receipts:" ]
        , button [ onClick DownloadBackup ] [ text "Download backup" ]
        ]
