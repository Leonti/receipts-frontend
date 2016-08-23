module Api
    exposing
        ( Error
        , authenticate
        , authenticateWithGoogle
        , fetchAppConfig
        , fetchUserInfo
        , fetchReceipts
        , fetchBackupUrl
        , createReceiptUrl
        , receiptFileUrl
        )

import Http
import Models exposing (..)
import Task
import Base64
import Json.Encode


baseUrl : String



--baseUrl =
--    "http://localhost:9000"


baseUrl =
    "https://api.receipts.leonti.me"


type Error
    = Error String



-- App config


fetchAppConfig : (Error -> msg) -> (AppConfig -> msg) -> Cmd msg
fetchAppConfig fetchFail fetchSucceed =
    Task.perform
        (handleError transformHttpError fetchFail)
        fetchSucceed
        (Http.get Models.appConfigDecoder (baseUrl ++ "/config"))


fetchBackupUrl : Authentication -> (Error -> msg) -> (String -> msg) -> Cmd msg
fetchBackupUrl authentication fetchFail fetchSucceed =
    let
        request =
            { verb = "GET"
            , headers = [ ( "Authorization", "Bearer " ++ authentication.token ) ]
            , url = baseUrl ++ "/user/" ++ authentication.userId ++ "/backup/token"
            , body = Http.empty
            }

        accessTokenTask =
            Http.fromJson Models.accessTokenDecoder (Http.send Http.defaultSettings request)

        backupUrlTask =
            Task.map
                (\token ->
                    baseUrl
                        ++ "/user/"
                        ++ authentication.userId
                        ++ "/backup/download?access_token="
                        ++ token
                )
                accessTokenTask
    in
        Task.perform (handleError transformHttpError fetchFail) fetchSucceed backupUrlTask



-- Authentication


authenticate : String -> String -> (Error -> msg) -> (String -> msg) -> Cmd msg
authenticate username password loginFail loginSucceed =
    let
        basicAuthHeaderResult =
            basicAuthHeader username password
    in
        case basicAuthHeaderResult of
            Result.Ok header ->
                Task.perform (handleError transformHttpError loginFail) loginSucceed (authenticationGet header)

            Result.Err error ->
                Cmd.none


basicAuthHeader : String -> String -> Result String String
basicAuthHeader username password =
    Result.map (\s -> "Basic " ++ s) (Base64.encode (username ++ ":" ++ password))


authenticationGet : String -> Task.Task Http.Error String
authenticationGet basicAuthHeader =
    let
        request =
            { verb = "GET"
            , headers = [ ( "Authorization", basicAuthHeader ) ]
            , url = baseUrl ++ "/token/create"
            , body = Http.empty
            }
    in
        Http.fromJson Models.accessTokenDecoder (Http.send Http.defaultSettings request)


authenticateWithGoogle : String -> (Error -> msg) -> (String -> msg) -> Cmd msg
authenticateWithGoogle accessToken loginFail loginSucceed =
    let
        accessTokenValue =
            Json.Encode.object
                [ ( "token", Json.Encode.string accessToken ) ]

        accessTokenBody =
            Http.string <| Json.Encode.encode 0 accessTokenValue

        request =
            { verb = "POST"
            , headers = [ ( "Content-Type", "application/json" ) ]
            , url = baseUrl ++ "/oauth/google-access-token"
            , body = accessTokenBody
            }

        task =
            Http.fromJson Models.accessTokenDecoder (Http.send Http.defaultSettings request)
    in
        Task.perform (handleError transformHttpError loginFail) loginSucceed task



-- user info


fetchUserInfo : String -> (Error -> msg) -> (UserInfo -> msg) -> Cmd msg
fetchUserInfo token fetchFail fetchSucceed =
    Task.perform (handleError transformHttpError fetchFail) fetchSucceed (fetchUserInfoGet token)


fetchUserInfoGet : String -> Task.Task Http.Error UserInfo
fetchUserInfoGet token =
    let
        request =
            { verb = "GET"
            , headers = [ ( "Authorization", "Bearer " ++ token ) ]
            , url = baseUrl ++ "/user/info"
            , body = Http.empty
            }
    in
        Http.fromJson Models.userInfoDecoder <| Http.send Http.defaultSettings request



-- user receipts


fetchReceipts : Authentication -> (Error -> msg) -> (List Receipt -> msg) -> Cmd msg
fetchReceipts authentication fetchFail fetchSucceed =
    Task.perform (handleError transformHttpError fetchFail) fetchSucceed (fetchReceiptsGet authentication)


fetchReceiptsGet : Authentication -> Task.Task Http.Error (List Receipt)
fetchReceiptsGet authentication =
    let
        request =
            { verb = "GET"
            , headers = [ ( "Authorization", "Bearer " ++ authentication.token ) ]
            , url = baseUrl ++ "/user/" ++ authentication.userId ++ "/receipt"
            , body = Http.empty
            }
    in
        Http.fromJson (Models.receiptsDecoder) (Http.send Http.defaultSettings request)


handleError : (Http.Error -> Error) -> (Error -> msg) -> Http.Error -> msg
handleError toError toMsg httpError =
    toMsg <| toError httpError


transformHttpError : Http.Error -> Error
transformHttpError httpError =
    case httpError of
        Http.Timeout ->
            Error "Timeout"

        Http.NetworkError ->
            Error "NetworkError"

        Http.UnexpectedPayload desc ->
            Error <| "UnexpectedPayload " ++ desc

        Http.BadResponse code desc ->
            Error <| "BadResponse " ++ (toString code) ++ " " ++ desc


createReceiptUrl : String -> String
createReceiptUrl userId =
    baseUrl ++ "/user/" ++ userId ++ "/receipt"


receiptFileUrl : String -> String -> String -> String -> String
receiptFileUrl userId receiptId fileId ext =
    baseUrl ++ "/user/" ++ userId ++ "/receipt/" ++ receiptId ++ "/file/" ++ fileId ++ "." ++ ext
