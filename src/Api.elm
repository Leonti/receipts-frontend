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
baseUrl =
    "http://localhost:9000"



--baseUrl =
--    "https://api.receipts.leonti.me"


type Error
    = Error String



-- App config


fetchAppConfig : (Result Error AppConfig -> msg) -> Cmd msg
fetchAppConfig handler =
    Http.send (transformResultHandler handler)
        (Http.get (baseUrl ++ "/config") Models.appConfigDecoder)


authorizationHeaders : String -> Http.Header
authorizationHeaders token =
    Http.header "Authorization" ("Bearer " ++ token)


fetchBackupUrl : Authentication -> (Result Error String -> msg) -> Cmd msg
fetchBackupUrl authentication handler =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ authorizationHeaders authentication.token ]
                , url = baseUrl ++ "/user/" ++ authentication.userId ++ "/backup/token"
                , body = Http.emptyBody
                , expect = Http.expectJson Models.accessTokenDecoder
                , timeout = Nothing
                , withCredentials = False
                }

        accessTokenTask =
            Http.toTask request

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
        Task.attempt (transformResultHandler handler) backupUrlTask



-- Authentication


authenticate : String -> String -> (Result Error String -> msg) -> Cmd msg
authenticate username password handler =
    let
        basicAuthHeaderResult =
            basicAuthHeader username password
    in
        case basicAuthHeaderResult of
            Result.Ok header ->
                authenticationGet header handler

            Result.Err error ->
                Cmd.none


basicAuthHeader : String -> String -> Result String String
basicAuthHeader username password =
    Result.map (\s -> "Basic " ++ s) (Base64.encode (username ++ ":" ++ password))


authenticationGet : String -> (Result Error String -> msg) -> Cmd msg
authenticationGet basicAuthHeader handler =
    Http.send (transformResultHandler handler) <|
        Http.request
            { method = "GET"
            , headers = [ Http.header "Authorization" basicAuthHeader ]
            , url = baseUrl ++ "/token/create"
            , body = Http.emptyBody
            , expect = Http.expectJson Models.accessTokenDecoder
            , timeout = Nothing
            , withCredentials = False
            }


authenticateWithGoogle : String -> (Result Error String -> msg) -> Cmd msg
authenticateWithGoogle accessToken handler =
    let
        accessTokenValue =
            Json.Encode.object
                [ ( "token", Json.Encode.string accessToken ) ]

        request =
            Http.request
                { method = "POST"
                , headers = [ Http.header "Content-Type" "application/json" ]
                , url = baseUrl ++ "/oauth/google-access-token"
                , body = Http.jsonBody accessTokenValue
                , expect = Http.expectJson Models.accessTokenDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (transformResultHandler handler) request



-- user info


fetchUserInfo : String -> (Result Error UserInfo -> msg) -> Cmd msg
fetchUserInfo token handler =
    Http.send (transformResultHandler handler) <|
        Http.request
            { method = "GET"
            , headers = [ (authorizationHeaders token) ]
            , url = baseUrl ++ "/user/info"
            , body = Http.emptyBody
            , expect = Http.expectJson Models.userInfoDecoder
            , timeout = Nothing
            , withCredentials = False
            }



-- user receipts


fetchReceipts : Authentication -> (Result Error (List Receipt) -> msg) -> Cmd msg
fetchReceipts authentication handler =
    Http.send (transformResultHandler handler) <|
        Http.request
            { method = "GET"
            , headers = [ (authorizationHeaders authentication.token) ]
            , url = baseUrl ++ "/user/" ++ authentication.userId ++ "/receipt"
            , body = Http.emptyBody
            , expect = Http.expectJson Models.receiptsDecoder
            , timeout = Nothing
            , withCredentials = False
            }


transformResultHandler : (Result Error a -> msg) -> Result Http.Error a -> msg
transformResultHandler toMsg result =
    toMsg <| Result.mapError transformHttpError result


transformHttpError : Http.Error -> Error
transformHttpError httpError =
    case httpError of
        Http.Timeout ->
            Error "Timeout"

        Http.NetworkError ->
            Error "NetworkError"

        Http.BadPayload desc response ->
            Error <| "BadPayload " ++ desc ++ " " ++ response.body

        Http.BadStatus response ->
            Error <| "BadStatus " ++ " " ++ response.body

        Http.BadUrl desc ->
            Error <| "BadUrl " ++ " " ++ desc


createReceiptUrl : String -> String
createReceiptUrl userId =
    baseUrl ++ "/user/" ++ userId ++ "/receipt"


receiptFileUrl : String -> String -> String -> String -> String
receiptFileUrl userId receiptId fileId ext =
    baseUrl ++ "/user/" ++ userId ++ "/receipt/" ++ receiptId ++ "/file/" ++ fileId ++ "." ++ ext
