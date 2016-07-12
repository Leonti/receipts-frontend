module Api exposing (Error, authenticate, fetchUserInfo, fetchReceipts, baseUrl)

import Http
import Models exposing (..)
import Task
import Base64


baseUrl : String
baseUrl =
    "http://localhost:9000"



-- baseUrl = https://api.receipts.leonti.me


type Error
    = Error String



-- Authentication


authenticate : String -> String -> (Http.Error -> msg) -> (String -> msg) -> Cmd msg
authenticate username password loginFail loginSucceed =
    let
        basicAuthHeaderResult =
            basicAuthHeader username password
    in
        case basicAuthHeaderResult of
            Result.Ok header ->
                Task.perform loginFail loginSucceed (authenticationGet header)

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


fetchReceipts : String -> String -> (Error -> msg) -> (List Receipt -> msg) -> Cmd msg
fetchReceipts token userId fetchFail fetchSucceed =
    Task.perform (handleError transformHttpError fetchFail) fetchSucceed (fetchReceiptsGet token userId)


fetchReceiptsGet : String -> String -> Task.Task Http.Error (List Receipt)
fetchReceiptsGet token userId =
    let
        request =
            { verb = "GET"
            , headers = [ ( "Authorization", "Bearer " ++ token ) ]
            , url = baseUrl ++ "/user/" ++ userId ++ "/receipt"
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
