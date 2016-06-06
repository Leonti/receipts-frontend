module Api exposing (authenticate, fetchUserInfo, UserInfo)

import Http
import Json.Decode exposing ((:=))
import Json.Decode as Json

import Task
import Base64

type alias UserInfo =
    { id : String
    , username : String
    }

type alias Receipt =
    { id : String }

-- Authentication
authenticate : String -> String -> (Http.Error -> msg) -> (String -> msg) -> Cmd msg
authenticate username password loginFail loginSucceed =
    let basicAuthHeaderResult =
        basicAuthHeader username password
    in
        case basicAuthHeaderResult of
            Result.Ok header -> Task.perform loginFail loginSucceed (authenticationGet header)
            Result.Err error -> Cmd.none

basicAuthHeader : String -> String -> Result String String
basicAuthHeader username password = Result.map (\s -> "Basic " ++ s) (Base64.encode (username ++ ":" ++ password))

authenticationGet : String -> Task.Task Http.Error String
authenticationGet basicAuthHeader =
    let request =
        { verb = "GET"
        , headers = [("Authorization", basicAuthHeader)]
        , url = "https://api.receipts.leonti.me/token/create"
        , body = Http.empty
        }
    in
        Http.fromJson (Json.at["access_token"] Json.string) (Http.send Http.defaultSettings request)

-- user info

fetchUserInfo : String -> (Http.Error -> msg) -> (UserInfo -> msg) -> Cmd msg
fetchUserInfo token fetchFail fetchSucceed =
    Task.perform fetchFail fetchSucceed (fetchUserInfoGet token)

fetchUserInfoGet : String -> Task.Task Http.Error UserInfo
fetchUserInfoGet token =
    let request =
        { verb = "GET"
        , headers = [("Authorization", "Bearer " ++ token)]
        , url = "https://api.receipts.leonti.me/user/info"
        , body = Http.empty
        }
    in
        Http.fromJson (userInfoDecoder) (Http.send Http.defaultSettings request)

userInfoDecoder : Json.Decoder UserInfo
userInfoDecoder =
    Json.object2 UserInfo
        ("id" := Json.string)
        ("username" := Json.string)

-- user receipts

fetchReceipts : String -> String -> Task.Task Http.Error (List Receipt)
fetchReceipts token userId =
    let request =
        { verb = "GET"
        , headers = [("Authorization", "Bearer " ++ token)]
        , url = "https://api.receipts.leonti.me/user/" ++ userId ++ "/receipt"
        , body = Http.empty
        }
    in
        Http.fromJson (Json.list receiptDecoder) (Http.send Http.defaultSettings request)

receiptDecoder : Json.Decoder Receipt
receiptDecoder =
    Json.object1 Receipt
        ("id" := Json.string)
