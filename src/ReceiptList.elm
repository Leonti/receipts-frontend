module ReceiptList exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task

type alias Model =
  { receipts : [String]
  , token : String
  }


init : String -> (Model, Cmd Msg)
init token =
    { receipts = []
    , token = token
    } ! []

type Msg
    = Fetch
    | FetchSucceed [String]
    | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fetch ->
        (model, Cmd.none)

    FetchSucceed receipts ->
        ({model | receipts = receipts}, Cmd.none)

    FetchFail error ->
        (model, Cmd.none)

fetchReceipts : String -> Task.Task Http.Error [String]
fetchReceipts token =
    let request =
        { verb = "GET"
        , headers = [("Authorization", "Bearer " ++ token)]
        , url = "https://api.receipts.leonti.me/user/info"
        , body = Http.empty
        }
    in
        Http.fromJson (Json.at["access_token"] Json.string) (Http.send Http.defaultSettings request)

view : Model -> Html Msg
view model =
  div []
    [ List.map receiptRow model.receipts
    ]

receiptRow : String -> Html Msg
receiptRow receipt =
    div []
        [ text receipt]
