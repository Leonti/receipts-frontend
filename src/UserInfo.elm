module UserInfo exposing (Model, Msg, emptyModel, init, update, view)

import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (..)

import Api
import Models exposing (UserInfo)

type alias Model =
  { token : String
  , userInfo : Maybe UserInfo
  }


init : String -> (Model, Cmd Msg, Bool)
init token =
    let model =
        { token = token
        , userInfo = Nothing
        }
    in
        update Fetch model

emptyModel : Model
emptyModel =
    { token = ""
    , userInfo = Nothing
    }

type Msg
    = Fetch
    | FetchSucceed UserInfo
    | FetchFail Api.Error

update : Msg -> Model -> (Model, Cmd Msg, Bool)
update msg model =
  case msg of
    Fetch ->
        (model, Api.fetchUserInfo model.token FetchFail FetchSucceed, False)

    FetchSucceed userInfo ->
        ({ model | userInfo = Just userInfo }, Cmd.none, True)

    FetchFail error ->
        (model, Cmd.none, False)

view : Model -> Html Msg
view model =
  div []
        [ div []
            [text <| Maybe.withDefault "no user" <| Maybe.map (\ui -> ui.id ++ " " ++ ui.username) model.userInfo]
        ]
