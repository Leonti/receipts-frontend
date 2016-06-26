module UserInfo exposing (Model, Msg, emptyModel, init, update, view, userInfo)

import Html exposing (..)
--import Html.Attributes exposing (..)
--import Html.Events exposing (..)

import Api
import Models exposing (UserInfo)

type alias Model =
  { token : String
  , userInfo : Maybe UserInfo
  }

userInfo : Model -> Maybe UserInfo
userInfo model = model.userInfo

init : String -> (Model, Cmd Msg)
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fetch ->
        (model, Api.fetchUserInfo model.token FetchFail FetchSucceed)

    FetchSucceed userInfo ->
        ({ model | userInfo = Just userInfo }, Cmd.none)

    FetchFail error ->
        (model, Cmd.none)

view : Model -> Html Msg
view model =
  div []
        [ div []
            [text <| Maybe.withDefault "no user" <| Maybe.map (\ui -> ui.id ++ " " ++ ui.username) model.userInfo]
        ]
