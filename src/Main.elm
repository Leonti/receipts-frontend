port module Main exposing (..)

import LoginForm
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Api
import Http

main =
  App.programWithFlags
    { init = init
    , view = view
    , update = (\msg model -> withSetStorage (update msg model))
    , subscriptions = \_ -> Sub.none
    }

port setStorage : PersistedModel -> Cmd msg

withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage (model, cmds) =
  ( model, Cmd.batch
    [ setStorage (persistedModel model), cmds ] )

-- MODEL

type alias PersistedModel =
    { token : Maybe String
    }

type alias Model =
  { loginForm : LoginForm.Model,
    userInfo : Maybe Api.UserInfo
  }

persistedModel : Model -> PersistedModel
persistedModel model =
    { token = (LoginForm.token model.loginForm)
    }

init : Maybe PersistedModel -> ( Model, Cmd Msg )
init maybePersistedModel =
    let maybeModel =
        Maybe.map fromPersistedModel maybePersistedModel
    in
  update FetchUserInfo (Maybe.withDefault emptyModel maybeModel)

emptyModel : Model
emptyModel =
    let (loginModel, loginCmd) =
        LoginForm.init Nothing
    in
    { loginForm = loginModel
    , userInfo = Nothing
    }

fromPersistedModel : PersistedModel -> Model
fromPersistedModel persistedModel =
    let (loginModel, loginCmd) =
        LoginForm.init persistedModel.token
    in
    { loginForm = loginModel
    , userInfo = Nothing
    }

-- UPDATE


type Msg
    = Login LoginForm.Msg
    | FetchUserInfo
    | FetchUserInfoSucceed Api.UserInfo
    | FetchUserInfoFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Login message ->
      let ( loginModel, loginCmd ) =
        LoginForm.update message model.loginForm
      in
        ({ model
            | loginForm = loginModel
         }
         , Cmd.map Login loginCmd
        )
    FetchUserInfo ->
        (model, (Api.fetchUserInfo (Maybe.withDefault "" (LoginForm.token model.loginForm)) FetchUserInfoFail FetchUserInfoSucceed))

    FetchUserInfoSucceed userInfo ->
        ({model | userInfo = Just userInfo}, Cmd.none)

    FetchUserInfoFail error ->
        (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ App.map Login (LoginForm.view model.loginForm)
    , div []
        [ span [] [text (Maybe.withDefault "no token" (LoginForm.token model.loginForm))]
        ]
    , button [ onClick FetchUserInfo] [ text "Fetch User Info" ]
    , div []
        [ span [] [text (Maybe.withDefault "no id" (Maybe.map (\ui -> ui.id) model.userInfo))]
        ]
    ]
