port module Main exposing (..)

import LoginForm
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main =
  App.programWithFlags
    { init = init
    , view = view
    , update = (\msg model -> withSetStorage (update msg model))
    , subscriptions = \_ -> Sub.none
    }

port setStorage : Model -> Cmd msg

withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage (model, cmds) =
  ( model, Cmd.batch [ setStorage model, cmds ] )

-- MODEL


type alias Model =
  { loginForm : LoginForm.Model
  }

emptyModel : Model
emptyModel =
    { loginForm = LoginForm.emptyModel
    }

-- should use LoginForm.init
init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []

-- UPDATE


type Msg
    = Login LoginForm.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Login message ->
      let ( loginModel, loginCmd ) =
        LoginForm.update message model.loginForm
      in ({ model | loginForm = loginModel }, Cmd.map Login loginCmd)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ App.map Login (LoginForm.view model.loginForm)
    ]
