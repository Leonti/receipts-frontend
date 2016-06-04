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
    authenticationToken : Maybe String
  }

emptyModel : Model
emptyModel =
    let (loginModel, loginCmd) =
        LoginForm.init
    in
    { loginForm = loginModel
    , authenticationToken = Nothing
    }

persistedModel : Model -> PersistedModel
persistedModel model =
    { token = (LoginForm.token model.loginForm)
    }

-- should use LoginForm.init
init : Maybe PersistedModel -> ( Model, Cmd Msg )
init maybePersistedModel =
    let maybeModel =
        Maybe.map fromPersistedModel maybePersistedModel
    in
  (Maybe.withDefault emptyModel maybeModel ! [])

fromPersistedModel : PersistedModel -> Model
fromPersistedModel persistedModel =
    { emptyModel
        | authenticationToken = persistedModel.token
    }

-- UPDATE


type Msg
    = Login LoginForm.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Login message ->
      let ( loginModel, loginCmd ) =
        LoginForm.update message model.loginForm
      in
        ({ model
            | loginForm = loginModel
            , authenticationToken = LoginForm.token loginModel
         }
         , Cmd.map Login loginCmd
        )

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ App.map Login (LoginForm.view model.loginForm)
    , div []
        [ span [] [text (Maybe.withDefault "no token" model.authenticationToken)]
        ]
    ]
