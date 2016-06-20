port module Main exposing (..)

import LoginForm
import ReceiptList
import Html exposing (..)
import Html.App as App
--import Html.Events exposing (..)
--import Html.Attributes
--import Api
import Models exposing (UserInfo, Receipt)
import Debug

main : Program (Maybe PersistedModel)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = (\msg model -> withSetStorage (Debug.log "model" (update msg model)) )
    , subscriptions = subscriptions
    }

port setStorage : PersistedModel -> Cmd msg

withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage (model, cmds) =
  ( model, Cmd.batch
    [ setStorage (persistedModel model), cmds ] )

-- MODEL

type Page
    = LoginPage
    | ReceiptListPage
    | LoadingPage

type alias PersistedModel =
    { token : Maybe String
    }

type alias Model =
  { activePage : Page
  , authToken : Maybe String
  , loginForm : LoginForm.Model
  , receiptList : ReceiptList.Model
  , userInfo : Maybe UserInfo
  }

emptyModel : Model
emptyModel =
    { activePage = LoadingPage
    , authToken = Nothing
    , loginForm = LoginForm.emptyModel
    , receiptList = ReceiptList.emptyModel
    , userInfo = Nothing
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
  (Maybe.withDefault emptyModel maybeModel, Cmd.none)

fromPersistedModel : PersistedModel -> Model
fromPersistedModel persistedModel =
    { emptyModel
        | authToken = persistedModel.token
    }

-- UPDATE


type Msg
    = LoginMsg LoginForm.Msg
    | ReceiptListMsg ReceiptList.Msg
--    | Init
--    | InitUserInfoSucceed UserInfo
--    | FetchUserInfo
--    | FetchUserInfoSucceed UserInfo
--    | FetchUserInfoFail Api.Error

-- https://github.com/afcastano/elm-nested-component-communication
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (Debug.log "msg" msg) of
    LoginMsg message ->
      let ( loginModel, loginCmd ) =
        LoginForm.update message model.loginForm
      in
        ({ model
            | loginForm = loginModel
         }
         , Cmd.map LoginMsg loginCmd
        )

    ReceiptListMsg message ->
      let ( receiptListModel, receiptListCmd ) =
        ReceiptList.update message model.receiptList
      in
        ({ model
            | receiptList = receiptListModel
         }
         , Cmd.map ReceiptListMsg receiptListCmd
        )

--    Init ->
--        (model, Api.fetchUserInfo (Maybe.withDefault "" (LoginForm.token model.loginForm)) FetchUserInfoFail InitUserInfoSucceed)

--    InitUserInfoSucceed userInfo ->
--        update FetchReceipts (Debug.log "init fetch user into succeed" {model | userInfo = Just userInfo})

--    FetchUserInfo ->
--        (model, Api.fetchUserInfo (Maybe.withDefault "" (LoginForm.token model.loginForm)) FetchUserInfoFail FetchUserInfoSucceed)

--    FetchUserInfoSucceed userInfo ->
--        update FetchReceipts (Debug.log "fetch user into succeed" {model | userInfo = Just userInfo})

--    FetchUserInfoFail error ->
--        (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ App.map LoginMsg (LoginForm.view model.loginForm)
        , div []
            [ span [] [text <| toAuthToken model]
            ]
--        , button [ onClick FetchUserInfo] [ text "Fetch User Info" ]
        , div []
            [ span [] [text (toUserId model)]
            ]
        , div []
            [ span [] [text (toString model)]
            ]
        , App.map ReceiptListMsg (ReceiptList.view model.receiptList)
        ]

toUserId : Model -> String
toUserId model =
    Maybe.withDefault "no id" (Maybe.map (\ui -> ui.id) model.userInfo)

toAuthToken : Model -> String
toAuthToken model =
    Maybe.withDefault "no token" (LoginForm.token model.loginForm)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map ReceiptListMsg (ReceiptList.subscriptions model.receiptList)
