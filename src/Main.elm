port module Main exposing (..)

import LoginForm
import Html exposing (..)
import Html.Attributes
import Html.App as App
import Html.Events exposing (..)
import Api
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

type alias PersistedModel =
    { token : Maybe String
    }

type alias Model =
  { loginForm : LoginForm.Model
  , userInfo : Maybe UserInfo
  , receipts : List Receipt
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
  update Init (Maybe.withDefault emptyModel maybeModel)

emptyModel : Model
emptyModel =
    let (loginModel, loginCmd) =
        LoginForm.init Nothing
    in
    { loginForm = loginModel
    , userInfo = Nothing
    , receipts = []
    }

fromPersistedModel : PersistedModel -> Model
fromPersistedModel persistedModel =
    let (loginModel, loginCmd) =
        LoginForm.init persistedModel.token
    in
    { loginForm = loginModel
    , userInfo = Nothing
    , receipts = []
    }

-- UPDATE


type Msg
    = Login LoginForm.Msg
    | Init
    | InitUserInfoSucceed UserInfo
    | FetchUserInfo
    | FetchUserInfoSucceed UserInfo
    | FetchUserInfoFail Api.Error
    | FetchReceipts
    | FetchReceiptsSucceed (List Receipt)
    | FetchReceiptsFail Api.Error
    | LoadImage LoadImageParams
    | LoadImageSucceed LoadImageResult

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (Debug.log "msg" msg) of
    Login message ->
      let ( loginModel, loginCmd ) =
        LoginForm.update message model.loginForm
      in
        ({ model
            | loginForm = loginModel
         }
         , Cmd.map Login loginCmd
        )

    Init ->
        (model, Api.fetchUserInfo (Maybe.withDefault "" (LoginForm.token model.loginForm)) FetchUserInfoFail InitUserInfoSucceed)

    InitUserInfoSucceed userInfo ->
        update FetchReceipts (Debug.log "init fetch user into succeed" {model | userInfo = Just userInfo})

    FetchUserInfo ->
        (model, Api.fetchUserInfo (Maybe.withDefault "" (LoginForm.token model.loginForm)) FetchUserInfoFail FetchUserInfoSucceed)

    FetchUserInfoSucceed userInfo ->
        update FetchReceipts (Debug.log "fetch user into succeed" {model | userInfo = Just userInfo})

    FetchUserInfoFail error ->
        (model, Cmd.none)

    FetchReceipts ->
        let
            token = Maybe.withDefault "no-token" (LoginForm.token model.loginForm)
            userId = Maybe.withDefault "no-id" (Maybe.map (\ui -> ui.id) model.userInfo)
        in
        (Debug.log "fetchReceipts start" model, Api.fetchReceipts token userId FetchReceiptsFail FetchReceiptsSucceed)

    FetchReceiptsSucceed receipts ->
        ({ model | receipts = List.take 10 receipts }, Cmd.none)

    FetchReceiptsFail error ->
        (model, Cmd.none)

    LoadImage loadImageParams ->
        (model, loadImage loadImageParams)

    LoadImageSucceed loadImageResult ->
        (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ App.map Login (LoginForm.view model.loginForm)
        , div []
            [ span [] [text <| toAuthToken model]
            ]
        , button [ onClick FetchUserInfo] [ text "Fetch User Info" ]
        , div []
            [ span [] [text (toUserId model)]
            ]
        , div []
            [text "Receipts:"]
        , div [] (List.map (\receipt -> receiptView (toUserId model) (toAuthToken model) receipt)  model.receipts)
        , img [Html.Attributes.id "image"] []
        ]

toUserId : Model -> String
toUserId model =
    Maybe.withDefault "no id" (Maybe.map (\ui -> ui.id) model.userInfo)

toAuthToken : Model -> String
toAuthToken model =
    Maybe.withDefault "no token" (LoginForm.token model.loginForm)

receiptView : String -> String -> Receipt -> Html Msg
receiptView userId authToken receipt =
    div []
        [ div []
            [ text receipt.id ]
        , div [] (List.map (\file -> div []
            [text ""
            , button [ onClick <| LoadImage <| LoadImageParams (Api.baseUrl ++ "/user/" ++ userId ++ "/receipt/" ++ receipt.id ++ "/file/" ++ file.id ++ "." ++ file.ext) authToken file.id]
                [ text "Load image" ]
            ]
            ) receipt.files)
        ]

port imageLoaded : (LoadImageResult -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  imageLoaded LoadImageSucceed

type alias LoadImageParams =
    { url : String
    , authToken : String
    , fileId : String
    }

type alias LoadImageResult =
    { fileId : String
    , imageData : String
    }

port loadImage : LoadImageParams -> Cmd msg
