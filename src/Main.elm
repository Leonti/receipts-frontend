port module Main exposing (..)

import LoginForm
import ReceiptList
import UserInfo
import Html exposing (..)
import Html.App as App


--import Html.Events exposing (..)
--import Html.Attributes
--import Api
--import Models exposing (Authentication)

import Debug


main : Program (Maybe PersistedModel)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = (\msg model -> withSetStorage (Debug.log "model" (update msg model)))
        , subscriptions = subscriptions
        }


port setStorage : PersistedModel -> Cmd msg


withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage ( model, cmds ) =
    ( model
    , Cmd.batch
        [ setStorage (toPersistedModel model), cmds ]
    )



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
    , userInfo : UserInfo.Model
    }


toPersistedModel : Model -> PersistedModel
toPersistedModel model =
    { token = model.authToken
    }


emptyPersistedModel : PersistedModel
emptyPersistedModel =
    { token = Nothing }


init : Maybe PersistedModel -> ( Model, Cmd Msg )
init maybePersistedModel =
    let
        persistedModel =
            Maybe.withDefault emptyPersistedModel maybePersistedModel

        ( loginFormModel, loginFormCmd ) =
            LoginForm.init persistedModel.token

        ( userInfoModel, userInfoCmd ) =
            UserInfo.init persistedModel.token

        ( receiptListModel, receiptListCmd ) =
            ReceiptList.init <|
                Maybe.map2
                    (\token userInfo ->
                        { userId = userInfo.id
                        , token = token
                        }
                    )
                    persistedModel.token
                    (UserInfo.userInfo userInfoModel)
    in
        ( { activePage = LoginPage
          , authToken = Nothing
          , loginForm = loginFormModel
          , userInfo = userInfoModel
          , receiptList = receiptListModel
          }
          -- proper commands here
        , Cmd.batch
            [ Cmd.map LoginFormMsg loginFormCmd
            , Cmd.map UserInfoMsg userInfoCmd
            , Cmd.map ReceiptListMsg receiptListCmd
            ]
        )



--initUserInfo : Model -> String -> ( Model, Cmd Msg )
--initUserInfo model authToken =
--    let
--        ( userInfoModel, userInfoCmd ) =
--            UserInfo.init authToken
--    in
--        ( { model
--            | userInfo = userInfoModel
--          }
--        , Cmd.map UserInfoMsg userInfoCmd
--        )


authTokenToPage : Maybe String -> Page
authTokenToPage maybeAuthToken =
    case (maybeAuthToken) of
        Just authToken ->
            ReceiptListPage

        Nothing ->
            LoginPage



-- UPDATE


type Msg
    = LoginFormMsg LoginForm.Msg
    | ReceiptListMsg ReceiptList.Msg
    | UserInfoMsg UserInfo.Msg



--    | Init
--    | InitUserInfoSucceed UserInfo
--    | FetchUserInfo
--    | FetchUserInfoSucceed UserInfo
--    | FetchUserInfoFail Api.Error
-- https://github.com/afcastano/elm-nested-component-communication


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        LoginFormMsg message ->
            let
                ( loginModel, loginCmd ) =
                    LoginForm.update message model.loginForm
            in
                ( { model
                    | loginForm = loginModel
                    , authToken = LoginForm.token loginModel
                    , activePage = authTokenToPage <| LoginForm.token loginModel
                  }
                , Cmd.map LoginFormMsg loginCmd
                )

        ReceiptListMsg message ->
            let
                ( receiptListModel, receiptListCmd ) =
                    ReceiptList.update message model.receiptList
            in
                ( { model
                    | receiptList = receiptListModel
                  }
                , Cmd.map ReceiptListMsg receiptListCmd
                )

        UserInfoMsg message ->
            let
                ( userInfoModel, userInfoCmd ) =
                    UserInfo.update message model.userInfo
            in
                case
                    ( model.authToken
                    , Maybe.map (\ui -> ui.id) <| UserInfo.userInfo userInfoModel
                    , (UserInfo.userInfo model.userInfo)
                    )
                of
                    ( Just authToken, Just userId, Nothing ) ->
                        let
                            ( receiptListModel, receiptListCmd ) =
                                ReceiptList.init (Just { userId = userId, token = authToken })
                        in
                            ( { model
                                | userInfo = userInfoModel
                                , receiptList = receiptListModel
                              }
                            , Cmd.batch [ Cmd.map UserInfoMsg userInfoCmd, Cmd.map ReceiptListMsg receiptListCmd ]
                            )

                    _ ->
                        ( { model | userInfo = userInfoModel }
                        , Cmd.map UserInfoMsg userInfoCmd
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
        [ div []
            [ span [] [ text <| toAuthToken model ]
            ]
          --        , button [ onClick FetchUserInfo] [ text "Fetch User Info" ]
        , App.map UserInfoMsg (UserInfo.view model.userInfo)
        , div []
            [ span [] [ text <| toString model ]
            ]
        , pageView model
        ]


pageView : Model -> Html Msg
pageView model =
    case (model.activePage) of
        LoginPage ->
            App.map LoginFormMsg (LoginForm.view model.loginForm)

        ReceiptListPage ->
            App.map ReceiptListMsg (ReceiptList.view model.receiptList)

        LoadingPage ->
            span [] [ text "Loading page" ]


toAuthToken : Model -> String
toAuthToken model =
    Maybe.withDefault "no token" (LoginForm.token model.loginForm)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ReceiptListMsg (ReceiptList.subscriptions model.receiptList)
