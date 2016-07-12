port module Main exposing (..)

import LoginForm
import ReceiptList
import UserInfo
import Html exposing (..)
import Html.App as App


--import Html.Events exposing (..)
--import Html.Attributes
--import Api
--import Models exposing (Receipt)

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
        [ setStorage (persistedModel model), cmds ]
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


emptyModel : Model
emptyModel =
    { activePage = LoginPage
    , authToken = Nothing
    , loginForm = LoginForm.emptyModel
    , receiptList = ReceiptList.emptyModel
    , userInfo = UserInfo.emptyModel
    }


persistedModel : Model -> PersistedModel
persistedModel model =
    { token = model.authToken
    }


init : Maybe PersistedModel -> ( Model, Cmd Msg )
init maybePersistedModel =
    let
        model =
            (Maybe.withDefault emptyModel (Maybe.map fromPersistedModel maybePersistedModel))
    in
        case (model.authToken) of
            Just authToken ->
                initUserInfo model authToken

            Nothing ->
                ( emptyModel, Cmd.none )


initUserInfo : Model -> String -> ( Model, Cmd Msg )
initUserInfo model authToken =
    let
        ( userInfoModel, userInfoCmd ) =
            UserInfo.init authToken
    in
        ( { model
            | userInfo = userInfoModel
          }
        , Cmd.map UserInfoMsg userInfoCmd
        )


fromPersistedModel : PersistedModel -> Model
fromPersistedModel persistedModel =
    { emptyModel
        | authToken = persistedModel.token
        , activePage = authTokenToPage persistedModel.token
    }


authTokenToPage : Maybe String -> Page
authTokenToPage maybeAuthToken =
    case (maybeAuthToken) of
        Just authToken ->
            ReceiptListPage

        Nothing ->
            LoginPage



-- UPDATE


type Msg
    = LoginMsg LoginForm.Msg
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
        LoginMsg message ->
            let
                ( loginModel, loginCmd ) =
                    LoginForm.update message model.loginForm
            in
                ( { model
                    | loginForm = loginModel
                    , authToken = LoginForm.token loginModel
                    , activePage = authTokenToPage <| LoginForm.token loginModel
                  }
                , Cmd.map LoginMsg loginCmd
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
                let
                    model =
                        { model | userInfo = userInfoModel }
                in
                    case ( model.authToken, Maybe.map (\ui -> ui.id) <| UserInfo.userInfo userInfoModel ) of
                        ( Just authToken, Just userId ) ->
                            let
                                ( receiptListModel, receiptListCmd ) =
                                    ReceiptList.init userId authToken
                            in
                                ( { model
                                    | receiptList = receiptListModel
                                  }
                                , Cmd.batch [ Cmd.map UserInfoMsg userInfoCmd, Cmd.map ReceiptListMsg receiptListCmd ]
                                )

                        _ ->
                            ( model
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
            App.map LoginMsg (LoginForm.view model.loginForm)

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
