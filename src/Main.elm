port module Main exposing (..)

import LoginForm
import UserInfo
import AuthenticatedUserView
import Html exposing (..)
import Html.App as App
import Navigation
import Result


--import Html.Events exposing (..)
--import Html.Attributes
--import Api
--import Models exposing (Authentication)

import Debug


main : Program (Maybe PersistedModel)
main =
    Navigation.programWithFlags
        urlParser
        { init = init
        , view = view
        , update = (\msg model -> withSetStorage (Debug.log "model" (update msg model)))
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }


port setStorage : PersistedModel -> Cmd msg


fromUrl : String -> Result String String
fromUrl url =
    Result.fromMaybe "impossible" (Just url)


urlParser : Navigation.Parser (Result String String)
urlParser =
    Navigation.makeParser (fromUrl << .hash)


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
    , loginFormModel : LoginForm.Model
    , userInfoModel : UserInfo.Model
    , maybeAuthenticatedUserViewModel : Maybe AuthenticatedUserView.Model
    }


toPersistedModel : Model -> PersistedModel
toPersistedModel model =
    { token = LoginForm.token model.loginFormModel
    }


emptyPersistedModel : PersistedModel
emptyPersistedModel =
    { token = Nothing }


init : Maybe PersistedModel -> Result String String -> ( Model, Cmd Msg )
init maybePersistedModel hash =
    let
        persistedModel =
            Maybe.withDefault emptyPersistedModel maybePersistedModel

        ( loginFormModel, loginFormCmd ) =
            LoginForm.init persistedModel.token (Result.withDefault "" hash)

        ( userInfoModel, userInfoCmd ) =
            UserInfo.init persistedModel.token
    in
        ( { activePage = authTokenToPage persistedModel.token
          , loginFormModel = loginFormModel
          , userInfoModel = userInfoModel
          , maybeAuthenticatedUserViewModel = Nothing
          }
        , Cmd.batch
            [ Cmd.map LoginFormMsg loginFormCmd
            , Cmd.map UserInfoMsg userInfoCmd
            ]
        )


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
    | UserInfoMsg UserInfo.Msg
    | AuthenticatedUserViewMsg AuthenticatedUserView.Msg


urlUpdate : Result String String -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        LoginFormMsg message ->
            let
                ( loginFormModel, loginFormCmd ) =
                    LoginForm.update message model.loginFormModel
            in
                case
                    ( LoginForm.token model.loginFormModel, LoginForm.token loginFormModel )
                of
                    ( Nothing, Just token ) ->
                        let
                            ( userInfoModel, userInfoCmd ) =
                                UserInfo.init (Just token)
                        in
                            ( { model
                                | loginFormModel = loginFormModel
                                , userInfoModel = userInfoModel
                                , activePage = authTokenToPage <| LoginForm.token loginFormModel
                              }
                            , Cmd.batch [ Cmd.map LoginFormMsg loginFormCmd, Cmd.map UserInfoMsg userInfoCmd ]
                            )

                    _ ->
                        ( { model
                            | loginFormModel = loginFormModel
                            , activePage = authTokenToPage <| LoginForm.token loginFormModel
                          }
                        , Cmd.map LoginFormMsg loginFormCmd
                        )

        UserInfoMsg message ->
            let
                ( userInfoModel, userInfoCmd ) =
                    UserInfo.update message model.userInfoModel
            in
                case
                    ( LoginForm.token model.loginFormModel
                    , Maybe.map (\ui -> ui.id) <| UserInfo.userInfo userInfoModel
                    , (UserInfo.userInfo model.userInfoModel)
                    )
                of
                    ( Just authToken, Just userId, Nothing ) ->
                        let
                            ( authenticatedUserViewModel, authenticatedUserViewCmd ) =
                                AuthenticatedUserView.init { userId = userId, token = authToken }
                        in
                            ( { model
                                | userInfoModel = userInfoModel
                                , maybeAuthenticatedUserViewModel = Just authenticatedUserViewModel
                              }
                            , Cmd.batch
                                [ Cmd.map UserInfoMsg userInfoCmd
                                , Cmd.map AuthenticatedUserViewMsg authenticatedUserViewCmd
                                ]
                            )

                    _ ->
                        ( { model | userInfoModel = userInfoModel }
                        , Cmd.map UserInfoMsg userInfoCmd
                        )

        -- Nothing
        AuthenticatedUserViewMsg message ->
            case model.maybeAuthenticatedUserViewModel of
                Just oldAuthenticatedUserViewModel ->
                    let
                        ( authenticatedUserViewModel, authenticatedUserViewCmd ) =
                            AuthenticatedUserView.update message oldAuthenticatedUserViewModel
                    in
                        ( { model
                            | maybeAuthenticatedUserViewModel = Just authenticatedUserViewModel
                          }
                        , Cmd.map AuthenticatedUserViewMsg authenticatedUserViewCmd
                        )

                Nothing ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ span [] [ text <| toAuthToken model ]
            ]
        , App.map UserInfoMsg (UserInfo.view model.userInfoModel)
        , div []
            [ span [] [ text <| toString model ]
            ]
        , pageView model
        ]


pageView : Model -> Html Msg
pageView model =
    case (model.activePage) of
        LoginPage ->
            App.map LoginFormMsg (LoginForm.view model.loginFormModel)

        ReceiptListPage ->
            case model.maybeAuthenticatedUserViewModel of
                Just authenticatedUserViewModel ->
                    App.map AuthenticatedUserViewMsg (AuthenticatedUserView.view authenticatedUserViewModel)

                Nothing ->
                    div [] []

        LoadingPage ->
            span [] [ text "Loading page" ]


toAuthToken : Model -> String
toAuthToken model =
    Maybe.withDefault "no token" (LoginForm.token model.loginFormModel)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.maybeAuthenticatedUserViewModel of
        Just authenticatedUserViewModel ->
            Sub.map AuthenticatedUserViewMsg (AuthenticatedUserView.subscriptions authenticatedUserViewModel)

        Nothing ->
            Sub.none
