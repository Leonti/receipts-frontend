port module Main exposing (..)

import LoginForm
import UserInfo
import AuthenticatedUserView
import Html exposing (..)
import Navigation
import Material
import Material.Layout as Layout
import Material.Typography as Typography
import Material.Options as Options exposing (css, when)


--import Html.Events exposing (..)
--import Html.Attributes
--import Api
--import Models exposing (Authentication)


type alias ParsedLocation =
    { host : String
    , hash : String
    }


main : Program (Maybe PersistedModel) Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = \msg model -> withSetStorage (update msg model)
        , subscriptions = subscriptions
        }


port setStorage : PersistedModel -> Cmd msg


fromLocation : Navigation.Location -> ParsedLocation
fromLocation location =
    { host = location.protocol ++ "//" ++ location.host
    , hash = location.hash
    }


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
    , mdl : Material.Model
    }


toPersistedModel : Model -> PersistedModel
toPersistedModel model =
    { token = LoginForm.token model.loginFormModel
    }


emptyPersistedModel : PersistedModel
emptyPersistedModel =
    { token = Nothing }


init : Maybe PersistedModel -> Navigation.Location -> ( Model, Cmd Msg )
init maybePersistedModel location =
    let
        persistedModel =
            Maybe.withDefault emptyPersistedModel maybePersistedModel

        parsedLocation =
            fromLocation location

        ( loginFormModel, loginFormCmd ) =
            LoginForm.init persistedModel.token parsedLocation.host parsedLocation.hash

        ( userInfoModel, userInfoCmd ) =
            UserInfo.init persistedModel.token
    in
        ( { activePage = authTokenToPage persistedModel.token
          , loginFormModel = loginFormModel
          , userInfoModel = userInfoModel
          , maybeAuthenticatedUserViewModel = Nothing
          , mdl = Material.model
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
    = UrlChange Navigation.Location
    | LoginFormMsg LoginForm.Msg
    | UserInfoMsg UserInfo.Msg
    | AuthenticatedUserViewMsg AuthenticatedUserView.Msg
    | Mdl (Material.Msg Msg)


urlUpdate : ParsedLocation -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( model, Cmd.none )

        Mdl message ->
            Material.update Mdl message model

        -- insert page changing logic here
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
        [ Layout.render Mdl
            model.mdl
            []
            { header = header model
            , drawer = drawer model
            , tabs = ( [], [] )
            , main = [ (pageView model) ]
            }
          --Html.map UserInfoMsg (UserInfo.view model.userInfoModel)
          --, pageView model
        ]


e404 : Model -> Html Msg
e404 _ =
    div
        []
        [ Options.styled Html.h1
            [ Options.cs "mdl-typography--display-4"
            , Typography.center
            ]
            [ text "404" ]
        ]


drawer : Model -> List (Html Msg)
drawer model =
    case model.maybeAuthenticatedUserViewModel of
        Just authenticatedUserViewModel ->
            [ Layout.title [] [ text "Receipts" ]
            , Layout.navigation
                []
                [ Html.map AuthenticatedUserViewMsg (AuthenticatedUserView.drawerView authenticatedUserViewModel) ]
            ]

        Nothing ->
            []


header : Model -> List (Html Msg)
header model =
    [ Layout.row
        [ Options.nop
        , css "transition" "height 333ms ease-in-out 0s"
        ]
        [ Layout.title [] [ text "Receipts" ]
        , Layout.spacer
        , Layout.navigation []
            []
        ]
    ]


pageView : Model -> Html Msg
pageView model =
    case (model.activePage) of
        LoginPage ->
            Html.map LoginFormMsg (LoginForm.view model.loginFormModel)

        ReceiptListPage ->
            case model.maybeAuthenticatedUserViewModel of
                Just authenticatedUserViewModel ->
                    Html.map AuthenticatedUserViewMsg (AuthenticatedUserView.view authenticatedUserViewModel)

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
