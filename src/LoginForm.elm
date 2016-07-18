module LoginForm exposing (Model, Msg, init, update, view, token)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (AppConfig)
import AccessTokenParser


type alias Model =
    { username : String
    , password : String
    , token : Maybe String
    , basicHeader : String
    , googleClientId : String
    }


emptyModel : Model
emptyModel =
    { username = ""
    , password = ""
    , token = Nothing
    , basicHeader = ""
    , googleClientId = ""
    }


token : Model -> Maybe String
token model =
    model.token


init : Maybe String -> String -> ( Model, Cmd Msg )
init maybeToken hash =
    let
        initModel =
            { emptyModel
                | token = maybeToken
            }

        ( modelWithClientId, appConfigCmd ) =
            update AppConfigFetch initModel
    in
        case AccessTokenParser.parse hash of
            Just googleAccessToken ->
                let
                    ( loginModel, loginCmd ) =
                        update (LoginWithGoogle googleAccessToken) modelWithClientId
                in
                    ( loginModel, Cmd.batch [ appConfigCmd, loginCmd ] )

            Nothing ->
                ( modelWithClientId, appConfigCmd )


type Msg
    = Name String
    | Password String
    | AppConfigFetch
    | AppConfigFetchSucceed AppConfig
    | AppConfigFetchFail Api.Error
    | Login
    | LoginWithGoogle String
    | LoginSucceed String
    | LoginFail Api.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        AppConfigFetch ->
            ( model, (Api.fetchAppConfig AppConfigFetchFail AppConfigFetchSucceed) )

        AppConfigFetchSucceed appConfig ->
            ( { model | googleClientId = appConfig.googleClientId }, Cmd.none )

        AppConfigFetchFail error ->
            ( model, Cmd.none )

        Login ->
            ( model, (Api.authenticate model.username model.password LoginFail LoginSucceed) )

        LoginWithGoogle accessToken ->
            ( model, (Api.authenticateWithGoogle accessToken LoginFail LoginSucceed) )

        LoginSucceed token ->
            ( { model | token = Just token }, Cmd.none )

        LoginFail error ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type' "text", placeholder "Name", value model.username, onInput Name ] []
        , input [ type' "password", placeholder "Password", onInput Password ] []
        , button [ onClick Login ] [ text "Login" ]
        , a [ href (googleOauthUrl model.googleClientId) ] [ text "Google Login" ]
        ]


googleOauthUrl : String -> String
googleOauthUrl googleClientId =
    "https://accounts.google.com/o/oauth2/v2/auth?response_type=token&client_id="
        ++ googleClientId
        ++ "&redirect_uri=http://localhost:8000&state=some_state&scope=email profile"
