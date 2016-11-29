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
    , host : String
    , token : Maybe String
    , basicHeader : String
    , googleClientId : String
    }


emptyModel : Model
emptyModel =
    { username = ""
    , password = ""
    , host = ""
    , token = Nothing
    , basicHeader = ""
    , googleClientId = ""
    }


token : Model -> Maybe String
token model =
    model.token


init : Maybe String -> String -> String -> ( Model, Cmd Msg )
init maybeToken host hash =
    let
        initModel =
            { emptyModel
                | token = maybeToken
                , host = host
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
    | AppConfigFetchResult (Result Api.Error AppConfig)
    | Login
    | LoginWithGoogle String
    | LoginResult (Result Api.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        AppConfigFetch ->
            ( model, (Api.fetchAppConfig AppConfigFetchResult) )

        AppConfigFetchResult (Ok appConfig) ->
            ( { model | googleClientId = appConfig.googleClientId }, Cmd.none )

        AppConfigFetchResult (Err error) ->
            ( model, Cmd.none )

        Login ->
            ( model, (Api.authenticate model.username model.password LoginResult) )

        LoginWithGoogle accessToken ->
            ( model, (Api.authenticateWithGoogle accessToken LoginResult) )

        LoginResult (Ok token) ->
            ( { model | token = Just token }, Cmd.none )

        LoginResult (Err error) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Name", value model.username, onInput Name ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , button [ onClick Login ] [ text "Login" ]
        , a [ href (googleOauthUrl model.host model.googleClientId) ] [ text "Google Login" ]
        ]


googleOauthUrl : String -> String -> String
googleOauthUrl host googleClientId =
    "https://accounts.google.com/o/oauth2/v2/auth?response_type=token&client_id="
        ++ googleClientId
        ++ "&redirect_uri="
        ++ host
        ++ "&state=some_state&scope=email profile"
