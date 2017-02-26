module LoginForm exposing (Model, Msg, init, update, view, token)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Models exposing (AppConfig)
import AccessTokenParser
import Material
import Material.Options as Options
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Typography as Typo


type alias Model =
    { username : String
    , password : String
    , host : String
    , token : Maybe String
    , basicHeader : String
    , googleClientId : String
    , mdl : Material.Model
    }


emptyModel : Model
emptyModel =
    { username = ""
    , password = ""
    , host = ""
    , token = Nothing
    , basicHeader = ""
    , googleClientId = ""
    , mdl = Material.model
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
    | Mdl (Material.Msg Msg)


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

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


view : Model -> Html Msg
view model =
    loginForm model


loginForm : Model -> Html Msg
loginForm model =
    div [ Html.Attributes.class "login-dialog-wrapper" ]
        [ div [ Html.Attributes.class "login-dialog mdl-shadow--2dp" ]
            [ div [ Html.Attributes.class "email-field" ]
                [ Textfield.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Textfield.label "Email"
                    , Textfield.floatingLabel
                    , Textfield.value model.username
                    , Options.onInput Name
                    ]
                    []
                ]
            , div [ Html.Attributes.class "email-field" ]
                [ Textfield.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Textfield.label "Password"
                    , Textfield.floatingLabel
                    , Textfield.password
                    , Options.onInput Password
                    ]
                    []
                ]
            , div [ Html.Attributes.class "login-button" ]
                [ Button.render Mdl
                    [ 2 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick Login
                    ]
                    [ text "Login" ]
                ]
            , div [ Html.Attributes.class "login-or" ]
                [ Options.styled p
                    [ Typo.subhead ]
                    [ text "OR" ]
                ]
            , div [ Html.Attributes.class "google-login-button" ]
                [ Button.render Mdl
                    [ 3 ]
                    model.mdl
                    [ Button.ripple
                    , Button.colored
                    , Button.raised
                    , Button.link (googleOauthUrl model.host model.googleClientId)
                    ]
                    [ text "Login With Google" ]
                ]
            ]
        ]


googleOauthUrl : String -> String -> String
googleOauthUrl host googleClientId =
    "https://accounts.google.com/o/oauth2/v2/auth?response_type=token&client_id="
        ++ googleClientId
        ++ "&redirect_uri="
        ++ host
        ++ "&state=some_state&scope=email profile"
