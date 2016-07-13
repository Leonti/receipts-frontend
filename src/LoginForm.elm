module LoginForm exposing (Model, Msg, init, update, view, token)

import Api
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


type alias Model =
    { username : String
    , password : String
    , token : Maybe String
    , basicHeader : String
    }


emptyModel : Model
emptyModel =
    { username = ""
    , password = ""
    , token = Nothing
    , basicHeader = ""
    }


token : Model -> Maybe String
token model =
    model.token


init : Maybe String -> ( Model, Cmd Msg )
init maybeToken =
    { emptyModel
        | token = maybeToken
    }
        ! []


type Msg
    = Name String
    | Password String
    | Login
    | LoginSucceed String
    | LoginFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name username ->
            ( { model | username = username }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            ( model, (Api.authenticate model.username model.password LoginFail LoginSucceed) )

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
        ]
