import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task
import Base64


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL


type alias Model =
  { username : String
  , password : String
  , token: String
  , basicHeader: String
  }

init : (Model, Cmd Msg)
init =
  ( Model "" "" "" ""
  , Cmd.none
  )

-- UPDATE


type Msg
    = Name String
    | Password String
    | Login
    | LoginSucceed String
    | LoginFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Name username ->
      ({ model | username = username }, Cmd.none)

    Password password ->
      ({ model | password = password }, Cmd.none)

    Login ->
        (model, (authenticate model.username model.password))

    LoginSucceed token ->
        ({model | token = token}, Cmd.none)

    LoginFail error ->
        (model, Cmd.none)

authenticate : String -> String -> Cmd Msg
authenticate username password =
    let basicAuthHeaderResult =
        basicAuthHeader username password
    in
        case basicAuthHeaderResult of
            Result.Ok header -> Task.perform LoginFail LoginSucceed (authenticationGet header)
            Result.Err error -> Cmd.none


basicAuthHeader : String -> String -> Result String String
basicAuthHeader username password = Result.map (\s -> "Basic " ++ s) (Base64.encode (username ++ ":" ++ password))

authenticationGet : String -> Task.Task Http.Error String
authenticationGet basicAuthHeader =
    let request =
        { verb = "GET"
        , headers = [("Authorization", basicAuthHeader)]
        , url = "https://api.receipts.leonti.me/token/create"
        , body = Http.empty
        }
    in
        Http.fromJson (Json.at["access_token"] Json.string) (Http.send Http.defaultSettings request)


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , button [ onClick Login] [ text "Login" ]
    , div []
        [ span [] [text model.token]
        ]
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
