module ReceiptForm exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { total : String
    , description : String
    }


init : ( Model, Cmd Msg )
init =
    ({ total = ""
     , description = ""
     }
        ! []
    )


type Msg
    = TotalChange String
    | DescriptionChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TotalChange total ->
            ( { model
                | total = total
              }
            , Cmd.none
            )

        DescriptionChange description ->
            ( { model
                | description = description
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ input [ type' "text", placeholder "Total", value model.total, onInput TotalChange ] []
        , textarea [ placeholder "Notes", onInput DescriptionChange ] []
        ]
