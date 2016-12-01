module ReceiptForm exposing (Model, Msg, init, update, view, formData)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Models exposing (ReceiptFormData)


type alias Model =
    { total : String
    , description : String
    , timestamp : Int
    , tags : List String
    }


init : ReceiptFormData -> ( Model, Cmd Msg )
init receiptFormData =
    ({ total = ""
     , description = ""
     , timestamp = receiptFormData.timestamp
     , tags = []
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
        [ input [ type_ "text", placeholder "Total", value model.total, onInput TotalChange ] []
        , textarea [ placeholder "Notes", onInput DescriptionChange ] []
        , span [] [ text <| toString model.timestamp ]
        ]


formData : Model -> ReceiptFormData
formData model =
    { total = Just 1.38
    , description = model.description
    , timestamp = model.timestamp
    , tags = model.tags
    }
