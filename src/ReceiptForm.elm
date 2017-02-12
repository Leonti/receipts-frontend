module ReceiptForm exposing (Model, Msg, init, update, view, formData)

import Html exposing (..)
import Models exposing (ReceiptFormData)
import Material.Textfield as Textfield
import Material
import Material.Options as Options


type alias Model =
    { total : String
    , description : String
    , timestamp : Int
    , tags : List String
    , mdl : Material.Model
    }


init : ReceiptFormData -> ( Model, Cmd Msg )
init receiptFormData =
    ({ total = Maybe.withDefault "" <| Maybe.map toString receiptFormData.total
     , description = receiptFormData.description
     , timestamp = receiptFormData.timestamp
     , tags = receiptFormData.tags
     , mdl = Material.model
     }
        ! []
    )


type Msg
    = TotalChange String
    | DescriptionChange String
    | Mdl (Material.Msg Msg)


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

        Mdl msg_ ->
            Material.update Mdl msg_ model


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ Textfield.render Mdl
                [ 0 ]
                model.mdl
                [ Textfield.label "Total"
                , Textfield.floatingLabel
                , Options.css "width" "20%"
                , Textfield.value model.total
                , Options.onInput TotalChange
                ]
                []
            ]
        , div []
            [ Textfield.render Mdl
                [ 1 ]
                model.mdl
                [ Textfield.label "Notes"
                , Textfield.floatingLabel
                , Options.css "width" "100%"
                , Textfield.textarea
                , Textfield.rows 6
                , Options.onInput DescriptionChange
                , Textfield.value model.description
                ]
                []
            ]
        , span [] [ text <| toString model.timestamp ]
        ]


formData : Model -> ReceiptFormData
formData model =
    { total = Just 1.38
    , description = model.description
    , timestamp = model.timestamp
    , tags = model.tags
    }
