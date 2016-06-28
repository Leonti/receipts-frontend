module ReceiptView exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ports

import Api
import Models exposing (Receipt, ReceiptFile)

type alias Model =
  { userId : String
  , token : String
  , receipt : Receipt
  }

init : String -> String -> Receipt -> (Model, Cmd Msg)
init userId token receipt =
    let model =
        { userId = userId
        , token = token
        , receipt = receipt
        }
    in
        case toImageParams model of
            Just imageParams -> update (LoadImage imageParams) model
            Nothing -> (model ! [])

toImageParams : Model -> Maybe Ports.LoadImageParams
toImageParams model =
    Maybe.map (\file ->
            Ports.LoadImageParams (Api.baseUrl ++ "/user/" ++ model.userId ++ "/receipt/" ++ model.receipt.id ++ "/file/" ++ file.id ++ "." ++ file.ext) model.token file.id
        ) <| toFile model.receipt

toFile : Receipt -> Maybe ReceiptFile
toFile receipt =
    case receipt.files of
        [original] -> Just original
        [original, resized] -> Just resized
        original::resized::_ -> Just resized
        [] -> Nothing

type Msg
    = LoadImage Ports.LoadImageParams
    | LoadImageSucceed Ports.LoadImageResult

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LoadImage loadImageParams ->
        (model, Ports.loadImage loadImageParams)

    LoadImageSucceed loadImageResult ->
        (model, Cmd.none)

subscriptions : Sub Msg
subscriptions =
  Ports.imageLoaded LoadImageSucceed

view : Model -> Html Msg
view model =
    div []
      [ div []
          [text "Receipt view:"]
      , div [] [text model.receipt.id]
      , div [] (List.map (\file -> div []
          [text ""
          , button [ onClick <| LoadImage <| Ports.LoadImageParams (Api.baseUrl ++ "/user/" ++ model.userId ++ "/receipt/" ++ model.receipt.id ++ "/file/" ++ file.id ++ "." ++ file.ext) model.token file.id]
              [ text "Load image" ]
          ]
          ) model.receipt.files)
      , img [Html.Attributes.id "image"] []
      ]
