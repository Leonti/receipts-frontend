module ReceiptList exposing (Model, Msg, emptyModel, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ports

import Api
import Models exposing (UserInfo, Receipt)

type alias Model =
  { userId : String
  , token : String
  , receipts : List Receipt
  }


init : String -> String -> (Model, Cmd Msg)
init userId token =
    let model =
        { userId = userId
        , token = token
        , receipts = []
        }
    in
        update Fetch model
        
emptyModel : Model
emptyModel =
    { userId = ""
    , token = ""
    , receipts = []
    }

type Msg
    = Fetch
    | FetchSucceed (List Receipt)
    | FetchFail Api.Error
    | LoadImage Ports.LoadImageParams
    | LoadImageSucceed Ports.LoadImageResult

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fetch ->
        (model, Api.fetchReceipts model.token model.userId FetchFail FetchSucceed)

    FetchSucceed receipts ->
        ({ model | receipts = List.take 10 receipts }, Cmd.none)

    FetchFail error ->
        (model, Cmd.none)

    LoadImage loadImageParams ->
        (model, Ports.loadImage loadImageParams)

    LoadImageSucceed loadImageResult ->
        (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Ports.imageLoaded LoadImageSucceed

view : Model -> Html Msg
view model =
  div []
        [ div []
            [text "Receipts:"]
        , div [] (List.map (\receipt -> receiptView model.userId model.token receipt)  model.receipts)
        , img [Html.Attributes.id "image"] []
        ]

receiptView : String -> String -> Receipt -> Html Msg
receiptView userId authToken receipt =
    div []
        [ div []
            [ text receipt.id ]
        , div [] (List.map (\file -> div []
            [text ""
            , button [ onClick <| LoadImage <| Ports.LoadImageParams (Api.baseUrl ++ "/user/" ++ userId ++ "/receipt/" ++ receipt.id ++ "/file/" ++ file.id ++ "." ++ file.ext) authToken file.id]
                [ text "Load image" ]
            ]
            ) receipt.files)
        ]
