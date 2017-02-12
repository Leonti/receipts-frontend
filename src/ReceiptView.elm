module ReceiptView exposing (Model, Msg, init, update, view, subscriptions)

import Html exposing (..)
import Html.Attributes exposing (..)
import Ports
import Api
import Models exposing (Receipt, ReceiptFile, Authentication)
import ReceiptForm
import Material.Spinner as Spinner
import MousePosition exposing (Offset, onMouseMove, onMouseDown, onMouseUp)


type alias SelectionBox =
    { deltaX : Int
    , deltaY : Int
    , x : Int
    , y : Int
    , w : Int
    , h : Int
    , scaling : Float
    }


type alias ZoomBox =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }


type alias Model =
    { authentication : Authentication
    , receipt : Receipt
    , loadingImage : Bool
    , imageData : String
    , receiptFormModel : ReceiptForm.Model
    , selectionBox : Maybe SelectionBox
    , zoomBox : Maybe ZoomBox
    }


init : Authentication -> Receipt -> ( Model, Cmd Msg )
init authentication receipt =
    let
        ( receiptFormModel, receiptFormCmd ) =
            ReceiptForm.init
                { total = receipt.total
                , description = receipt.description
                , timestamp = receipt.timestamp
                , tags = receipt.tags
                }

        model =
            { authentication = authentication
            , receipt = receipt
            , loadingImage = False
            , imageData = ""
            , receiptFormModel = receiptFormModel
            , selectionBox = Nothing
            , zoomBox = Nothing
            }
    in
        case toImageParams model of
            Just imageParams ->
                update (LoadImage imageParams) model

            Nothing ->
                (model ! [])


toImageParams : Model -> Maybe Ports.LoadImageParams
toImageParams model =
    Maybe.map
        (\file ->
            { url = Api.receiptFileUrl model.authentication.userId model.receipt.id file.id file.ext
            , authToken = model.authentication.token
            , fileId = file.id
            }
        )
    <|
        toFile model.receipt


toFile : Receipt -> Maybe ReceiptFile
toFile receipt =
    case receipt.files of
        [ original ] ->
            Just original

        [ original, resized ] ->
            Just resized

        original :: resized :: _ ->
            Just resized

        [] ->
            Nothing


type Msg
    = LoadImage Ports.LoadImageParams
    | LoadImageSucceed Ports.LoadImageResult
    | ReceiptFormMsg ReceiptForm.Msg
    | MouseDown Offset
    | MouseMove Offset
    | MouseUp Offset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadImage loadImageParams ->
            ( { model
                | loadingImage = True
              }
            , Ports.loadImage loadImageParams
            )

        LoadImageSucceed loadImageResult ->
            ( { model
                | loadingImage = False
                , imageData = loadImageResult.imageData
              }
            , Cmd.none
            )

        ReceiptFormMsg message ->
            let
                ( receiptFormModel, receiptFormCmd ) =
                    ReceiptForm.update message model.receiptFormModel
            in
                ( { model
                    | receiptFormModel = receiptFormModel
                  }
                , Cmd.map ReceiptFormMsg receiptFormCmd
                )

        MouseDown offset ->
            let
                selectionBox =
                    Maybe.map (initialScalingBox offset) <| toFile model.receipt
            in
                ( { model | selectionBox = selectionBox }, Cmd.none )

        MouseMove offset ->
            let
                selectionBox =
                    Maybe.map (updateSelectionBox offset) model.selectionBox
            in
                ( { model | selectionBox = selectionBox }, Cmd.none )

        MouseUp _ ->
            ( { model
                | selectionBox = Nothing
                , zoomBox = Maybe.map toZoomBox model.selectionBox
              }
            , Cmd.none
            )


toZoomBox : SelectionBox -> ZoomBox
toZoomBox selectionBox =
    { x = round <| toFloat selectionBox.x / selectionBox.scaling
    , y = round <| toFloat selectionBox.y / selectionBox.scaling
    , w = round <| toFloat selectionBox.w / selectionBox.scaling
    , h = round <| toFloat selectionBox.h / selectionBox.scaling
    }


initialScalingBox : Offset -> ReceiptFile -> SelectionBox
initialScalingBox offset receiptFile =
    { deltaX = offset.pageX - offset.offsetX
    , deltaY = offset.pageY - offset.offsetY
    , x = offset.offsetX
    , y = offset.offsetY
    , w = 0
    , h = 0
    , scaling = (toFloat offset.target.offsetHeight / toFloat receiptFile.metaData.height)
    }


updateSelectionBox : Offset -> SelectionBox -> SelectionBox
updateSelectionBox offset selectionBox =
    let
        x =
            offset.pageX - selectionBox.deltaX

        y =
            offset.pageY - selectionBox.deltaY
    in
        { selectionBox
            | w = (x - selectionBox.x)
            , h = (y - selectionBox.y)
        }


subscriptions : Sub Msg
subscriptions =
    Ports.imageLoaded LoadImageSucceed


view : Model -> Html Msg
view model =
    let
        maybeZoomedView =
            Maybe.map2 (zoomWindowView model.imageData) (toFile model.receipt) model.zoomBox

        zoomedView =
            Maybe.withDefault (div [] []) maybeZoomedView
    in
        div []
            [ div []
                [ text "Receipt view:" ]
            , div [] [ text model.receipt.id ]
            , Spinner.spinner
                [ Spinner.active model.loadingImage
                , Spinner.singleColor True
                ]
            , Html.map ReceiptFormMsg (ReceiptForm.view model.receiptFormModel)
            , zoomedView
            , receiptImageView model
            ]


receiptImageView : Model -> Html Msg
receiptImageView model =
    let
        imageDataUrl =
            "data:image/jpeg;base64," ++ model.imageData

        imageBaseStyle =
            [ ( "width", "100%" ) ]

        imageStyle =
            if model.loadingImage then
                imageBaseStyle ++ [ ( "display", "none" ) ]
            else
                imageBaseStyle

        selectorStyle =
            case model.selectionBox of
                Just selectionBox ->
                    [ ( "top", (toString selectionBox.y) ++ "px" )
                    , ( "left", (toString selectionBox.x) ++ "px" )
                    , ( "width", (toString selectionBox.w) ++ "px" )
                    , ( "height", (toString selectionBox.h) ++ "px" )
                    ]

                Nothing ->
                    [ ( "display", "none" ) ]
    in
        div
            [ Html.Attributes.class "receipt-image-wrapper"
            , onMouseMove MouseMove
            , onMouseDown MouseDown
            , onMouseUp MouseUp
            ]
            [ img
                [ Html.Attributes.src imageDataUrl
                , Html.Attributes.style imageStyle
                ]
                []
            , div
                [ Html.Attributes.class "region-selector"
                , Html.Attributes.style selectorStyle
                ]
                []
            ]


zoomWindowView : String -> ReceiptFile -> ZoomBox -> Html Msg
zoomWindowView imageData receiptFile zoomBox =
    let
        imageDataUrl =
            "data:image/jpeg;base64," ++ imageData

        ratio =
            toFloat zoomBox.w / toFloat zoomBox.h

        w =
            300

        h =
            300 / ratio

        imageScale =
            toFloat w / toFloat zoomBox.w

        imageWidth =
            toFloat receiptFile.metaData.width * imageScale

        imageHeight =
            toFloat receiptFile.metaData.height * imageScale

        imageTop =
            -(toFloat zoomBox.y * imageScale)

        imageLeft =
            -(toFloat zoomBox.x * imageScale)

        zoomWindowStyle =
            [ ( "width", toString w ++ "px" )
            , ( "height", toString h ++ "px" )
            ]

        imageStyle =
            [ ( "top", toString imageTop ++ "px" )
            , ( "left", toString imageLeft ++ "px" )
            , ( "width", toString imageWidth ++ "px" )
            , ( "height", toString imageHeight ++ "px" )
            ]
    in
        div
            [ Html.Attributes.class "zoom-window"
            , Html.Attributes.style zoomWindowStyle
            ]
            [ img
                [ Html.Attributes.class "zoomed-image"
                , Html.Attributes.src imageDataUrl
                , Html.Attributes.style imageStyle
                ]
                []
            ]
