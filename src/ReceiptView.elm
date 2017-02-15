module ReceiptView exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Api
import Task
import Models exposing (Receipt, ReceiptFile, Authentication)
import ReceiptForm
import Material.Spinner as Spinner
import MousePosition exposing (Offset, onMouseMove, onMouseDown, onMouseUp)
import Time
import Process


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
    , imageUrl : Maybe String
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
            , imageUrl = Nothing
            , receiptFormModel = receiptFormModel
            , selectionBox = Nothing
            , zoomBox = Nothing
            }
    in
        ( model, delay 10 SetImageUrl )


toImageUrl : Authentication -> Receipt -> Maybe String
toImageUrl authentication receipt =
    Maybe.map
        (\file -> Api.receiptFileUrl authentication.userId receipt.id file.id file.ext)
    <|
        toFile receipt


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
    = ReceiptFormMsg ReceiptForm.Msg
    | MouseDown Offset
    | MouseMove Offset
    | MouseUp Offset
    | SetImageUrl


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        SetImageUrl ->
            ( { model
                | imageUrl = toImageUrl model.authentication model.receipt
              }
            , Cmd.none
            )

        MouseMove offset ->
            ( { model
                | selectionBox = Maybe.map (updateSelectionBox offset) model.selectionBox
              }
            , Cmd.none
            )

        MouseUp _ ->
            ( { model
                | selectionBox = Nothing
                , zoomBox = Debug.log "zoombox" <| Maybe.map toZoomBox model.selectionBox
              }
            , Cmd.none
            )


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


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


view : Model -> Html Msg
view model =
    let
        maybeZoomedView =
            Maybe.map2 (zoomWindowView model.imageUrl) (toFile model.receipt) model.zoomBox

        zoomedView =
            Maybe.withDefault (div [] []) maybeZoomedView
    in
        div []
            [ Html.map ReceiptFormMsg (ReceiptForm.view model.receiptFormModel)
            , zoomedView
            , receiptImageView model
            ]


receiptImageView : Model -> Html Msg
receiptImageView model =
    let
        imageUrl =
            Debug.log "url" <| Maybe.withDefault "" model.imageUrl

        imageStyle =
            [ ( "width", "100%" ) ]

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
            [ div [ Html.Attributes.class "loading-spinner" ]
                [ Spinner.spinner
                    [ Spinner.active True
                    , Spinner.singleColor True
                    ]
                ]
            , img
                [ Html.Attributes.src imageUrl
                , Html.Attributes.style imageStyle
                ]
                []
            , div [ Html.Attributes.class "selection-area" ] []
            , div
                [ Html.Attributes.class "region-selector"
                , Html.Attributes.style selectorStyle
                ]
                []
            ]


zoomWindowView : Maybe String -> ReceiptFile -> ZoomBox -> Html Msg
zoomWindowView maybeImageUrl receiptFile zoomBox =
    let
        imageUrl =
            Maybe.withDefault "" maybeImageUrl

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
                , Html.Attributes.src imageUrl
                , Html.Attributes.style imageStyle
                ]
                []
            ]
