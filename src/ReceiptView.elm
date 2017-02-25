module ReceiptView exposing (Model, Msg, init, update, view, updatedReceipt, isReceiptClosed)

import Html exposing (..)
import Html.Attributes exposing (..)
import Api
import Task
import Models exposing (Receipt, ReceiptFile, Authentication, ReceiptFormData)
import MousePosition exposing (Offset, onMouseMove, onMouseDown, onMouseUp)
import Time
import Process
import Ports
import DateTimePicker
import Date exposing (Date)
import FormatUtils
import Material
import Material.Options
import Material.Icon as Icon
import Material.Typography as Typo
import Material.Spinner as Spinner
import Material.Textfield as Textfield
import Material.Options as Options
import Material.Button as Button
import Material.Dialog as Dialog


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
    , selectionBox : Maybe SelectionBox
    , zoomBox : Maybe ZoomBox
    , receiptFormData : ReceiptFormData
    , total : String
    , editMode : EditMode
    , updatingReceipt : Bool
    , dateTimePickerModel : DateTimePicker.Model
    , mdl : Material.Model
    }


init : Authentication -> Receipt -> ( Model, Cmd Msg )
init authentication receipt =
    let
        receiptFormData =
            { total = receipt.total
            , description = receipt.description
            , transactionTime = receipt.transactionTime
            , tags = receipt.tags
            }

        dateTimePickerModel =
            DateTimePicker.init <| Date.fromTime (toFloat receiptFormData.transactionTime)

        model =
            { authentication = authentication
            , receipt = receipt
            , imageUrl = Nothing
            , selectionBox = Nothing
            , zoomBox = Nothing
            , receiptFormData = receiptFormData
            , total = Maybe.withDefault "" <| Maybe.map toString receipt.total
            , editMode = None
            , updatingReceipt = False
            , dateTimePickerModel = dateTimePickerModel
            , mdl = Material.model
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


type EditMode
    = EditTotal
    | EditTransactionTime
    | None


type Msg
    = TotalChange String
    | DescriptionChange String
    | MouseDown Offset
    | MouseMove Offset
    | MouseUp Offset
    | SetImageUrl
    | EditModeSwitch
    | UpdateReceipt
    | CloseReceipt
    | UpdateReceiptResult (Result Api.Error Receipt)
    | DateTimePickerMsg DateTimePicker.Msg
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
            let
                receiptFormData =
                    model.receiptFormData

                updated =
                    { receiptFormData | description = description }
            in
                ( { model
                    | receiptFormData = updated
                  }
                , Cmd.none
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

        EditModeSwitch ->
            ( { model
                | editMode = switchMode model.editMode
              }
            , Cmd.none
            )

        MouseMove offset ->
            let
                selectionBox =
                    Maybe.map (updateSelectionBox offset) model.selectionBox
            in
                ( { model
                    | selectionBox = selectionBox
                  }
                , Cmd.none
                )

        MouseUp _ ->
            let
                cmd =
                    if model.editMode == None then
                        Cmd.none
                    else
                        Ports.showDialog (toString model.editMode)

                editMode =
                    Maybe.withDefault None (Maybe.map toEditMode model.selectionBox)
            in
                ( { model
                    | selectionBox = Nothing
                    , zoomBox = Maybe.map toZoomBox model.selectionBox
                    , editMode = editMode
                  }
                , cmd
                )

        Mdl msg_ ->
            Material.update Mdl msg_ model

        UpdateReceipt ->
            let
                receiptFormData =
                    model.receiptFormData

                updated =
                    { receiptFormData | total = Result.toMaybe <| String.toFloat model.total }
            in
                ( { model | updatingReceipt = True }
                , (Api.updateReceipt
                    model.authentication
                    model.receipt.id
                    updated
                    UpdateReceiptResult
                  )
                )

        UpdateReceiptResult (Ok receipt) ->
            ( { model | updatingReceipt = False }, Cmd.none )

        UpdateReceiptResult (Err error) ->
            ( model, Cmd.none )

        CloseReceipt ->
            ( model, Cmd.none )

        DateTimePickerMsg message ->
            let
                dateTimePickerModel =
                    DateTimePicker.update message model.dateTimePickerModel

                receiptFormData =
                    model.receiptFormData

                updated =
                    { receiptFormData | transactionTime = DateTimePicker.timestamp dateTimePickerModel }
            in
                ( { model
                    | dateTimePickerModel = dateTimePickerModel
                    , receiptFormData = updated
                  }
                , Cmd.none
                )


isReceiptClosed : Msg -> Bool
isReceiptClosed msg =
    case msg of
        CloseReceipt ->
            True

        _ ->
            False


updatedReceipt : Msg -> Maybe Receipt
updatedReceipt msg =
    case msg of
        UpdateReceiptResult (Ok receipt) ->
            Just receipt

        _ ->
            Nothing


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


switchMode : EditMode -> EditMode
switchMode editMode =
    case editMode of
        EditTotal ->
            EditTransactionTime

        EditTransactionTime ->
            EditTotal

        None ->
            None


toEditMode : SelectionBox -> EditMode
toEditMode selectionBox =
    let
        ratio =
            toFloat selectionBox.w / toFloat selectionBox.h
    in
        if ratio > 5.5 then
            EditTransactionTime
        else
            EditTotal


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
    div []
        [ receiptFormView model
        , receiptImageView model
        , dialogView model
        ]



-- Material.Options.css "padding" "0"


dialogTitle : EditMode -> String
dialogTitle editMode =
    case editMode of
        EditTotal ->
            "Total"

        EditTransactionTime ->
            "Transaction Time"

        None ->
            "None"


dialogContent : Model -> Html Msg
dialogContent model =
    let
        maybeZoomedView =
            Maybe.map2 (zoomWindowView model.imageUrl) (toFile model.receipt) model.zoomBox

        zoomedView =
            Maybe.withDefault (div [] []) maybeZoomedView
    in
        case model.editMode of
            EditTotal ->
                div []
                    [ zoomedView
                    , div [ Html.Attributes.class "" ]
                        [ Textfield.render Mdl
                            [ 2 ]
                            model.mdl
                            [ Textfield.label "Total"
                            , Textfield.floatingLabel
                            , Options.css "width" "20%"
                            , Textfield.value model.total
                            , Options.onInput TotalChange
                            ]
                            []
                        ]
                    ]

            EditTransactionTime ->
                div []
                    [ zoomedView
                    , Html.map DateTimePickerMsg (DateTimePicker.view model.dateTimePickerModel)
                    ]

            None ->
                div [] []


dialogView : Model -> Html Msg
dialogView model =
    let
        dialogWidth =
            if model.editMode == EditTransactionTime then
                "700px"
            else
                "500px"
    in
        Dialog.view
            [ Material.Options.css "width" dialogWidth ]
            [ Dialog.title [] [ text <| dialogTitle model.editMode ]
            , Dialog.content []
                [ dialogContent model
                ]
            , Dialog.actions []
                [ Button.render Mdl
                    [ 0 ]
                    model.mdl
                    [ Dialog.closeOn "click" ]
                    [ text "Close" ]
                , Button.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Options.onClick EditModeSwitch ]
                    [ text "Switch mode" ]
                ]
            ]


receiptFormView : Model -> Html Msg
receiptFormView model =
    let
        formattedDate =
            FormatUtils.formatTimestamp model.receiptFormData.transactionTime

        updateButton =
            case model.updatingReceipt of
                False ->
                    Button.render Mdl
                        [ 4 ]
                        model.mdl
                        [ Button.raised
                        , Button.colored
                        , Button.ripple
                        , Options.css "float" "right"
                        , Options.onClick UpdateReceipt
                        ]
                        [ text "Save receipt" ]

                True ->
                    div [ Html.Attributes.class "receipt-update-spinner" ]
                        [ Spinner.spinner
                            [ Spinner.active True
                            , Spinner.singleColor True
                            ]
                        ]
    in
        div [ Html.Attributes.class "receipt-form" ]
            [ div [ Html.Attributes.class "price-wrapper" ]
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
                , div [ Html.Attributes.class "receipt-close-button" ]
                    [ Button.render Mdl
                        [ 5 ]
                        model.mdl
                        [ Button.minifab
                        , Button.ripple
                        , Options.onClick CloseReceipt
                        ]
                        [ Icon.i "close" ]
                    ]
                ]
            , div []
                [ Textfield.render Mdl
                    [ 1 ]
                    model.mdl
                    [ Textfield.label "Notes"
                    , Textfield.floatingLabel
                    , Options.css "width" "100%"
                    , Textfield.textarea
                    , Textfield.rows 3
                    , Options.onInput DescriptionChange
                    , Textfield.value model.receiptFormData.description
                    ]
                    []
                ]
            , Options.styled p
                [ Typo.subhead, Options.css "display" "inline-block" ]
                [ text formattedDate ]
            , updateButton
            ]


receiptImageView : Model -> Html Msg
receiptImageView model =
    let
        imageUrl =
            Maybe.withDefault "" model.imageUrl

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
