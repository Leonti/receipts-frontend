module DateTimePicker exposing (Model, Msg, init, update, view, timestamp)

import Html exposing (..)
import Html.Attributes exposing (..)
import DatePicker
import TimePicker
import Date exposing (Date, toTime)


type alias Model =
    { datePickerModel : DatePicker.Model
    , timePickerModel : TimePicker.Model
    }


init : Date -> Model
init dateTime =
    let
        timePickerModel =
            TimePicker.init
                { is24Hours = True
                , mainColor = "#00bcd4"
                }
                dateTime

        datePickerModel =
            DatePicker.init dateTime "#00bcd4"
    in
        { timePickerModel = timePickerModel
        , datePickerModel = datePickerModel
        }


type Msg
    = TimePickerMsg TimePicker.Msg
    | DatePickerMsg DatePicker.Msg


updateDatePickerModel : DatePicker.Model -> Date -> DatePicker.Model
updateDatePickerModel model date =
    { model | date = date }


updateTimePickerModel : TimePicker.Model -> Date -> TimePicker.Model
updateTimePickerModel model date =
    { model | date = date }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TimePickerMsg message ->
            let
                timePickerModel =
                    TimePicker.update message model.timePickerModel
            in
                { model
                    | timePickerModel = timePickerModel
                    , datePickerModel =
                        updateDatePickerModel model.datePickerModel
                            (TimePicker.selectedTime timePickerModel)
                }

        DatePickerMsg message ->
            let
                datePickerModel =
                    DatePicker.update message model.datePickerModel
            in
                { model
                    | datePickerModel = datePickerModel
                    , timePickerModel =
                        updateTimePickerModel model.timePickerModel
                            (DatePicker.selectedDate datePickerModel)
                }


timestamp : Model -> Int
timestamp model =
    round <| Date.toTime (TimePicker.selectedTime model.timePickerModel)


view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "date-time-picker" ]
        [ Html.map DatePickerMsg (DatePicker.view model.datePickerModel)
        , Html.map TimePickerMsg (TimePicker.view model.timePickerModel)
        ]
