module EventDateAndTimeView exposing (eventDateAndTimeView)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import EventDateAndTime exposing (EventDateAndTime)
import Html exposing (Html, button, div, h3, input, label, text)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Msg exposing (Msg(..))
import ViewCommon exposing (smallButton)


defaultEventTimes : List String
defaultEventTimes =
    [ "07:00", "08:00", "09:00", "09:30", "10:00", "10:30", "11:00" ]


eventTimeButton : String -> Html Msg
eventTimeButton time =
    smallButton (EventTimeChanged time) [ class "event-time-button" ] time


eventDateAndTimeView : EventDateAndTime -> Html Msg
eventDateAndTimeView eventDateAndTime =
    let
        dateInputClass : String
        dateInputClass =
            if eventDateAndTime.enteredDate /= "" && eventDateAndTime.validatedDate == Nothing then
                "date-error form-control"

            else
                "form-control"

        timeInputClass : String
        timeInputClass =
            if eventDateAndTime.enteredTime /= "" && eventDateAndTime.validatedTime == Nothing then
                "time-error form-control"

            else
                "form-control"
    in
    div
        []
        [ h3 []
            [ text "Date/time" ]
        , Grid.row [ Row.attrs [ class "col-form-label" ] ]
            [ Grid.col
                [ Col.xs1 ]
                [ label [ for "eventDateBox", class "col-form-label" ] [ text "Date: " ] ]
            , Grid.col
                [ Col.xs5 ]
                [ input
                    [ type_ "text"
                    , id "eventDateBox"
                    , class dateInputClass
                    , onInput EventDateChanged
                    , value eventDateAndTime.enteredDate
                    ]
                    []
                ]
            ]
        , Grid.row [ Row.attrs [ class "col-form-label" ] ]
            [ Grid.col
                [ Col.xs1 ]
                [ label [ for "eventTimeBox", class "col-form-label" ] [ text "Time: " ] ]
            , Grid.col
                [ Col.xs11 ]
                (input
                    [ type_ "text"
                    , id "eventTimeBox"
                    , class timeInputClass
                    , onInput EventTimeChanged
                    , value eventDateAndTime.enteredTime
                    ]
                    []
                    :: List.map eventTimeButton defaultEventTimes
                )
            ]
        ]
