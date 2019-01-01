module EventDateAndTimeView exposing (eventDateAndTimeView)

import DataStructures exposing (EventDateAndTime)
import Html exposing (Html, button, div, h3, input, label, text)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Msg exposing (Msg(..))


defaultEventTimes : List String
defaultEventTimes =
    [ "09:00", "09:30", "10:00", "10:30", "11:00" ]


eventTimeButton : String -> Html Msg
eventTimeButton time =
    button
        [ type_ "button"
        , class "btn btn-primary btn-xs event-time-button"
        , onClick (EventTimeChanged time)
        ]
        [ text time ]


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
        [ class "form-horizontal" ]
        [ h3 []
            [ text "Date/time" ]
        , div [ class "form-group" ]
            [ label [ for "eventDateBox", class "col-xs-1 control-label" ] [ text "Date: " ]
            , div [ class "col-xs-11" ]
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
        , div [ class "form-group" ]
            [ label [ for "eventTimeBox", class "col-xs-1 control-label" ] [ text "Time: " ]
            , div [ class "col-xs-11" ]
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
