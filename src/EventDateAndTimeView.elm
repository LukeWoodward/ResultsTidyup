module EventDateAndTimeView exposing (eventDateAndTimeView)

import Html exposing (Html, div, h3, input, label, text)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onInput)
import Model exposing (EventDateAndTime)
import Msg exposing (Msg(..))


eventDateAndTimeView : EventDateAndTime -> Html Msg
eventDateAndTimeView eventDateAndTime =
    let
        dateInputClass : String
        dateInputClass =
            if eventDateAndTime.enteredDate /= "" && eventDateAndTime.validatedDate == Nothing then
                "date-error"

            else
                ""
    in
    div
        []
        [ h3 [] [ text "Date/time" ]
        , div
            []
            [ label [ for "eventDateBox" ] [ text "Date: " ]
            , input
                [ type_ "text"
                , id "eventDateBox"
                , class dateInputClass
                , onInput EventDateChanged
                , value eventDateAndTime.enteredDate
                ]
                []
            ]
        ]
