module EventDateAndTimeEditing exposing (handleEventDateChange, handleEventTimeChange)

import DateHandling exposing (dateStringToPosix)
import EventDateAndTime exposing (EventDateAndTime)
import Model exposing (Model)
import Time exposing (Posix)
import TimeHandling exposing (parseHoursAndMinutes)


handleEventDateChange : String -> Model -> Model
handleEventDateChange newEventDate model =
    let
        newParsedDate : Maybe Posix
        newParsedDate =
            newEventDate
                ++ " 00:00:00"
                |> dateStringToPosix

        oldEventDateAndTime : EventDateAndTime
        oldEventDateAndTime =
            model.eventDateAndTime

        newEventDateAndTime : EventDateAndTime
        newEventDateAndTime =
            { oldEventDateAndTime
                | enteredDate = newEventDate
                , validatedDate = newParsedDate
            }
    in
    { model | eventDateAndTime = newEventDateAndTime }


handleEventTimeChange : String -> Model -> Model
handleEventTimeChange newEventTime model =
    let
        newParsedTime : Maybe Int
        newParsedTime =
            parseHoursAndMinutes newEventTime
                |> Result.toMaybe

        oldEventDateAndTime : EventDateAndTime
        oldEventDateAndTime =
            model.eventDateAndTime

        newEventDateAndTime : EventDateAndTime
        newEventDateAndTime =
            { oldEventDateAndTime
                | enteredTime = newEventTime
                , validatedTime = newParsedTime
            }
    in
    { model | eventDateAndTime = newEventDateAndTime }
