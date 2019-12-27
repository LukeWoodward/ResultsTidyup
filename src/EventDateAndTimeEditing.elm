module EventDateAndTimeEditing exposing (handleEventDateChange, handleEventTimeChange)

import DataEntry exposing (DateEntry, IntegerEntry, integerEntryFromHoursAndMinutes)
import DateHandling exposing (dateTimeStringToPosix)
import EventDateAndTime exposing (EventDateAndTime)
import Model exposing (Model)
import Time exposing (Posix)


handleEventDateChange : String -> Model -> Model
handleEventDateChange newEventDate model =
    let
        newParsedDate : Maybe Posix
        newParsedDate =
            newEventDate
                ++ " 00:00:00"
                |> dateTimeStringToPosix

        oldEventDateAndTime : EventDateAndTime
        oldEventDateAndTime =
            model.eventDateAndTime

        newEventDateAndTime : EventDateAndTime
        newEventDateAndTime =
            { oldEventDateAndTime
                | date = DateEntry newEventDate newParsedDate
            }
    in
    { model | eventDateAndTime = newEventDateAndTime }


handleEventTimeChange : String -> Model -> Model
handleEventTimeChange newEventTime model =
    let
        oldEventDateAndTime : EventDateAndTime
        oldEventDateAndTime =
            model.eventDateAndTime

        newEventDateAndTime : EventDateAndTime
        newEventDateAndTime =
            { oldEventDateAndTime
                | time = integerEntryFromHoursAndMinutes newEventTime
            }
    in
    { model | eventDateAndTime = newEventDateAndTime }
