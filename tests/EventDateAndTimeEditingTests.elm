module EventDateAndTimeEditingTests exposing (suite)

import DataEntry exposing (DateEntry, IntegerEntry, emptyEntry)
import EventDateAndTime exposing (EventDateAndTime)
import EventDateAndTimeEditing exposing (handleEventDateChange, handleEventTimeChange)
import Expect
import Model exposing (initModel)
import Test exposing (Test, describe, test)
import TestData exposing (toPosix)


suite : Test
suite =
    describe "EventDateAndTimeEditing tests"
        [ describe "handleEventDateChange tests"
            [ test "Setting a valid date sets the validated date" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime (DateEntry "26/05/2018" (toPosix "2018-05-26T00:00:00.000Z")) emptyEntry }
                        (handleEventDateChange "26/05/2018" initModel)
            , test "Setting a nonexistent date clears the validated date" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime (DateEntry "29/02/2018" Nothing) emptyEntry }
                        (handleEventDateChange "29/02/2018" initModel)
            , test "Setting an invalid date clears the validated date" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime (DateEntry "This is not a valid date" Nothing) emptyEntry }
                        (handleEventDateChange "This is not a valid date" initModel)
            , test "Setting an empty date clears the validated date" <|
                \() ->
                    Expect.equal initModel (handleEventDateChange "" initModel)
            ]
        , describe "Event time changed tests"
            [ test "Setting a valid time sets the validated time and issues a command" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime emptyEntry (IntegerEntry "09:30" (Just (9 * 60 + 30))) }
                        (handleEventTimeChange "09:30" initModel)
            , test "Setting a nonexistent time clears the validated time" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime emptyEntry (IntegerEntry "25:30" Nothing) }
                        (handleEventTimeChange "25:30" initModel)
            , test "Setting an invalid time clears the validated time" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime emptyEntry (IntegerEntry "This is not a valid time" Nothing) }
                        (handleEventTimeChange "This is not a valid time" initModel)
            , test "Setting an empty time clears the validated time" <|
                \() ->
                    Expect.equal
                        initModel
                        (handleEventTimeChange "" initModel)
            ]
        ]
