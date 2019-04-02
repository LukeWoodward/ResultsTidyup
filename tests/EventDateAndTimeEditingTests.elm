module EventDateAndTimeEditingTests exposing (suite)

import BarcodeScannerTests exposing (toPosix)
import DataStructures exposing (EventDateAndTime)
import EventDateAndTimeEditing exposing (handleEventDateChange, handleEventTimeChange)
import Expect
import Model exposing (initModel)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "EventDateAndTimeEditing tests"
        [ describe "handleEventDateChange tests"
            [ test "Setting a valid date sets the validated date" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime "26/05/2018" (toPosix "2018-05-26T00:00:00.000Z") "" Nothing }
                        (handleEventDateChange "26/05/2018" initModel)
            , test "Setting a nonexistent date clears the validated date" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime "29/02/2018" Nothing "" Nothing }
                        (handleEventDateChange "29/02/2018" initModel)
            , test "Setting an invalid date clears the validated date" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime "This is not a valid date" Nothing "" Nothing }
                        (handleEventDateChange "This is not a valid date" initModel)
            , test "Setting an empty date clears the validated date" <|
                \() ->
                    Expect.equal initModel (handleEventDateChange "" initModel)
            ]
        , describe "Event time changed tests"
            [ test "Setting a valid time sets the validated time and issues a command" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime "" Nothing "09:30" (Just (9 * 60 + 30)) }
                        (handleEventTimeChange "09:30" initModel)
            , test "Setting a nonexistent time clears the validated time" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime "" Nothing "25:30" Nothing }
                        (handleEventTimeChange "25:30" initModel)
            , test "Setting an invalid time clears the validated time" <|
                \() ->
                    Expect.equal
                        { initModel | eventDateAndTime = EventDateAndTime "" Nothing "This is not a valid time" Nothing }
                        (handleEventTimeChange "This is not a valid time" initModel)
            , test "Setting an empty time clears the validated time" <|
                \() ->
                    Expect.equal
                        initModel
                        (handleEventTimeChange "" initModel)
            ]
        ]
