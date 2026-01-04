module TimeHandlingTests exposing (suite)

import Errors exposing (expectError)
import Expect
import Test exposing (Test, describe, test)
import TimeHandling exposing (formatTime, formatTimeWithHours, parseTime)
import Timer exposing (..)


suite : Test
suite =
    describe "TimeHandling tests"
        [ describe "parseTime tests"
            [ test "parseTime of a valid time is successful" <|
                \() ->
                    parseTime "00:04:17"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a string without hours is valid" <|
                \() ->
                    parseTime "04:17"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a valid time with fractional seconds is successful, ignoring fractional seconds" <|
                \() ->
                    parseTime "00:04:17.492"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a string without hours but with fractional sections is successful, ignoring fractional seconds" <|
                \() ->
                    parseTime "04:17.516"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a string with single-digit minutes field is valid" <|
                \() ->
                    parseTime "4:17"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a string with single-digit seconds field is valid" <|
                \() ->
                    parseTime "04:9"
                        |> Expect.equal (Ok (4 * 60 + 9))
            , test "parseTime of a string with leading spaces is valid" <|
                \() ->
                    parseTime "    \t  00:04:17"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a string with trailing spaces is valid" <|
                \() ->
                    parseTime "00:04:17   \t    "
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of an invalid time is an error" <|
                \() ->
                    parseTime "nonsense"
                        |> expectError "UNRECOGNISED_TIME"
            , test "parseTime of a string with a minutes value too large is not valid" <|
                \() ->
                    parseTime "00:60:17"
                        |> expectError "MINUTES_TOO_LARGE"
            , test "parseTime of a string with a seconds value too large is not valid" <|
                \() ->
                    parseTime "00:05:60"
                        |> expectError "SECONDS_TOO_LARGE"
            , test "parseTime of a time with trailing nonsense is not valid" <|
                \() ->
                    parseTime "00:04:17abc"
                        |> expectError "UNRECOGNISED_TIME"
            , test "parseTime of a time with fractional seconds and trailing nonsense is not valid" <|
                \() ->
                    parseTime "00:04:17.446abc"
                        |> expectError "UNRECOGNISED_TIME"
            ]
        , describe "formatTime tests"
            [ test "formatTime of zero is correct" <|
                \() ->
                    formatTime 0
                        |> Expect.equal "00:00"
            , test "formatTime of one second is correct" <|
                \() ->
                    formatTime 1
                        |> Expect.equal "00:01"
            , test "formatTime of nine seconds is correct" <|
                \() ->
                    formatTime 9
                        |> Expect.equal "00:09"
            , test "formatTime of ten seconds is correct" <|
                \() ->
                    formatTime 10
                        |> Expect.equal "00:10"
            , test "formatTime of one minute is correct" <|
                \() ->
                    formatTime 60
                        |> Expect.equal "01:00"
            , test "formatTime of nine minutes is correct" <|
                \() ->
                    formatTime (9 * 60)
                        |> Expect.equal "09:00"
            , test "formatTime of ten minutes is correct" <|
                \() ->
                    formatTime (10 * 60)
                        |> Expect.equal "10:00"
            , test "formatTime of one hour is correct" <|
                \() ->
                    formatTime (60 * 60)
                        |> Expect.equal "01:00:00"
            , test "formatTime of ten hours is correct" <|
                \() ->
                    formatTime (10 * 60 * 60)
                        |> Expect.equal "10:00:00"
            , test "formatTime of some other time is correct" <|
                \() ->
                    formatTime (49 * 60 * 60 + 37 * 60 + 19)
                        |> Expect.equal "49:37:19"
            , test "formatTime of a negative time is correct" <|
                \() ->
                    formatTime -(49 * 60 * 60 + 37 * 60 + 19)
                        |> Expect.equal "-49:37:19"
            ]
        , describe "formatTimeWithHours tests"
            [ test "formatTimeWithHours of zero is correct" <|
                \() ->
                    formatTimeWithHours 0
                        |> Expect.equal "00:00:00"
            , test "formatTimeWithHours of one second is correct" <|
                \() ->
                    formatTimeWithHours 1
                        |> Expect.equal "00:00:01"
            , test "formatTimeWithHours of nine seconds is correct" <|
                \() ->
                    formatTimeWithHours 9
                        |> Expect.equal "00:00:09"
            , test "formatTimeWithHours of ten seconds is correct" <|
                \() ->
                    formatTimeWithHours 10
                        |> Expect.equal "00:00:10"
            , test "formatTimeWithHours of one minute is correct" <|
                \() ->
                    formatTimeWithHours 60
                        |> Expect.equal "00:01:00"
            , test "formatTimeWithHours of nine minutes is correct" <|
                \() ->
                    formatTimeWithHours (9 * 60)
                        |> Expect.equal "00:09:00"
            , test "formatTimeWithHours of ten minutes is correct" <|
                \() ->
                    formatTimeWithHours (10 * 60)
                        |> Expect.equal "00:10:00"
            , test "formatTimeWithHours of one hour is correct" <|
                \() ->
                    formatTimeWithHours (60 * 60)
                        |> Expect.equal "01:00:00"
            , test "formatTimeWithHours of ten hours is correct" <|
                \() ->
                    formatTimeWithHours (10 * 60 * 60)
                        |> Expect.equal "10:00:00"
            , test "formatTimeWithHours of some other time is correct" <|
                \() ->
                    formatTimeWithHours (49 * 60 * 60 + 37 * 60 + 19)
                        |> Expect.equal "49:37:19"
            , test "formatTimeWithHours of a negative time is correct" <|
                \() ->
                    formatTimeWithHours -(49 * 60 * 60 + 37 * 60 + 19)
                        |> Expect.equal "-49:37:19"
            ]
        ]
