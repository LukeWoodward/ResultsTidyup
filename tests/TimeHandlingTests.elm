module TimeHandlingTests exposing (suite)

import Expect
import Test exposing (describe, test, Test)
import Stopwatch exposing (..)
import Errors exposing (expectError)
import TimeHandling exposing (parseTime)


suite : Test
suite =
    describe "TimeHandling tests"
        [ describe "parseTime tests"
            [ test "parseTime of a valid time is successful" <|
                \() ->
                    parseTime "00:04:17"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a string without hours is a valid list of results" <|
                \() ->
                    parseTime "04:17"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a string with single-digit minutes field is a valid list of results" <|
                \() ->
                    parseTime "4:17"
                        |> Expect.equal (Ok (4 * 60 + 17))
            , test "parseTime of a string with single-digit seconds field is a valid list of results" <|
                \() ->
                    parseTime "04:9"
                        |> Expect.equal (Ok (4 * 60 + 9))
            , test "parseTime of an invalid time is an error" <|
                \() ->
                    parseTime "nonsense"
                        |> expectError "UNRECOGNISED_TIME"
            , test "parseTime of a string with a minutes value too large is not a valid list of results" <|
                \() ->
                    parseTime "00:60:17"
                        |> expectError "MINUTES_TOO_LARGE"
            , test "parseTime of a string with a seconds value too large is not a valid list of results" <|
                \() ->
                    parseTime "00:05:60"
                        |> expectError "SECONDS_TOO_LARGE"
            ]
        ]
