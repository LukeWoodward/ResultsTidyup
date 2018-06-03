module StopwatchTests exposing (suite)

import Expect
import Test exposing (describe, test, Test)
import Stopwatch exposing (..)
import Errors exposing (expectError)
import String.Extra


sampleData : String
sampleData =
    "STARTOFEVENT,01/01/2001 00:00:00,abcdefghij\n"
        ++ "0,01/01/2001 00:00:00\n"
        ++ "1,01/01/2001 00:03:11,00:03:11\n"
        ++ "2,01/01/2001 00:07:44,00:07:44\n"
        ++ "3,01/01/2001 00:10:03,00:10:03\n"
        ++ "ENDOFEVENT,01/01/2001 00:15:55\n"


expectedParsedSampleData : Stopwatch
expectedParsedSampleData =
    StopwatchData [ 3 * 60 + 11, 7 * 60 + 44, 10 * 60 + 3 ]


suite : Test
suite =
    describe "Stopwatch tests"
        [ describe "readStopwatchData tests"
            [ test "readStopwatchData of a valid single-line string is a valid singleton list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 04:17,04:17"
                        |> Expect.equal (Ok (StopwatchData [ 4 * 60 + 17 ]))
            , test "readStopwatchData of a valid multi-line string is a valid list of results" <|
                \() ->
                    readStopwatchData sampleData
                        |> Expect.equal (Ok expectedParsedSampleData)
            , test "readStopwatchData of a valid multi-line string with CRLF line-endings is a valid list of results" <|
                \() ->
                    readStopwatchData (String.Extra.replace "\n" "\x0D\n" sampleData)
                        |> Expect.equal (Ok expectedParsedSampleData)
            , test "readStopwatchData of a valid multi-line string with CR line-endings is a valid list of results" <|
                \() ->
                    readStopwatchData (String.Extra.replace "\n" "\x0D" sampleData)
                        |> Expect.equal (Ok expectedParsedSampleData)
            , test "readStopwatchData of a valid multi-line string with blank lines is a valid list of results" <|
                \() ->
                    readStopwatchData (String.Extra.replace "\n" "\n\n" sampleData)
                        |> Expect.equal (Ok expectedParsedSampleData)
            , test "readStopwatchData of a string without hours is a valid list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 04:17,04:17"
                        |> Expect.equal (Ok (StopwatchData [ 4 * 60 + 17 ]))
            , test "readStopwatchData of a string with single-digit minutes field is a valid list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 4:17,4:17"
                        |> Expect.equal (Ok (StopwatchData [ 4 * 60 + 17 ]))
            , test "readStopwatchData of a string with single-digit seconds field is a valid list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 04:9,04:9"
                        |> Expect.equal (Ok (StopwatchData [ 4 * 60 + 9 ]))
            , test "readStopwatchData of an empty string is not a valid list of results" <|
                \() ->
                    readStopwatchData ""
                        |> expectError "NO_RESULTS"
            , test "readStopwatchData of a string with too many parts is not a valid list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 04:17,04:17,some extra nonsense"
                        |> expectError "NOT_THREE_PARTS"
            , test "readStopwatchData of a string with too few parts is not a valid list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 04:17"
                        |> expectError "NOT_THREE_PARTS"
            , test "readStopwatchData of a string with an invalid time is not a valid list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 04:17,nonsense"
                        |> expectError "UNRECOGNISED_TIME"
            , test "readStopwatchData of a string with a minutes value too large is not a valid list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 00:60:17,00:60:17"
                        |> expectError "MINUTES_TOO_LARGE"
            , test "readStopwatchData of a string with a seconds value too large is not a valid list of results" <|
                \() ->
                    readStopwatchData "1,01/01/2001 00:05:60,00:05:60"
                        |> expectError "SECONDS_TOO_LARGE"
            , test "readStopwatchData of a multi-line string with an invalid value on one line is not a valid list of results" <|
                \() ->
                    readStopwatchData (String.Extra.replace "00:07:44" "nonsense" sampleData)
                        |> expectError "UNRECOGNISED_TIME"
            ]
        ]
