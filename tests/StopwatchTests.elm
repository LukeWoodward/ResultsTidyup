module StopwatchTests exposing (suite)

import Errors exposing (expectError)
import Expect
import FileHandling exposing (crlf)
import Stopwatch exposing (..)
import Test exposing (Test, describe, test)
import TestData exposing (expectedParsedSampleStopwatchData, sampleStopwatchData)


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
                    readStopwatchData sampleStopwatchData
                        |> Expect.equal (Ok expectedParsedSampleStopwatchData)
            , test "readStopwatchData of a valid multi-line string with CRLF line-endings is a valid list of results" <|
                \() ->
                    readStopwatchData (String.replace "\n" crlf sampleStopwatchData)
                        |> Expect.equal (Ok expectedParsedSampleStopwatchData)
            , test "readStopwatchData of a valid multi-line string with CR line-endings is a valid list of results" <|
                \() ->
                    readStopwatchData (String.replace "\n" "\u{000D}" sampleStopwatchData)
                        |> Expect.equal (Ok expectedParsedSampleStopwatchData)
            , test "readStopwatchData of a valid multi-line string with blank lines is a valid list of results" <|
                \() ->
                    readStopwatchData (String.replace "\n" "\n\n" sampleStopwatchData)
                        |> Expect.equal (Ok expectedParsedSampleStopwatchData)
            , test "readStopwatchData of an empty string is not a valid list of results" <|
                \() ->
                    readStopwatchData ""
                        |> expectError "NO_RESULTS"
            , test "readStopwatchData of a string containing binary data is not a valid list of results" <|
                \() ->
                    readStopwatchData "\u{0000}\u{0000}\u{0000}Z\u{0001}j\u{0007}\u{0000}\u{0003}\u{0000}$\u{0000}"
                        |> expectError "BINARY_FILE"
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
            , test "readStopwatchData of a multi-line string with an invalid value on one line is not a valid list of results" <|
                \() ->
                    readStopwatchData (String.replace "00:07:44" "nonsense" sampleStopwatchData)
                        |> expectError "UNRECOGNISED_TIME"
            ]
        ]
