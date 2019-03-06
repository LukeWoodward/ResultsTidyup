module StopwatchOffsetDetectionTests exposing (suite)

import Errors exposing (expectError)
import Expect
import MergedTable exposing (DoubleStopwatchData, MergedTableRow, Stopwatches(..), noUnderlines)
import Merger exposing (MergeEntry(..))
import StopwatchOffsetDetection exposing (findMostCommonNumber, getStopwatchTimeOffset)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Stopwatch offset detection tests"
        [ describe "findMostCommonNumber tests"
            [ test "findMostCommonNumber returns Nothing for an empty list" <|
                \() ->
                    findMostCommonNumber []
                        |> Expect.equal Nothing
            , test "findMostCommonNumber returns Nothing for a singleton list" <|
                \() ->
                    findMostCommonNumber [ 37 ]
                        |> Expect.equal Nothing
            , test "findMostCommonNumber returns Nothing for a list where no number is repeated" <|
                \() ->
                    findMostCommonNumber [ 37, 89, 46, 25, -47, 1, 13, 0, -52, -5 ]
                        |> Expect.equal Nothing
            , test "findMostCommonNumber returns Nothing for a list where no number appears more than twice" <|
                \() ->
                    findMostCommonNumber [ 37, -2, -47, 25, -47, 89, 37, 0, -52, -5 ]
                        |> Expect.equal Nothing
            , test "findMostCommonNumber returns the most common value in a multiple-entry list if it occurs at least three times" <|
                \() ->
                    findMostCommonNumber [ 37, 89, 46, 37, 88, 89, 37, 37, 42, 89, 37 ]
                        |> Expect.equal (Just 37)
            , test "findMostCommonNumber returns one of the most common values in a multiple-entry list with more than one most-common value that occur at least three times" <|
                \() ->
                    let
                        mostCommonNumber =
                            findMostCommonNumber [ 37, 89, 46, 37, 88, 89, 37, 42, 89, 37 ]
                    in
                    Expect.true "Result should be Just 37 or Just 89" (mostCommonNumber == Just 37 || mostCommonNumber == Just 89)
            ]
        , describe "getStopwatchTimeOffset tests"
            [ test "getStopwatchTimeOffset returns zero for no stopwatches" <|
                \() ->
                    getStopwatchTimeOffset None
                        |> Expect.equal 0
            , test "getStopwatchTimeOffset returns zero for a single stopwatch" <|
                \() ->
                    getStopwatchTimeOffset (Single "stopwatch1.txt" [ 1000, 1100, 1200 ])
                        |> Expect.equal 0
            , test "getStopwatchTimeOffset returns zero for a double stopwatch with identical times" <|
                \() ->
                    let
                        times : List Int
                        times =
                            [ 1000, 1033, 1047, 1066, 1097, 1104, 1119, 1177, 1206 ]
                    in
                    getStopwatchTimeOffset (Double { times1 = times, times2 = times, filename1 = "stopwatch1.txt", filename2 = "stopwatch2.txt", mergedTableRows = [] })
                        |> Expect.equal 0
            , test "getStopwatchTimeOffset returns a positive number for a double stopwatch with identical times with a fixed difference" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 1000, 1033, 1047, 1066, 1097, 1104, 1119, 1177, 1206 ]

                        times2 : List Int
                        times2 =
                            List.map (\time -> time + 19) times1
                    in
                    getStopwatchTimeOffset (Double { times1 = times1, times2 = times2, filename1 = "stopwatch1.txt", filename2 = "stopwatch2.txt", mergedTableRows = [] })
                        |> Expect.equal -19
            ]
        ]