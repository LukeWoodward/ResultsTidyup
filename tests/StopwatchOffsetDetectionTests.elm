module StopwatchOffsetDetectionTests exposing (suite)

import Errors exposing (expectError)
import Expect
import Stopwatch exposing (DoubleStopwatchData, MergeEntry(..), MergedTableRow, Stopwatches(..), noUnderlines)
import StopwatchOffsetDetection exposing (findPossibleOffsets, getStopwatchTimeOffset)
import Test exposing (Test, describe, test)
import TestData exposing (defaultMatchSummary)


suite : Test
suite =
    describe "Stopwatch offset detection tests"
        [ describe "findPossibleOffsets tests"
            [ test "findPossibleOffsets returns an empty list for an empty list" <|
                \() ->
                    findPossibleOffsets 3 []
                        |> Expect.equal []
            , test "findPossibleOffsets returns an empty list for a singleton list" <|
                \() ->
                    findPossibleOffsets 3 [ 37 ]
                        |> Expect.equal []
            , test "findPossibleOffsets returns an empty list for a list where no number is repeated" <|
                \() ->
                    findPossibleOffsets 3 [ 37, 89, 46, 25, -47, 1, 13, 0, -52, -5 ]
                        |> Expect.equal []
            , test "findPossibleOffsets returns an empty list for a list where no number appears more than twice" <|
                \() ->
                    findPossibleOffsets 3 [ 37, -2, -47, 25, -47, 89, 37, 0, -52, -5 ]
                        |> Expect.equal []
            , test "findPossibleOffsets returns the values in a multiple-entry list that occur at least three times" <|
                \() ->
                    findPossibleOffsets 3 [ 37, 89, 46, 37, 88, 89, 37, 37, 42, 89, 37 ]
                        |> List.sort
                        |> Expect.equal [ 37, 89 ]
            ]
        , describe "getStopwatchTimeOffset tests"
            [ test "getStopwatchTimeOffset returns Nothing for no stopwatches" <|
                \() ->
                    getStopwatchTimeOffset None
                        |> Expect.equal Nothing
            , test "getStopwatchTimeOffset returns Nothing for a single stopwatch" <|
                \() ->
                    getStopwatchTimeOffset (Single "stopwatch1.txt" [ 1000, 1100, 1200 ])
                        |> Expect.equal Nothing
            , test "getStopwatchTimeOffset returns zero for a double stopwatch with identical times" <|
                \() ->
                    let
                        times : List Int
                        times =
                            [ 1000, 1033, 1047, 1066, 1097, 1104, 1119, 1177, 1206 ]
                    in
                    getStopwatchTimeOffset
                        (Double
                            { times1 = times
                            , times2 = times
                            , filename1 = "stopwatch1.txt"
                            , filename2 = "stopwatch2.txt"
                            , mergedTableRows = []
                            , matchSummary = defaultMatchSummary
                            }
                        )
                        |> Expect.equal (Just 0)
            , test "getStopwatchTimeOffset returns a nonzero number for a double stopwatch with identical times with a fixed difference" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 1000, 1033, 1047, 1066, 1097, 1104, 1119, 1177, 1206 ]

                        times2 : List Int
                        times2 =
                            List.map (\time -> time + 19) times1
                    in
                    getStopwatchTimeOffset
                        (Double
                            { times1 = times1
                            , times2 = times2
                            , filename1 = "stopwatch1.txt"
                            , filename2 = "stopwatch2.txt"
                            , mergedTableRows = []
                            , matchSummary = defaultMatchSummary
                            }
                        )
                        |> Expect.equal (Just -19)
            , test "getStopwatchTimeOffset returns Nothing for two unrelated sets of times" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 1000, 1033, 1047, 1066, 1097, 1104, 1119, 1177, 1206 ]

                        times2 : List Int
                        times2 =
                            [ 1104, 1165, 1202, 1204, 1222, 1239, 1248, 1258, 1269 ]
                    in
                    getStopwatchTimeOffset
                        (Double
                            { times1 = times1
                            , times2 = times2
                            , filename1 = "stopwatch1.txt"
                            , filename2 = "stopwatch2.txt"
                            , mergedTableRows = []
                            , matchSummary = defaultMatchSummary
                            }
                        )
                        |> Expect.equal Nothing
            ]
        ]
