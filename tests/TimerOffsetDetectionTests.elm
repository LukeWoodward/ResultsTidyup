module TimerOffsetDetectionTests exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import TestData exposing (defaultMatchSummary)
import Timer exposing (MergeEntry(..), TimerFile, Timers(..))
import TimerOffsetDetection exposing (findPossibleOffsets, getTimerTimeOffset)


suite : Test
suite =
    describe "Timer offset detection tests"
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
        , describe "getTimerTimeOffset tests"
            [ test "getTimerTimeOffset returns Nothing for no timers" <|
                \() ->
                    getTimerTimeOffset None
                        |> Expect.equal Nothing
            , test "getTimerTimeOffset returns Nothing for a single timer" <|
                \() ->
                    getTimerTimeOffset (Single (TimerFile "timer1.txt" "Name1") [ 1000, 1100, 1200 ])
                        |> Expect.equal Nothing
            , test "getTimerTimeOffset returns zero for a double timer with identical times" <|
                \() ->
                    let
                        times : List Int
                        times =
                            [ 1000, 1033, 1047, 1066, 1097, 1104, 1119, 1177, 1206 ]
                    in
                    getTimerTimeOffset
                        (Double
                            { times1 = times
                            , times2 = times
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows = []
                            , matchSummary = defaultMatchSummary
                            }
                        )
                        |> Expect.equal (Just 0)
            , test "getTimerTimeOffset returns a nonzero number for a double timer with identical times with a fixed difference" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 1000, 1033, 1047, 1066, 1097, 1104, 1119, 1177, 1206 ]

                        times2 : List Int
                        times2 =
                            List.map (\time -> time + 19) times1
                    in
                    getTimerTimeOffset
                        (Double
                            { times1 = times1
                            , times2 = times2
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows = []
                            , matchSummary = defaultMatchSummary
                            }
                        )
                        |> Expect.equal (Just -19)
            , test "getTimerTimeOffset returns Nothing for two unrelated sets of times" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 1000, 1033, 1047, 1066, 1097, 1104, 1119, 1177, 1206 ]

                        times2 : List Int
                        times2 =
                            [ 1104, 1165, 1202, 1204, 1222, 1239, 1248, 1258, 1269 ]
                    in
                    getTimerTimeOffset
                        (Double
                            { times1 = times1
                            , times2 = times2
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows = []
                            , matchSummary = defaultMatchSummary
                            }
                        )
                        |> Expect.equal Nothing
            ]
        ]
