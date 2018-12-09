module MergerTests exposing (suite)

import DataStructures exposing (WhichStopwatch(..))
import Errors exposing (expectError)
import Expect
import Merger exposing (MergeEntry(..), merge)
import Test exposing (Test, describe, test)
import TimeHandling exposing (formatTime, parseTime)


suite : Test
suite =
    describe "Merger tests"
        [ describe "merge tests"
            [ test "merging two empty lists returns empty list" <|
                \() ->
                    merge 1 [] []
                        |> Expect.equal []
            , test "merging two singleton lists returns singleton list" <|
                \() ->
                    merge 1 [ 5 ] [ 5 ]
                        |> Expect.equal [ ExactMatch 5 ]
            , test "merging two lists with the same repeated result returns correct repeated result" <|
                \() ->
                    merge 1 [ 5, 5, 5 ] [ 5, 5, 5 ]
                        |> Expect.equal [ ExactMatch 5, ExactMatch 5, ExactMatch 5 ]
            , test "merging two lists with the same repeated result with first longer than second returns correct result" <|
                \() ->
                    merge 1 [ 5, 5, 5 ] [ 5, 5 ]
                        |> Expect.equal [ ExactMatch 5, ExactMatch 5, OneWatchOnly StopwatchOne 5 ]
            , test "merging two lists with the same repeated result with first shorter than second returns correct result" <|
                \() ->
                    merge 1 [ 5, 5 ] [ 5, 5, 5 ]
                        |> Expect.equal [ ExactMatch 5, ExactMatch 5, OneWatchOnly StopwatchTwo 5 ]
            , test "merging two lists with no common numbers returns correct result" <|
                \() ->
                    merge 1 [ 10, 30, 50 ] [ 20, 40, 60 ]
                        |> Expect.equal
                            [ OneWatchOnly StopwatchOne 10
                            , OneWatchOnly StopwatchTwo 20
                            , OneWatchOnly StopwatchOne 30
                            , OneWatchOnly StopwatchTwo 40
                            , OneWatchOnly StopwatchOne 50
                            , OneWatchOnly StopwatchTwo 60
                            ]
            , test "merging two lists with near-matches returns expected result" <|
                \() ->
                    merge 1 [ 10, 30, 50 ] [ 10, 31, 50 ]
                        |> Expect.equal [ ExactMatch 10, NearMatch 30 31, ExactMatch 50 ]
            , test "merging two lists with a near-match and a nearer match returns expected result 1" <|
                \() ->
                    merge 2 [ 10, 29, 30, 50 ] [ 10, 31, 50 ]
                        |> Expect.equal [ ExactMatch 10, OneWatchOnly StopwatchOne 29, NearMatch 30 31, ExactMatch 50 ]
            , test "merging two lists with a near-match and a nearer match returns expected result 2" <|
                \() ->
                    merge 2 [ 10, 31, 50 ] [ 10, 29, 30, 50 ]
                        |> Expect.equal [ ExactMatch 10, OneWatchOnly StopwatchTwo 29, NearMatch 31 30, ExactMatch 50 ]
            , test "merging two lists with a near-match and a nearer match returns expected result 3" <|
                \() ->
                    merge 2 [ 10, 32, 33, 50 ] [ 10, 31, 50 ]
                        |> Expect.equal [ ExactMatch 10, NearMatch 32 31, OneWatchOnly StopwatchOne 33, ExactMatch 50 ]
            , test "merging two lists with a near-match and a nearer match returns expected result 4" <|
                \() ->
                    merge 2 [ 10, 31, 50 ] [ 10, 32, 33, 50 ]
                        |> Expect.equal [ ExactMatch 10, NearMatch 31 32, OneWatchOnly StopwatchTwo 33, ExactMatch 50 ]
            , test "merging two lists with a near-match and followed by an exact match returns expected result" <|
                \() ->
                    merge 2 [ 118, 127, 127 ] [ 118, 126, 127 ]
                        |> Expect.equal [ ExactMatch 118, NearMatch 127 126, ExactMatch 127 ]
            , test "merging two lists with a near-match and followed by an exact match returns expected result 2" <|
                \() ->
                    merge 2 [ 118, 126, 127 ] [ 118, 127, 127 ]
                        |> Expect.equal [ ExactMatch 118, NearMatch 126 127, ExactMatch 127 ]
            ]
        ]
