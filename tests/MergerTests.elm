module MergerTests exposing (suite)

import Expect
import Test exposing (describe, test, Test)
import Merger exposing (MergeEntry(..), merge)
import Errors exposing (expectError)
import TimeHandling exposing (parseTime, formatTime)


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
                        |> Expect.equal [ ExactMatch 5, ExactMatch 5, Watch1Only 5 ]
            , test "merging two lists with the same repeated result with first shorter than second returns correct result" <|
                \() ->
                    merge 1 [ 5, 5 ] [ 5, 5, 5 ]
                        |> Expect.equal [ ExactMatch 5, ExactMatch 5, Watch2Only 5 ]
            , test "merging two lists with no common numbers returns correct result" <|
                \() ->
                    merge 1 [ 10, 30, 50 ] [ 20, 40, 60 ]
                        |> Expect.equal [ Watch1Only 10, Watch2Only 20, Watch1Only 30, Watch2Only 40, Watch1Only 50, Watch2Only 60 ]
            , test "merging two lists with near-matches returns expected result" <|
                \() ->
                    merge 1 [ 10, 30, 50 ] [ 10, 31, 50 ]
                        |> Expect.equal [ ExactMatch 10, NearMatch 30 31, ExactMatch 50 ]
            , test "merging two lists with a near-match and a nearer match returns expected result 1" <|
                \() ->
                    merge 2 [ 10, 29, 30, 50 ] [ 10, 31, 50 ]
                        |> Expect.equal [ ExactMatch 10, Watch1Only 29, NearMatch 30 31, ExactMatch 50 ]
            , test "merging two lists with a near-match and a nearer match returns expected result 2" <|
                \() ->
                    merge 2 [ 10, 31, 50 ] [ 10, 29, 30, 50 ]
                        |> Expect.equal [ ExactMatch 10, Watch2Only 29, NearMatch 31 30, ExactMatch 50 ]
            , test "merging two lists with a near-match and a nearer match returns expected result 3" <|
                \() ->
                    merge 2 [ 10, 32, 33, 50 ] [ 10, 31, 50 ]
                        |> Expect.equal [ ExactMatch 10, NearMatch 32 31, Watch1Only 33, ExactMatch 50 ]
            , test "merging two lists with a near-match and a nearer match returns expected result 4" <|
                \() ->
                    merge 2 [ 10, 31, 50 ] [ 10, 32, 33, 50 ]
                        |> Expect.equal [ ExactMatch 10, NearMatch 31 32, Watch2Only 33, ExactMatch 50 ]
            ]
        ]
