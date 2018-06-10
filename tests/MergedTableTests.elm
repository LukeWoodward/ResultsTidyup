module MergedTableTests exposing (suite)

import Expect
import Test exposing (describe, test, Test)
import Merger exposing (MergeEntry(..))
import MergedTable exposing (..)
import Errors exposing (expectError)


entry1 : MergeEntry
entry1 =
    ExactMatch 259


entry2 : MergeEntry
entry2 =
    NearMatch 284 285


entry3 : MergeEntry
entry3 =
    Watch2Only 303


entry4 : MergeEntry
entry4 =
    Watch1Only 355


sampleMergedTable : List MergedTableRow
sampleMergedTable =
    [ MergedTableRow 0 (Just 1) entry1 True
    , MergedTableRow 1 (Just 2) entry2 True
    , MergedTableRow 2 (Just 3) entry3 True
    , MergedTableRow 3 (Just 4) entry4 True
    ]


suite : Test
suite =
    describe "Merged Table tests"
        [ describe "generateInitialTable tests"
            [ test "generates empty table from empty list of merge entries" <|
                \() ->
                    generateInitialTable []
                        |> Expect.equal []
            , test "generates singleton table from single list of merge entries" <|
                \() ->
                    generateInitialTable [ entry1 ]
                        |> Expect.equal [ MergedTableRow 0 (Just 1) entry1 True ]
            , test "generates table with three rows from list of three merge entries" <|
                \() ->
                    generateInitialTable [ entry1, entry2, entry3, entry4 ]
                        |> Expect.equal sampleMergedTable
            ]
        , describe "toggleRowInTable tests"
            [ test "has no effect when toggling exact-match row" <|
                \() ->
                    toggleRowInTable 0 sampleMergedTable
                        |> Expect.equal sampleMergedTable
            , test "has no effect when toggling near-match row" <|
                \() ->
                    toggleRowInTable 1 sampleMergedTable
                        |> Expect.equal sampleMergedTable
            , test "toggles out watch-1-only row and renumbers remaining rows" <|
                \() ->
                    toggleRowInTable 3 sampleMergedTable
                        |> Expect.equal
                            [ MergedTableRow 0 (Just 1) entry1 True
                            , MergedTableRow 1 (Just 2) entry2 True
                            , MergedTableRow 2 (Just 3) entry3 True
                            , MergedTableRow 3 Nothing entry4 False
                            ]
            , test "toggles out watch-2-only row and renumbers remaining rows" <|
                \() ->
                    toggleRowInTable 2 sampleMergedTable
                        |> Expect.equal
                            [ MergedTableRow 0 (Just 1) entry1 True
                            , MergedTableRow 1 (Just 2) entry2 True
                            , MergedTableRow 2 Nothing entry3 False
                            , MergedTableRow 3 (Just 3) entry4 True
                            ]
            , test "toggles back in watch-1-only row" <|
                \() ->
                    let
                        previousData =
                            [ MergedTableRow 0 (Just 1) entry1 True
                            , MergedTableRow 1 (Just 2) entry2 True
                            , MergedTableRow 2 (Just 3) entry3 True
                            , MergedTableRow 3 Nothing entry4 False
                            ]
                    in
                        toggleRowInTable 3 previousData
                            |> Expect.equal sampleMergedTable
            , test "toggles back in watch-2-only row" <|
                \() ->
                    let
                        previousData =
                            [ MergedTableRow 0 (Just 1) entry1 True
                            , MergedTableRow 1 (Just 2) entry2 True
                            , MergedTableRow 2 Nothing entry3 False
                            , MergedTableRow 3 (Just 3) entry4 True
                            ]
                    in
                        toggleRowInTable 2 previousData
                            |> Expect.equal sampleMergedTable
            , test "has no effect when toggling nonexistent row" <|
                \() ->
                    toggleRowInTable 99 sampleMergedTable
                        |> Expect.equal sampleMergedTable
            ]
        ]