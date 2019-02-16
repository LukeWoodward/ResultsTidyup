module MergedTableTests exposing (suite)

import DataStructures exposing (WhichStopwatch(..))
import Errors exposing (expectError)
import Expect
import MergedTable exposing (..)
import Merger exposing (MergeEntry(..))
import Test exposing (Test, describe, test)


entry1 : MergeEntry
entry1 =
    ExactMatch 259


entry2 : MergeEntry
entry2 =
    NearMatch 284 285


entry3 : MergeEntry
entry3 =
    OneWatchOnly StopwatchTwo 303


entry4 : MergeEntry
entry4 =
    OneWatchOnly StopwatchOne 355


sampleMergedTable : List MergedTableRow
sampleMergedTable =
    [ MergedTableRow 0 (Just 1) entry1 True noUnderlines
    , MergedTableRow 1 (Just 2) entry2 True noUnderlines
    , MergedTableRow 2 (Just 3) entry3 True noUnderlines
    , MergedTableRow 3 (Just 4) entry4 True noUnderlines
    ]


wrapEntry : MergeEntry -> MergedTableRow
wrapEntry entry =
    MergedTableRow 0 (Just 1) entry True noUnderlines


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
                        |> Expect.equal [ MergedTableRow 0 (Just 1) entry1 True noUnderlines ]
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
                            [ MergedTableRow 0 (Just 1) entry1 True noUnderlines
                            , MergedTableRow 1 (Just 2) entry2 True noUnderlines
                            , MergedTableRow 2 (Just 3) entry3 True noUnderlines
                            , MergedTableRow 3 Nothing entry4 False noUnderlines
                            ]
            , test "toggles out watch-2-only row and renumbers remaining rows" <|
                \() ->
                    toggleRowInTable 2 sampleMergedTable
                        |> Expect.equal
                            [ MergedTableRow 0 (Just 1) entry1 True noUnderlines
                            , MergedTableRow 1 (Just 2) entry2 True noUnderlines
                            , MergedTableRow 2 Nothing entry3 False noUnderlines
                            , MergedTableRow 3 (Just 3) entry4 True noUnderlines
                            ]
            , test "toggles back in watch-1-only row" <|
                \() ->
                    let
                        previousData =
                            [ MergedTableRow 0 (Just 1) entry1 True noUnderlines
                            , MergedTableRow 1 (Just 2) entry2 True noUnderlines
                            , MergedTableRow 2 (Just 3) entry3 True noUnderlines
                            , MergedTableRow 3 Nothing entry4 False noUnderlines
                            ]
                    in
                    toggleRowInTable 3 previousData
                        |> Expect.equal sampleMergedTable
            , test "toggles back in watch-2-only row" <|
                \() ->
                    let
                        previousData =
                            [ MergedTableRow 0 (Just 1) entry1 True noUnderlines
                            , MergedTableRow 1 (Just 2) entry2 True noUnderlines
                            , MergedTableRow 2 Nothing entry3 False noUnderlines
                            , MergedTableRow 3 (Just 3) entry4 True noUnderlines
                            ]
                    in
                    toggleRowInTable 2 previousData
                        |> Expect.equal sampleMergedTable
            , test "has no effect when toggling nonexistent row" <|
                \() ->
                    toggleRowInTable 99 sampleMergedTable
                        |> Expect.equal sampleMergedTable
            ]
        , describe "flipTable tests"
            [ test "Flips a table of entries" <|
                \() ->
                    flipTable (List.map wrapEntry [ entry1, entry2, entry3, entry4 ])
                        |> Expect.equal
                            (List.map wrapEntry
                                [ entry1
                                , NearMatch 285 284
                                , OneWatchOnly StopwatchOne 303
                                , OneWatchOnly StopwatchTwo 355
                                ]
                            )
            ]
        ]
