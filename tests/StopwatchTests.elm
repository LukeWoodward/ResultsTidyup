module StopwatchTests exposing (suite)

import Errors exposing (expectError)
import Expect
import FileHandling exposing (crlf)
import Stopwatch exposing (..)
import Test exposing (Test, describe, test)
import TestData exposing (expectedParsedSampleStopwatchData, sampleStopwatchData)


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


entry5 : MergeEntry
entry5 =
    NotNearMatch 406 419


sampleMergedTable : List MergedTableRow
sampleMergedTable =
    [ MergedTableRow 0 (Just 1) entry1 True noUnderlines
    , MergedTableRow 1 (Just 2) entry2 True noUnderlines
    , MergedTableRow 2 (Just 3) entry3 True noUnderlines
    , MergedTableRow 3 (Just 4) entry4 True noUnderlines
    , MergedTableRow 4 (Just 5) entry5 True noUnderlines
    ]


wrapEntry : MergeEntry -> MergedTableRow
wrapEntry entry =
    MergedTableRow 0 (Just 1) entry True noUnderlines


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
        , describe "generateInitialTable tests"
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
                    generateInitialTable [ entry1, entry2, entry3, entry4, entry5 ]
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
                            , MergedTableRow 4 (Just 4) entry5 True noUnderlines
                            ]
            , test "toggles out watch-2-only row and renumbers remaining rows" <|
                \() ->
                    toggleRowInTable 2 sampleMergedTable
                        |> Expect.equal
                            [ MergedTableRow 0 (Just 1) entry1 True noUnderlines
                            , MergedTableRow 1 (Just 2) entry2 True noUnderlines
                            , MergedTableRow 2 Nothing entry3 False noUnderlines
                            , MergedTableRow 3 (Just 3) entry4 True noUnderlines
                            , MergedTableRow 4 (Just 4) entry5 True noUnderlines
                            ]
            , test "toggles back in watch-1-only row" <|
                \() ->
                    let
                        previousData =
                            [ MergedTableRow 0 (Just 1) entry1 True noUnderlines
                            , MergedTableRow 1 (Just 2) entry2 True noUnderlines
                            , MergedTableRow 2 (Just 3) entry3 True noUnderlines
                            , MergedTableRow 3 Nothing entry4 False noUnderlines
                            , MergedTableRow 4 (Just 4) entry5 True noUnderlines
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
                            , MergedTableRow 4 (Just 4) entry5 True noUnderlines
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
                    flipTable (List.map wrapEntry [ entry1, entry2, entry3, entry4, entry5 ])
                        |> Expect.equal
                            (List.map wrapEntry
                                [ entry1
                                , NearMatch 285 284
                                , OneWatchOnly StopwatchOne 303
                                , OneWatchOnly StopwatchTwo 355
                                , NotNearMatch 419 406
                                ]
                            )
            ]
        , describe "flipMatchSummary tests"
            [ test "Flips a MatchSummary entry" <|
                \() ->
                    flipMatchSummary (StopwatchMatchSummary 1 2 3 4 5)
                        |> Expect.equal (StopwatchMatchSummary 1 2 3 5 4)
            ]
        , describe "merge tests"
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
            , test "merging two lists with no common numbers returns correct result 2" <|
                \() ->
                    merge 1 [ 20, 40, 60 ] [ 10, 30, 50 ]
                        |> Expect.equal
                            [ OneWatchOnly StopwatchTwo 10
                            , OneWatchOnly StopwatchOne 20
                            , OneWatchOnly StopwatchTwo 30
                            , OneWatchOnly StopwatchOne 40
                            , OneWatchOnly StopwatchTwo 50
                            , OneWatchOnly StopwatchOne 60
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
            , test "merging two lists with a not-near-match at the start returns expected result" <|
                \() ->
                    merge 2 [ 8, 30, 50 ] [ 14, 30, 50 ]
                        |> Expect.equal [ NotNearMatch 8 14, ExactMatch 30, ExactMatch 50 ]
            , test "merging two lists with a not-near-match at the start the other way around returns expected result" <|
                \() ->
                    merge 2 [ 14, 30, 50 ] [ 8, 30, 50 ]
                        |> Expect.equal [ NotNearMatch 14 8, ExactMatch 30, ExactMatch 50 ]
            , test "merging two lists with a not-near-match in the middle returns expected result" <|
                \() ->
                    merge 2 [ 10, 28, 50 ] [ 10, 34, 50 ]
                        |> Expect.equal [ ExactMatch 10, NotNearMatch 28 34, ExactMatch 50 ]
            , test "merging two lists with a not-near-match in the middle the other way around returns expected result" <|
                \() ->
                    merge 2 [ 10, 34, 50 ] [ 10, 28, 50 ]
                        |> Expect.equal [ ExactMatch 10, NotNearMatch 34 28, ExactMatch 50 ]
            , test "merging two lists with a not-near-match at the end returns expected result" <|
                \() ->
                    merge 2 [ 10, 30, 48 ] [ 10, 30, 54 ]
                        |> Expect.equal [ ExactMatch 10, ExactMatch 30, NotNearMatch 48 54 ]
            , test "merging two lists with a not-near-match at the end the other way around returns expected result" <|
                \() ->
                    merge 2 [ 10, 30, 54 ] [ 10, 30, 48 ]
                        |> Expect.equal [ ExactMatch 10, ExactMatch 30, NotNearMatch 54 48 ]
            , test "merging two lists with times on alternating stopwatches returns expected result 1" <|
                \() ->
                    merge 2 [ 10, 30, 48, 61 ] [ 10, 30, 54 ]
                        |> Expect.equal [ ExactMatch 10, ExactMatch 30, OneWatchOnly StopwatchOne 48, OneWatchOnly StopwatchTwo 54, OneWatchOnly StopwatchOne 61 ]
            , test "merging two lists with times on alternating stopwatches returns expected result 2" <|
                \() ->
                    merge 2 [ 10, 30, 54 ] [ 10, 30, 48, 61 ]
                        |> Expect.equal [ ExactMatch 10, ExactMatch 30, OneWatchOnly StopwatchTwo 48, OneWatchOnly StopwatchOne 54, OneWatchOnly StopwatchTwo 61 ]
            , test "merging two lists with a near-match and followed by an exact match returns expected result" <|
                \() ->
                    merge 2 [ 118, 127, 127 ] [ 118, 126, 127 ]
                        |> Expect.equal [ ExactMatch 118, NearMatch 127 126, ExactMatch 127 ]
            , test "merging two lists with a near-match and followed by an exact match returns expected result 2" <|
                \() ->
                    merge 2 [ 118, 126, 127 ] [ 118, 127, 127 ]
                        |> Expect.equal [ ExactMatch 118, NearMatch 126 127, ExactMatch 127 ]
            ]
        , describe "createMergedTable tests"
            [ test "Creating a merged table from an empty list generates empty data" <|
                \() ->
                    createMergedTable [] [] "empty1.txt" "empty2.txt"
                        |> Expect.equal (Double (DoubleStopwatchData [] [] "empty1.txt" "empty2.txt" [] (StopwatchMatchSummary 0 0 0 0 0)))
            , test "Creating a merged table from a list of identical times" <|
                \() ->
                    let
                        times : List Int
                        times =
                            [ 10, 30, 50 ]

                        mergedTable : List MergedTableRow
                        mergedTable =
                            merge 1 times times
                                |> generateInitialTable
                    in
                    createMergedTable times times "identical1.txt" "identical2.txt"
                        |> Expect.equal (Double (DoubleStopwatchData times times "identical1.txt" "identical2.txt" mergedTable (StopwatchMatchSummary 3 0 0 0 0)))
            , test "Creating a merged table from a pair of lists of times with a near match" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 10, 30, 50 ]

                        times2 : List Int
                        times2 =
                            [ 10, 31, 50 ]

                        mergedTable : List MergedTableRow
                        mergedTable =
                            merge 1 times1 times2
                                |> generateInitialTable
                    in
                    createMergedTable times1 times2 "nearmatch1.txt" "nearmatch2.txt"
                        |> Expect.equal (Double (DoubleStopwatchData times1 times2 "nearmatch1.txt" "nearmatch2.txt" mergedTable (StopwatchMatchSummary 2 1 0 0 0)))
            , test "Creating a merged table from a pair of lists of times with a not-near match" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 10, 29, 50 ]

                        times2 : List Int
                        times2 =
                            [ 10, 31, 50 ]

                        mergedTable : List MergedTableRow
                        mergedTable =
                            merge 1 times1 times2
                                |> generateInitialTable
                    in
                    createMergedTable times1 times2 "notnearmatch1.txt" "notnearmatch2.txt"
                        |> Expect.equal (Double (DoubleStopwatchData times1 times2 "notnearmatch1.txt" "notnearmatch2.txt" mergedTable (StopwatchMatchSummary 2 0 1 0 0)))
            , test "Creating a merged table from a pair of lists of times with a time only on the first watch" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 10, 30, 50 ]

                        times2 : List Int
                        times2 =
                            [ 10, 50 ]

                        mergedTable : List MergedTableRow
                        mergedTable =
                            merge 1 times1 times2
                                |> generateInitialTable
                    in
                    createMergedTable times1 times2 "watch1only1.txt" "watch1only2.txt"
                        |> Expect.equal (Double (DoubleStopwatchData times1 times2 "watch1only1.txt" "watch1only2.txt" mergedTable (StopwatchMatchSummary 2 0 0 1 0)))
            , test "Creating a merged table from a pair of lists of times with a time only on the second watch" <|
                \() ->
                    let
                        times1 : List Int
                        times1 =
                            [ 10, 50 ]

                        times2 : List Int
                        times2 =
                            [ 10, 30, 50 ]

                        mergedTable : List MergedTableRow
                        mergedTable =
                            merge 1 times1 times2
                                |> generateInitialTable
                    in
                    createMergedTable times1 times2 "watch2only1.txt" "watch2only2.txt"
                        |> Expect.equal (Double (DoubleStopwatchData times1 times2 "watch2only1.txt" "watch2only2.txt" mergedTable (StopwatchMatchSummary 2 0 0 0 1)))
            ]
        , describe "outputMergedTable tests"
            [ test "outputMergedTable of an empty list of stopwatch times is empty" <|
                \() ->
                    outputMergedTable []
                        |> Expect.equal (String.join crlf (header ++ [ footer ]))
            , test "outputMergedTable of a single exact-match time is the time" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (ExactMatch 517) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ "1,01/01/2001 00:08:37,00:08:37", footer ]))
            , test "outputMergedTable of a single near-match time with the first smaller is the first time" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (NearMatch 662 663) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ "1,01/01/2001 00:11:02,00:11:02", footer ]))
            , test "outputMergedTable of a single near-match time with the second smaller is the second time" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (NearMatch 663 662) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ "1,01/01/2001 00:11:02,00:11:02", footer ]))
            , test "outputMergedTable of a single not-near-match time with the first smaller is the first time" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (NotNearMatch 772 779) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ "1,01/01/2001 00:12:52,00:12:52", footer ]))
            , test "outputMergedTable of a single not-near-match time with the second smaller is the second time" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (NotNearMatch 779 772) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ "1,01/01/2001 00:12:52,00:12:52", footer ]))
            , test "outputMergedTable of a single time only on stopwatch 1 is the single time" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (OneWatchOnly StopwatchOne 588) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ "1,01/01/2001 00:09:48,00:09:48", footer ]))
            , test "outputMergedTable of a single time only on stopwatch 2 is the single time" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (OneWatchOnly StopwatchTwo 588) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ "1,01/01/2001 00:09:48,00:09:48", footer ]))
            , test "outputMergedTable of a non-included single time only on stopwatch 1 is empty" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (OneWatchOnly StopwatchOne 588) False noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ footer ]))
            , test "outputMergedTable of a non-included single time only on stopwatch 2 is empty" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 (Just 1) (OneWatchOnly StopwatchTwo 588) False noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ footer ]))
            , test "outputMergedTable of a single time on stopwatch 1 with no row number is empty" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 Nothing (OneWatchOnly StopwatchOne 588) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ footer ]))
            , test "outputMergedTable of a single time on stopwatch 2 with no row number is empty" <|
                \() ->
                    outputMergedTable [ MergedTableRow 1 Nothing (OneWatchOnly StopwatchTwo 588) True noUnderlines ]
                        |> Expect.equal (String.join crlf (header ++ [ footer ]))
            ]
        ]
