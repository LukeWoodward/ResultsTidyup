module ProblemsTests exposing (suite)

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , DeletionReason(..)
        , DeletionStatus(..)
        , UnrecognisedLine
        )
import BarcodeScannerTests exposing (createBarcodeScannerData)
import Dict
import Expect
import Problems
    exposing
        ( AthleteWithAndWithoutPositionProblem
        , AthleteWithMultiplePositionsProblem
        , MisScannedAthleteBarcodeProblem
        , PositionAndTime
        , PositionOffEndOfTimesProblem
        , PositionWithMultipleAthletesProblem
        , TimerTimesOutOfOrder
        , identifyProblems
        , noIgnoredProblems
        , noProblems
        )
import Test exposing (Test, describe, test)
import TestData exposing (doubleTimers)
import Timer exposing (MergeEntry(..), MergedTableRow, TimerFile, TimerMatchSummary, Timers(..), WhichTimer(..))


wrapMergeEntriesInTable : List MergeEntry -> List MergedTableRow
wrapMergeEntriesInTable entries =
    let
        wrapRow : Int -> MergeEntry -> MergedTableRow
        wrapRow index entry =
            MergedTableRow index (Just (index + 1)) entry True
    in
    List.indexedMap wrapRow entries


doubleTimersForTimeLookupTests : Timers
doubleTimersForTimeLookupTests =
    let
        expectedEntries : List MergedTableRow
        expectedEntries =
            [ { index = 0, rowNumber = Just 1, entry = ExactMatch 191, included = True }
            , { index = 1, rowNumber = Just 2, entry = NotNearMatch 469 463, included = True }
            , { index = 2, rowNumber = Nothing, entry = OneTimerOnly TimerOne 603, included = False }
            , { index = 3, rowNumber = Just 3, entry = ExactMatch 746, included = True }
            , { index = 4, rowNumber = Nothing, entry = OneTimerOnly TimerTwo 791, included = False }
            , { index = 5, rowNumber = Just 4, entry = ExactMatch 882, included = True }
            ]

        expectedMatchSummary : TimerMatchSummary
        expectedMatchSummary =
            { exactMatches = 3, nearMatches = 0, notNearMatches = 1, timer1Only = 1, timer2Only = 1 }
    in
    Double
        { times1 = [ 191, 469, 603, 746, 882 ]
        , times2 = [ 191, 463, 746, 791, 882 ]
        , file1 = TimerFile "timer1.txt" "Name1"
        , file2 = TimerFile "timer2.txt" "Name2"
        , mergedTableRows = expectedEntries
        , matchSummary = expectedMatchSummary
        }


suite : Test
suite =
    describe "Problems tests"
        [ describe "identifyProblems tests"
            [ test "identifyProblems returns no problems for no data" <|
                \() ->
                    identifyProblems None BarcodeScanner.empty noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns no problems for a single athlete with a single position" <|
                \() ->
                    identifyProblems None (createBarcodeScannerData (Dict.singleton 12 [ "A123456" ]) []) noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns no problems for three athletes with three different positions" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns no problem for two barcode scanner files with two equal last-scan dates" <|
                \() ->
                    let
                        barcodeScannerData : BarcodeScannerData
                        barcodeScannerData =
                            BarcodeScannerData
                                [ BarcodeScannerFile "barcodes1.txt" "Name1" []
                                , BarcodeScannerFile "barcodes2.txt" "Name2" []
                                ]
                                Dict.empty
                                []
                                []
                    in
                    identifyProblems
                        None
                        barcodeScannerData
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns a problem for an athlete with two repeated positions" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A123456" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 12 Nothing, PositionAndTime 19 Nothing ]
                                    ]
                            }
            , test "identifyProblems returns a problem for an athlete with two repeated positions and looks times up from single timer data" <|
                \() ->
                    identifyProblems
                        (Single (TimerFile "timer1.txt" "Name1") [ 604, 775, 802, 993, 1011, 1143, 1197 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 6, [ "A123456" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 3 (Just 802), PositionAndTime 6 (Just 1143) ]
                                    ]
                            }
            , test "identifyProblems returns a problem for an athlete with two repeated positions and looks times up from single timer data, ignoring time off the end" <|
                \() ->
                    identifyProblems
                        (Single (TimerFile "timer1.txt" "Name1") [ 604, 775, 802, 993, 1011 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 6, [ "A123456" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 3 (Just 802), PositionAndTime 6 Nothing ]
                                    ]
                                , positionOffEndOfTimes = Just (PositionOffEndOfTimesProblem 5 6)
                            }
            , test "identifyProblems returns a problem for an athlete with two repeated positions and looks times up from double timer data, hitting exact match and near match" <|
                \() ->
                    identifyProblems
                        doubleTimers
                        (createBarcodeScannerData (Dict.fromList [ ( 1, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 2, [ "A123456" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 1 (Just 191), PositionAndTime 2 (Just 463) ]
                                    ]
                            }
            , test "identifyProblems returns a problem for an athlete with two repeated positions and looks times up from double timer data, hitting times only on one of the timers" <|
                \() ->
                    identifyProblems
                        doubleTimers
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A123456" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 3 (Just 603), PositionAndTime 5 (Just 791) ]
                                    ]
                            }
            , test "identifyProblems returns a problem for an athlete with two repeated positions and looks times up from double timer data, hitting a not-near match and skipping over ignored times" <|
                \() ->
                    identifyProblems
                        doubleTimersForTimeLookupTests
                        (createBarcodeScannerData (Dict.fromList [ ( 2, [ "A123456" ] ), ( 3, [ "A252525" ] ), ( 4, [ "A123456" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 2 (Just 463), PositionAndTime 4 (Just 882) ]
                                    ]
                            }
            , test "identifyProblems returns a problem for an athlete with two repeated positions and looks times up from double timer data, ignoring time off the end" <|
                \() ->
                    identifyProblems
                        doubleTimers
                        (createBarcodeScannerData (Dict.fromList [ ( 1, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 8, [ "A123456" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 1 (Just 191), PositionAndTime 8 Nothing ]
                                    ]
                                , positionOffEndOfTimes = Just (PositionOffEndOfTimesProblem 7 8)
                            }
            , test "identifyProblems returns a problem for an athlete with three repeated positions" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A123456" ] ), ( 19, [ "A123456" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 12 Nothing, PositionAndTime 16 Nothing, PositionAndTime 19 Nothing ]
                                    ]
                            }
            , test "identifyProblems returns two problems for two athletes with repeated positions" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A123456" ] ), ( 25, [ "A252525" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 12 Nothing, PositionAndTime 19 Nothing ]
                                    , AthleteWithMultiplePositionsProblem "A252525" [ PositionAndTime 16 Nothing, PositionAndTime 25 Nothing ]
                                    ]
                            }
            , test "identifyProblems returns a problem for a position with two athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525" ] ), ( 19, [ "A987654" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsWithMultipleAthletes = [ PositionWithMultipleAthletesProblem 12 [ "A123456", "A252525" ] ] }
            , test "identifyProblems returns a problem for a position with three athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525", "A748159" ] ), ( 19, [ "A987654" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsWithMultipleAthletes = [ PositionWithMultipleAthletesProblem 12 [ "A123456", "A252525", "A748159" ] ] }
            , test "identifyProblems returns two problems for two positions with two athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525" ] ), ( 19, [ "A987654", "A748159" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | positionsWithMultipleAthletes =
                                    [ PositionWithMultipleAthletesProblem 12 [ "A123456", "A252525" ]
                                    , PositionWithMultipleAthletesProblem 19 [ "A748159", "A987654" ]
                                    ]
                            }
            , test "identifyProblems returns no problems for a finish position not off the end" <|
                \() ->
                    identifyProblems
                        (Single (TimerFile "timer1.txt" "Name1") [ 1000, 1100, 1200, 1300, 1400 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns a problem for a finish position off the end" <|
                \() ->
                    identifyProblems
                        (Single (TimerFile "timer1.txt" "Name1") [ 1000, 1100, 1200, 1300 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionOffEndOfTimes = Just (PositionOffEndOfTimesProblem 4 5) }
            , test "identifyProblems returns a single problem for multiple finish positions off the end" <|
                \() ->
                    identifyProblems
                        (Single (TimerFile "timer1.txt" "Name1") [ 1000, 1100 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionOffEndOfTimes = Just (PositionOffEndOfTimesProblem 2 5) }
            , test "identifyProblems returns a single problem for an athlete with no position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A951623" ])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesMissingPosition = [ "A951623" ] }
            , test "identifyProblems returns multiple problems for multiple athletes with no position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A321456", "A951623" ])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesMissingPosition = [ "A321456", "A951623" ] }
            , test "identifyProblems returns a mis-scanned athlete barcode problem for an athlete barcode too long and similar to an existing barcode" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A123456789" ])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | misScannedAthleteBarcodes = [ MisScannedAthleteBarcodeProblem "A123456789" "A123456" ] }
            , test "identifyProblems returns a mis-scanned athlete barcode problem for an athlete barcode too long scanned multiple times and similar to an existing barcode" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A123456789", "A123456789", "A123456789" ])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | misScannedAthleteBarcodes = [ MisScannedAthleteBarcodeProblem "A123456789" "A123456" ] }
            , test "identifyProblems returns an athlete-barcode-without-position problem for an athlete barcode too long but not similar to an existing barcode" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A505479977654" ])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesMissingPosition = [ "A505479977654" ] }
            , test "identifyProblems returns a problem for an unrecognised barcode-scanner line" <|
                \() ->
                    identifyProblems
                        None
                        (BarcodeScannerData [] Dict.empty [] [ UnrecognisedLine "This is not a valid line" "code" "message" ])
                        noIgnoredProblems
                        |> Expect.equal { noProblems | unrecognisedBarcodeScannerLines = [ "This is not a valid line" ] }
            , test "identifyProblems returns a fixable problem for an athlete with a position and with a missing position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            [ "A252525" ]
                        )
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesWithAndWithoutPosition = [ AthleteWithAndWithoutPositionProblem "A252525" 1 16 ] }
            , test "identifyProblems returns two fixable problems for two athletes with and without a position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            [ "A252525", "A987654" ]
                        )
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesWithAndWithoutPosition = [ AthleteWithAndWithoutPositionProblem "A252525" 1 16, AthleteWithAndWithoutPositionProblem "A987654" 1 19 ] }
            , test "identifyProblems returns two fixable problems for two athletes with and without a position, one with multiple no-position scans" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            [ "A252525", "A987654", "A252525", "A252525" ]
                        )
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesWithAndWithoutPosition = [ AthleteWithAndWithoutPositionProblem "A252525" 3 16, AthleteWithAndWithoutPositionProblem "A987654" 1 19 ] }
            , test "identifyProblems returns no problems for timers almost in sync" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1201 ]
                            , times2 = [ 1000, 1080, 1200 ]
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows = wrapMergeEntriesInTable [ ExactMatch 1000, ExactMatch 1100, NearMatch 1201 1200 ]
                            , matchSummary = TimerMatchSummary 2 1 0 0 0
                            }
                        )
                        BarcodeScanner.empty
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns a problem for timers not in sync" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1200 ]
                            , times2 = [ 1005, 1085, 1205 ]
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows =
                                wrapMergeEntriesInTable
                                    [ OneTimerOnly TimerOne 1000
                                    , OneTimerOnly TimerTwo 1005
                                    , OneTimerOnly TimerOne 1100
                                    , OneTimerOnly TimerTwo 1105
                                    , OneTimerOnly TimerOne 1200
                                    , OneTimerOnly TimerTwo 1205
                                    ]
                            , matchSummary = TimerMatchSummary 0 0 0 3 3
                            }
                        )
                        BarcodeScanner.empty
                        noIgnoredProblems
                        |> Expect.equal { noProblems | timerTimeOffset = Just -5 }
            , test "identifyProblems returns no problems for timers not in sync if problem ignored" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1200 ]
                            , times2 = [ 1005, 1085, 1205 ]
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows =
                                wrapMergeEntriesInTable
                                    [ OneTimerOnly TimerOne 1000
                                    , OneTimerOnly TimerTwo 1005
                                    , OneTimerOnly TimerOne 1100
                                    , OneTimerOnly TimerTwo 1105
                                    , OneTimerOnly TimerOne 1200
                                    , OneTimerOnly TimerTwo 1205
                                    ]
                            , matchSummary = TimerMatchSummary 0 0 0 3 3
                            }
                        )
                        BarcodeScanner.empty
                        { noIgnoredProblems | ignoreTimerTimeOffsets = True }
                        |> Expect.equal noProblems
            , test "identifyProblems returns a problem for timer times out of order when a single timer" <|
                \() ->
                    identifyProblems (Single (TimerFile "timer1.txt" "Name1") [ 1000, 1200, 1080 ]) BarcodeScanner.empty noIgnoredProblems
                        |> Expect.equal { noProblems | timerTimesOutOfOrder = [ TimerTimesOutOfOrder TimerOne "20:00" "18:00" ] }
            , test "identifyProblems returns a problem for multiple timer times out of order when a single timer" <|
                \() ->
                    identifyProblems (Single (TimerFile "timer1.txt" "Name1") [ 1000, 1200, 1080, 3784, 3696 ]) BarcodeScanner.empty noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | timerTimesOutOfOrder =
                                    [ TimerTimesOutOfOrder TimerOne "20:00" "18:00"
                                    , TimerTimesOutOfOrder TimerOne "01:03:04" "01:01:36"
                                    ]
                            }
            , test "identifyProblems returns a problem for multiple timer times out of order on timer 1 of 2" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1200, 1080, 3784, 3696 ]
                            , times2 = [ 1000, 1080, 1200, 3696, 3784 ]
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows = []
                            , matchSummary = TimerMatchSummary 0 0 0 0 0
                            }
                        )
                        BarcodeScanner.empty
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | timerTimesOutOfOrder =
                                    [ TimerTimesOutOfOrder TimerOne "20:00" "18:00"
                                    , TimerTimesOutOfOrder TimerOne "01:03:04" "01:01:36"
                                    ]
                            }
            , test "identifyProblems returns a problem for multiple timer times out of order on timer 2 of 2" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1200, 3696, 3784 ]
                            , times2 = [ 1000, 1200, 1080, 3784, 3696 ]
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows = []
                            , matchSummary = TimerMatchSummary 0 0 0 0 0
                            }
                        )
                        BarcodeScanner.empty
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | timerTimesOutOfOrder =
                                    [ TimerTimesOutOfOrder TimerTwo "20:00" "18:00"
                                    , TimerTimesOutOfOrder TimerTwo "01:03:04" "01:01:36"
                                    ]
                            }
            , test "identifyProblems returns a problem for multiple timer times out of order on both timers" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1200, 3784, 3696 ]
                            , times2 = [ 1000, 1200, 1080, 3784, 3696 ]
                            , file1 = TimerFile "timer1.txt" "Name1"
                            , file2 = TimerFile "timer2.txt" "Name2"
                            , mergedTableRows = []
                            , matchSummary = TimerMatchSummary 0 0 0 0 0
                            }
                        )
                        BarcodeScanner.empty
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | timerTimesOutOfOrder =
                                    [ TimerTimesOutOfOrder TimerOne "01:03:04" "01:01:36"
                                    , TimerTimesOutOfOrder TimerTwo "20:00" "18:00"
                                    , TimerTimesOutOfOrder TimerTwo "01:03:04" "01:01:36"
                                    ]
                            }
            ]
        ]
