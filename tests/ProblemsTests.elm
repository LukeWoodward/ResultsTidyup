module ProblemsTests exposing (suite)

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        , MisScannedItem
        , UnrecognisedLine
        )
import BarcodeScannerTests exposing (createBarcodeScannerData)
import DataEntry exposing (DateEntry, IntegerEntry, emptyEntry)
import Dict exposing (Dict)
import Errors exposing (expectError)
import EventDateAndTime exposing (EventDateAndTime)
import Expect
import Problems
    exposing
        ( AthleteAndPositionPair
        , AthleteWithMultiplePositionsProblem
        , BarcodesScannedBeforeEventStartProblem
        , BarcodesScannedTheWrongWayAroundProblem
        , PositionAndTime
        , PositionOffEndOfTimesProblem
        , PositionWithMultipleAthletesProblem
        , Problems
        , identifyProblems
        , noIgnoredProblems
        , noProblems
        )
import Test exposing (Test, describe, test)
import TestData exposing (createBarcodeScannerDataFromFiles, doubleTimers, misScanFileLine, ordinaryFileLine, toPosix)
import Timer exposing (MergeEntry(..), MergedTableRow, TimerMatchSummary, Timers(..), WhichTimer(..), noUnderlines)


emptyEventDateAndTime : EventDateAndTime
emptyEventDateAndTime =
    EventDateAndTime emptyEntry emptyEntry


exampleEventDateAndTime : EventDateAndTime
exampleEventDateAndTime =
    EventDateAndTime (DateEntry "14/03/2018" (toPosix "2018-03-14T00:00:00.000Z")) (IntegerEntry "09:00" (Just (9 * 60)))


lateEventDateAndTime : EventDateAndTime
lateEventDateAndTime =
    EventDateAndTime (DateEntry "14/03/2018" (toPosix "2018-03-14T00:00:00.000Z")) (IntegerEntry "10:00" (Just (10 * 60)))


{-| 2018-03-14T09:00:00
-}
baseEventStartTime : Int
baseEventStartTime =
    1521018000000


wrapMergeEntriesInTable : List MergeEntry -> List MergedTableRow
wrapMergeEntriesInTable entries =
    let
        wrapRow : Int -> MergeEntry -> MergedTableRow
        wrapRow index entry =
            MergedTableRow index (Just (index + 1)) entry True noUnderlines
    in
    List.indexedMap wrapRow entries


doubleTimersForTimeLookupTests : Timers
doubleTimersForTimeLookupTests =
    let
        expectedEntries : List MergedTableRow
        expectedEntries =
            [ { index = 0, rowNumber = Just 1, entry = ExactMatch 191, included = True, underlines = noUnderlines }
            , { index = 1, rowNumber = Just 2, entry = NotNearMatch 469 463, included = True, underlines = noUnderlines }
            , { index = 2, rowNumber = Nothing, entry = OneWatchOnly TimerOne 603, included = False, underlines = noUnderlines }
            , { index = 3, rowNumber = Just 3, entry = ExactMatch 746, included = True, underlines = noUnderlines }
            , { index = 4, rowNumber = Nothing, entry = OneWatchOnly TimerTwo 791, included = False, underlines = noUnderlines }
            , { index = 5, rowNumber = Just 4, entry = ExactMatch 882, included = True, underlines = noUnderlines }
            ]

        expectedMatchSummary : TimerMatchSummary
        expectedMatchSummary =
            { exactMatches = 3, nearMatches = 0, notNearMatches = 1, timer1Only = 1, timer2Only = 1 }
    in
    Double
        { times1 = [ 191, 469, 603, 746, 882 ]
        , times2 = [ 191, 463, 746, 791, 882 ]
        , filename1 = "timer1.txt"
        , filename2 = "timer2.txt"
        , mergedTableRows = expectedEntries
        , matchSummary = expectedMatchSummary
        }


suite : Test
suite =
    describe "Problems tests"
        [ describe "identifyProblems tests"
            [ test "identifyProblems returns no problems for no data" <|
                \() ->
                    identifyProblems None BarcodeScanner.empty emptyEventDateAndTime noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns no problems for a single athlete with a single position" <|
                \() ->
                    identifyProblems None (createBarcodeScannerData (Dict.singleton 12 [ "A123456" ]) [] []) emptyEventDateAndTime noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns no problems for three athletes with three different positions" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns no problem for two barcode scanner files with two equal last-scan dates" <|
                \() ->
                    let
                        barcodeScannerData : BarcodeScannerData
                        barcodeScannerData =
                            BarcodeScannerData
                                [ BarcodeScannerFile "barcodes1.txt" [] (toPosix "2018-03-14T09:47:03.000Z")
                                , BarcodeScannerFile "barcodes2.txt" [] (toPosix "2018-03-14T09:49:08.000Z")
                                ]
                                Dict.empty
                                []
                                []
                                []
                                []
                                Nothing
                    in
                    identifyProblems
                        None
                        barcodeScannerData
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns a problem for an athlete with two repeated positions" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (Single "timer1.txt" [ 604, 775, 802, 993, 1011, 1143, 1197 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 6, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (Single "timer1.txt" [ 604, 775, 802, 993, 1011 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 6, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (createBarcodeScannerData (Dict.fromList [ ( 1, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 2, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (createBarcodeScannerData (Dict.fromList [ ( 2, [ "A123456" ] ), ( 3, [ "A252525" ] ), ( 4, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (createBarcodeScannerData (Dict.fromList [ ( 1, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 8, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A123456" ] ), ( 19, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 12 Nothing, PositionAndTime 16 Nothing, PositionAndTime 19 Nothing ]
                                    ]
                            }
            , test "identifyProblems returns a problem for an athlete with three positions, two of which are the same" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A123456", "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesInSamePositionMultipleTimes = [ AthleteAndPositionPair "A123456" 16 ]
                                , athletesWithMultiplePositions =
                                    [ AthleteWithMultiplePositionsProblem "A123456" [ PositionAndTime 12 Nothing, PositionAndTime 16 Nothing ]
                                    ]
                            }
            , test "identifyProblems returns two problems for two athletes with repeated positions" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A123456" ] ), ( 25, [ "A252525" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsWithMultipleAthletes = [ PositionWithMultipleAthletesProblem 12 [ "A123456", "A252525" ] ] }
            , test "identifyProblems returns a problem for a position with three athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525", "A748159" ] ), ( 19, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsWithMultipleAthletes = [ PositionWithMultipleAthletesProblem 12 [ "A123456", "A252525", "A748159" ] ] }
            , test "identifyProblems returns a problem for a position with three athletes, two of which are the same" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525", "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesInSamePositionMultipleTimes = [ AthleteAndPositionPair "A252525" 12 ]
                                , positionsWithMultipleAthletes = [ PositionWithMultipleAthletesProblem 12 [ "A123456", "A252525" ] ]
                            }
            , test "identifyProblems returns two problems for two positions with two athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525" ] ), ( 19, [ "A987654", "A748159" ] ) ]) [] [])
                        emptyEventDateAndTime
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
                        (Single "filename" [ 1000, 1100, 1200, 1300, 1400 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns a problem for a finish position off the end" <|
                \() ->
                    identifyProblems
                        (Single "filename" [ 1000, 1100, 1200, 1300 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionOffEndOfTimes = Just (PositionOffEndOfTimesProblem 4 5) }
            , test "identifyProblems returns a single problem for multiple finish positions off the end" <|
                \() ->
                    identifyProblems
                        (Single "filename" [ 1000, 1100 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionOffEndOfTimes = Just (PositionOffEndOfTimesProblem 2 5) }
            , test "identifyProblems returns a single problem for an athlete with no position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A951623" ] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesMissingPosition = [ "A951623" ] }
            , test "identifyProblems returns multiple problems for multiple athletes with no position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A321456", "A951623" ] [])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesMissingPosition = [ "A321456", "A951623" ] }
            , test "identifyProblems returns a single problem for a position with no athlete" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [ 15 ])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsMissingAthlete = [ 15 ] }
            , test "identifyProblems returns multiple problems for positions with no athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [ 15, 18, 26 ])
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsMissingAthlete = [ 15, 18, 26 ] }
            , test "identifyProblems returns a problem for a mis-scanned item" <|
                \() ->
                    identifyProblems
                        None
                        (BarcodeScannerData [] Dict.empty [] [] [ MisScannedItem "&d084" "14/03/2018 09:47:03" ] [] Nothing)
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | misScans = [ "&d084" ] }
            , test "identifyProblems returns a problem for an unrecognised barcode-scanner line" <|
                \() ->
                    identifyProblems
                        None
                        (BarcodeScannerData [] Dict.empty [] [] [] [ UnrecognisedLine "This is not a valid line" "code" "message" ] Nothing)
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | unrecognisedBarcodeScannerLines = [ "This is not a valid line" ] }
            , test "identifyProblems returns a fixable problem for the same athlete with the same finish position twice" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525", "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            []
                            []
                        )
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesInSamePositionMultipleTimes = [ AthleteAndPositionPair "A252525" 16 ] }
            , test "identifyProblems returns two fixable problems for two athletes with the same finish position twice" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525", "A252525" ] ), ( 19, [ "A987654", "A987654" ] ) ])
                            []
                            []
                        )
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesInSamePositionMultipleTimes = [ AthleteAndPositionPair "A252525" 16, AthleteAndPositionPair "A987654" 19 ] }
            , test "identifyProblems returns a fixable problem for an athlete with a position and with a missing position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            [ "A252525" ]
                            []
                        )
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesWithAndWithoutPosition = [ AthleteAndPositionPair "A252525" 16 ] }
            , test "identifyProblems returns two fixable problems for two athletes with and without a position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            [ "A252525", "A987654" ]
                            []
                        )
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | athletesWithAndWithoutPosition = [ AthleteAndPositionPair "A252525" 16, AthleteAndPositionPair "A987654" 19 ] }
            , test "identifyProblems returns a fixable problem for a position with and without an athlete" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            []
                            [ 19 ]
                        )
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsWithAndWithoutAthlete = [ AthleteAndPositionPair "A987654" 19 ] }
            , test "identifyProblems returns two fixable problems for two positions with and without athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            []
                            [ 19, 12 ]
                        )
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsWithAndWithoutAthlete = [ AthleteAndPositionPair "A987654" 19, AthleteAndPositionPair "A123456" 12 ] }
            , test "identifyProblems returns no problems for a scanned barcode with its scan time after the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
                                (toPosix "2018-03-14T09:47:03.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns a wrong-way-around problem for a sequence of barcodes apparently scanned the wrong way around" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                , ordinaryFileLine 2 "" (Just 52) "14/03/2018 09:48:01"
                                , ordinaryFileLine 3 "A4922730" (Just 59) "14/03/2018 09:48:42"
                                , ordinaryFileLine 4 "A4109106" Nothing "14/03/2018 09:48:53"
                                , ordinaryFileLine 5 "A4442284" (Just 64) "14/03/2018 09:49:16"
                                ]
                                (toPosix "2018-03-14T09:49:16.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | athletesMissingPosition = [ "A4109106" ]
                                , positionsMissingAthlete = [ 52 ]
                                , barcodesScannedTheWrongWayAround = [ BarcodesScannedTheWrongWayAroundProblem "barcodes1.txt" 2 4 ]
                            }
            , test "identifyProblems does not return a wrong-way-around problem for a finish token scanned on its own and no athletes barcodes scanned on their own" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                , ordinaryFileLine 2 "" (Just 52) "14/03/2018 09:48:01"
                                , ordinaryFileLine 3 "A4922730" (Just 59) "14/03/2018 09:48:42"
                                , ordinaryFileLine 4 "A4109106" (Just 62) "14/03/2018 09:48:53"
                                , ordinaryFileLine 5 "A4442284" (Just 64) "14/03/2018 09:49:16"
                                ]
                                (toPosix "2018-03-14T09:49:16.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsMissingAthlete = [ 52 ] }
            , test "identifyProblems does not return a wrong-way-around problem for a sequence of barcodes apparently scanned the wrong way around but with a mis-scan" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                , ordinaryFileLine 2 "" (Just 52) "14/03/2018 09:48:01"
                                , ordinaryFileLine 3 "A4922730" (Just 59) "14/03/2018 09:48:42"
                                , misScanFileLine 4 "Some nonsense" "14/03/2018 09:48:53"
                                , ordinaryFileLine 5 "A4442284" (Just 64) "14/03/2018 09:49:16"
                                ]
                                (toPosix "2018-03-14T09:49:16.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | positionsMissingAthlete = [ 52 ]
                                , misScans = [ "Some nonsense" ]
                            }
            , test "identifyProblems does not return a wrong-way-around problem for a sequence of barcodes apparently scanned the wrong way around but with a completely blank row" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                , ordinaryFileLine 2 "" (Just 52) "14/03/2018 09:48:01"
                                , ordinaryFileLine 3 "A4922730" (Just 59) "14/03/2018 09:48:42"
                                , ordinaryFileLine 4 "" Nothing "14/03/2018 09:48:53"
                                , ordinaryFileLine 5 "A4442284" (Just 64) "14/03/2018 09:49:16"
                                ]
                                (toPosix "2018-03-14T09:49:16.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | positionsMissingAthlete = [ 52 ] }
            , test "identifyProblems returns a wrong-way-around problem for a sequence of barcodes apparently scanned the wrong way around and ending with a rescan of a token" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                , ordinaryFileLine 2 "" (Just 52) "14/03/2018 09:48:01"
                                , ordinaryFileLine 3 "A4922730" (Just 59) "14/03/2018 09:48:42"
                                , ordinaryFileLine 4 "A4109106" (Just 59) "14/03/2018 09:48:53"
                                , ordinaryFileLine 5 "A4442284" (Just 64) "14/03/2018 09:49:16"
                                ]
                                (toPosix "2018-03-14T09:49:16.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | positionsMissingAthlete = [ 52 ]
                                , positionsWithMultipleAthletes = [ PositionWithMultipleAthletesProblem 59 [ "A4109106", "A4922730" ] ]
                                , barcodesScannedTheWrongWayAround = [ BarcodesScannedTheWrongWayAroundProblem "barcodes1.txt" 2 3 ]
                            }
            , test "identifyProblems returns a wrong-way-around problem for a sequence of barcodes including an unscannable athlete barcode" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                , ordinaryFileLine 2 "" (Just 52) "14/03/2018 09:48:01"
                                , ordinaryFileLine 3 "A4922730" (Just 59) "14/03/2018 09:48:42"
                                , ordinaryFileLine 4 "" (Just 62) "14/03/2018 09:48:53"
                                , ordinaryFileLine 5 "A4442284" (Just 64) "14/03/2018 09:49:16"
                                , ordinaryFileLine 6 "A4760002" Nothing "14/03/2018 09:49:20"
                                ]
                                (toPosix "2018-03-14T09:49:16.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | positionsMissingAthlete = [ 52, 62 ]
                                , athletesMissingPosition = [ "A4760002" ]
                                , barcodesScannedTheWrongWayAround = [ BarcodesScannedTheWrongWayAroundProblem "barcodes1.txt" 2 6 ]
                            }
            , test "identifyProblems returns a fixable problem for a scanned barcode with its scan time before the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
                                (toPosix "2018-03-14T09:47:03.000Z")
                            ]
                        )
                        lateEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | barcodesScannedBeforeEventStart = Just (BarcodesScannedBeforeEventStartProblem 1 (baseEventStartTime + 60 * 60 * 1000) "14/03/2018 10:00") }
            , test "identifyProblems returns no problems for a deleted scanned barcode with its scan time before the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ BarcodeScannerFileLine 1 (Ordinary "A4580442" (Just 47)) "14/03/2018 09:47:03" (Deleted BeforeEventStart) ]
                                (toPosix "2018-03-14T09:47:03.000Z")
                            ]
                        )
                        lateEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns no problems for a barcode-scanner file with all scan times within an hour before the event start but all rows deleted" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ BarcodeScannerFileLine 1 (Ordinary "A4580442" (Just 47)) "14/03/2018 08:47:03" (Deleted BeforeEventStart)
                                , BarcodeScannerFileLine 2 (Ordinary "A123456" (Just 33)) "14/03/2018 08:37:22" (Deleted BeforeEventStart)
                                , BarcodeScannerFileLine 3 (Ordinary "A252525" (Just 59)) "14/03/2018 08:42:08" (Deleted BeforeEventStart)
                                ]
                                (toPosix "2018-03-14T08:47:03.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns only an individual problem for a barcode-scanner file with half the scan times within an hour before the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerDataFromFiles
                            [ BarcodeScannerFile "barcodes1.txt"
                                [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 08:47:03"
                                , ordinaryFileLine 2 "A123456" (Just 33) "14/03/2018 08:37:22"
                                , ordinaryFileLine 3 "A252525" (Just 59) "14/03/2018 09:42:08"
                                , ordinaryFileLine 4 "A987654" (Just 42) "14/03/2018 09:44:07"
                                ]
                                (toPosix "2018-03-14T09:44:07.000Z")
                            ]
                        )
                        exampleEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal
                            { noProblems
                                | barcodesScannedBeforeEventStart = Just (BarcodesScannedBeforeEventStartProblem 2 baseEventStartTime "14/03/2018 09:00")
                            }
            , test "identifyProblems returns a problem for timers exactly in sync" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1200 ]
                            , times2 = [ 1000, 1080, 1200 ]
                            , filename1 = "timers1.txt"
                            , filename2 = "timers2.txt"
                            , mergedTableRows = wrapMergeEntriesInTable [ ExactMatch 1000, ExactMatch 1100, ExactMatch 1200 ]
                            , matchSummary = TimerMatchSummary 3 0 0 0 0
                            }
                        )
                        BarcodeScanner.empty
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | identicalTimerTimes = True }
            , test "identifyProblems returns no problems for timers almost in sync" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1201 ]
                            , times2 = [ 1000, 1080, 1200 ]
                            , filename1 = "timers1.txt"
                            , filename2 = "timers2.txt"
                            , mergedTableRows = wrapMergeEntriesInTable [ ExactMatch 1000, ExactMatch 1100, NearMatch 1201 1200 ]
                            , matchSummary = TimerMatchSummary 2 1 0 0 0
                            }
                        )
                        BarcodeScanner.empty
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal noProblems
            , test "identifyProblems returns a problem for timers not in sync" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1200 ]
                            , times2 = [ 1005, 1085, 1205 ]
                            , filename1 = "timers1.txt"
                            , filename2 = "timers2.txt"
                            , mergedTableRows =
                                wrapMergeEntriesInTable
                                    [ OneWatchOnly TimerOne 1000
                                    , OneWatchOnly TimerTwo 1005
                                    , OneWatchOnly TimerOne 1100
                                    , OneWatchOnly TimerTwo 1105
                                    , OneWatchOnly TimerOne 1200
                                    , OneWatchOnly TimerTwo 1205
                                    ]
                            , matchSummary = TimerMatchSummary 0 0 0 3 3
                            }
                        )
                        BarcodeScanner.empty
                        emptyEventDateAndTime
                        noIgnoredProblems
                        |> Expect.equal { noProblems | timerTimeOffset = Just -5 }
            , test "identifyProblems returns no problems for timers not in sync if problem ignored" <|
                \() ->
                    identifyProblems
                        (Double
                            { times1 = [ 1000, 1080, 1200 ]
                            , times2 = [ 1005, 1085, 1205 ]
                            , filename1 = "timers1.txt"
                            , filename2 = "timers2.txt"
                            , mergedTableRows =
                                wrapMergeEntriesInTable
                                    [ OneWatchOnly TimerOne 1000
                                    , OneWatchOnly TimerTwo 1005
                                    , OneWatchOnly TimerOne 1100
                                    , OneWatchOnly TimerTwo 1105
                                    , OneWatchOnly TimerOne 1200
                                    , OneWatchOnly TimerTwo 1205
                                    ]
                            , matchSummary = TimerMatchSummary 0 0 0 3 3
                            }
                        )
                        BarcodeScanner.empty
                        emptyEventDateAndTime
                        { noIgnoredProblems | ignoreTimerTimeOffsets = True }
                        |> Expect.equal noProblems
            ]
        ]
