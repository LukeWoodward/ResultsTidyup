module ProblemFixingTests exposing (suite)

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        , PositionAndTimePair
        , WrongWayAroundStatus(..)
        , empty
        , regenerate
        )
import BarcodeScannerTests exposing (createBarcodeScannerData)
import DataStructures exposing (ProblemFix(..), WhichStopwatch(..))
import Dict
import Expect exposing (Expectation)
import MergedTable exposing (Stopwatches)
import Model exposing (Model, NumberCheckerManualEntryRow, ProblemEntry, emptyNumberCheckerManualEntryRow, initModel)
import ProblemFixing exposing (fixProblem)
import Problems exposing (FixableProblem(..), NonFixableProblem(..), Problem(..))
import Test exposing (Test, describe, test)
import TestData exposing (defaultTime, doubleStopwatches, ordinaryFileLine, stopwatchesForAdjusting)


createBarcodeScannerDataForRemovingUnassociatedFinishTokens : List Int -> Model
createBarcodeScannerDataForRemovingUnassociatedFinishTokens finishTokens =
    let
        fakeAthlete : Int -> String
        fakeAthlete index =
            "A" ++ String.fromInt (index + 1)

        fileLines : List BarcodeScannerFileLine
        fileLines =
            finishTokens
                |> List.indexedMap
                    (\index token ->
                        [ ordinaryFileLine (index * 2 + 1) (fakeAthlete index) (Just token) "14/03/2018 09:47:03"
                        , ordinaryFileLine (index * 2 + 2) "" (Just token) "14/03/2018 09:47:03"
                        ]
                    )
                |> List.concat
    in
    { initModel
        | barcodeScannerData = regenerate { empty | files = [ BarcodeScannerFile "barcodes1.txt" fileLines Nothing ] }
        , problems = List.indexedMap (\index position -> ProblemEntry (Fixable (PositionWithAndWithoutAthlete position (fakeAthlete index))) index False) finishTokens
    }


createBarcodeScannerDataForRemovingUnassociatedAthletes : List String -> Model
createBarcodeScannerDataForRemovingUnassociatedAthletes athletes =
    let
        fileLines : List BarcodeScannerFileLine
        fileLines =
            athletes
                |> List.indexedMap
                    (\index athlete ->
                        [ BarcodeScannerFileLine (index * 2 + 1) (Ordinary athlete (Just (index + 1))) "14/03/2018 09:47:03" NotDeleted NotWrongWayAround
                        , BarcodeScannerFileLine (index * 2 + 2) (Ordinary athlete Nothing) "14/03/2018 09:47:03" NotDeleted NotWrongWayAround
                        ]
                    )
                |> List.concat
    in
    { initModel
        | barcodeScannerData = regenerate { empty | files = [ BarcodeScannerFile "barcodes1.txt" fileLines Nothing ] }
        , problems = List.indexedMap (\index athlete -> ProblemEntry (Fixable (AthleteWithAndWithoutPosition athlete (index + 1))) index False) athletes
    }


createBarcodeScannerDataForRemovingDuplicateScans : Int -> Model
createBarcodeScannerDataForRemovingDuplicateScans numberOfTimes =
    let
        barcodeScannerData : BarcodeScannerData
        barcodeScannerData =
            createBarcodeScannerData (Dict.singleton 27 (List.repeat numberOfTimes "A1234")) [] []

        fileLines : List BarcodeScannerFileLine
        fileLines =
            List.range 1 numberOfTimes
                |> List.map
                    (\index ->
                        BarcodeScannerFileLine index (Ordinary "A1234" (Just 27)) "14/03/2018 09:47:03" NotDeleted NotWrongWayAround
                    )
    in
    { initModel
        | barcodeScannerData = regenerate { empty | files = [ BarcodeScannerFile "barcodes1.txt" fileLines defaultTime ] }
        , problems =
            if numberOfTimes > 1 then
                [ ProblemEntry (Fixable (AthleteInSamePositionMultipleTimes "A1234" 27)) 0 False ]

            else
                []
    }


barcodeScannerDataForEventStartTimeFiltering : BarcodeScannerData
barcodeScannerDataForEventStartTimeFiltering =
    { files =
        [ BarcodeScannerFile
            "barcodes1.txt"
            [ ordinaryFileLine 1 "A123456" (Just 27) "14/03/2018 09:22:08"
            , ordinaryFileLine 2 "A345678" Nothing "14/03/2018 09:47:54"
            , ordinaryFileLine 3 "" (Just 19) "14/03/2018 10:11:16"
            ]
            Nothing
        ]
    , scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A123456" "14/03/2018 09:22:08" ]
    , athleteBarcodesOnly = [ AthleteAndTimePair "A345678" "14/03/2018 09:47:54" ]
    , finishTokensOnly = [ PositionAndTimePair 19 "14/03/2018 10:11:16" ]
    , misScannedItems = []
    , unrecognisedLines = []
    , lastScanDate = Nothing
    }


deleteLinesWithinFile : (BarcodeScannerFileLine -> BarcodeScannerFileLine) -> List BarcodeScannerFile -> List BarcodeScannerFile
deleteLinesWithinFile deleter files =
    let
        deleteInFile : BarcodeScannerFile -> BarcodeScannerFile
        deleteInFile file =
            { file | lines = List.map deleter file.lines }
    in
    List.map deleteInFile files


ifLineNumberIn : List Int -> BarcodeScannerFileLine -> BarcodeScannerFileLine
ifLineNumberIn linesToDelete line =
    if List.member line.lineNumber linesToDelete then
        { line | deletionStatus = Deleted BeforeEventStart }

    else
        line


ifAthlete : String -> BarcodeScannerFileLine -> BarcodeScannerFileLine
ifAthlete athlete line =
    case line.contents of
        Ordinary someAthlete Nothing ->
            if athlete == someAthlete then
                { line | deletionStatus = Deleted (AthleteScannedWithFinishTokenElsewhere athlete) }

            else
                line

        _ ->
            line


ifFinishPosition : Int -> BarcodeScannerFileLine -> BarcodeScannerFileLine
ifFinishPosition position line =
    case line.contents of
        Ordinary "" somePosition ->
            if somePosition == Just position then
                { line | deletionStatus = Deleted (FinishTokenScannedWithAthleteElsewhere position) }

            else
                line

        _ ->
            line


ifLineNumberGreaterThanOne : BarcodeScannerFileLine -> BarcodeScannerFileLine
ifLineNumberGreaterThanOne line =
    if line.lineNumber > 1 then
        { line | deletionStatus = Deleted (DuplicateScan "A1234" 27) }

    else
        line


makeProblemEntries : List Problem -> List ProblemEntry
makeProblemEntries problems =
    List.indexedMap (\index problem -> ProblemEntry problem index False) problems


{-| 2018-03-14T09:00:00
-}
baseEventStartTime : Int
baseEventStartTime =
    1521018000000


suite : Test
suite =
    describe "ProblemFixing tests"
        [ describe "Removing unassociated finish position tests"
            [ test "Can remove unassociated finish token" <|
                \() ->
                    let
                        initialModel : Model
                        initialModel =
                            createBarcodeScannerDataForRemovingUnassociatedFinishTokens [ 14, 18, 39, 44 ]

                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            initialModel.barcodeScannerData

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            regenerate
                                { initialBarcodeScannerData
                                    | finishTokensOnly = List.filter (\x -> x.position /= 39) initialBarcodeScannerData.finishTokensOnly
                                    , files = deleteLinesWithinFile (ifFinishPosition 39) initialModel.barcodeScannerData.files
                                }

                        actualModel : Model
                        actualModel =
                            fixProblem (RemoveUnassociatedFinishToken 39) initialModel
                    in
                    Expect.equal
                        { initialModel
                            | barcodeScannerData = expectedBarcodeScannerData
                            , problems = actualModel.problems
                        }
                        actualModel
            , test "Can remove unassociated finish token if it occurs multiple times" <|
                \() ->
                    let
                        initialModel : Model
                        initialModel =
                            createBarcodeScannerDataForRemovingUnassociatedFinishTokens [ 14, 39, 18, 39, 39, 44, 39 ]

                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            initialModel.barcodeScannerData

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            { initialBarcodeScannerData
                                | finishTokensOnly = List.filter (\x -> x.position /= 39) initialBarcodeScannerData.finishTokensOnly
                                , files = deleteLinesWithinFile (ifFinishPosition 39) initialModel.barcodeScannerData.files
                            }

                        actualModel : Model
                        actualModel =
                            fixProblem (RemoveUnassociatedFinishToken 39) initialModel
                    in
                    Expect.equal
                        { initialModel
                            | barcodeScannerData = expectedBarcodeScannerData
                            , problems = actualModel.problems
                        }
                        actualModel
            , test "Removing unassociated finish token when it never occurs has no effect" <|
                \() ->
                    let
                        initialModel : Model
                        initialModel =
                            createBarcodeScannerDataForRemovingUnassociatedFinishTokens [ 14, 18, 39, 44 ]

                        actualModel : Model
                        actualModel =
                            fixProblem (RemoveUnassociatedFinishToken 27) initialModel
                    in
                    Expect.equal initialModel actualModel
            , describe "Removing unassociated athlete tests"
                [ test "Can remove unassociated athlete" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingUnassociatedAthletes [ "A1234", "A3456", "A5678", "A9012" ]

                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                initialModel.barcodeScannerData

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { initialBarcodeScannerData
                                    | athleteBarcodesOnly = List.filter (\x -> x.athlete /= "A5678") initialBarcodeScannerData.athleteBarcodesOnly
                                    , files = deleteLinesWithinFile (ifAthlete "A5678") initialModel.barcodeScannerData.files
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveUnassociatedAthlete "A5678") initialModel
                        in
                        Expect.equal
                            { initialModel
                                | barcodeScannerData = expectedBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                , test "Can remove unassociated athlete if they occur multiple times" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingUnassociatedAthletes [ "A1234", "A5678", "A3456", "A5678", "A5678", "A9012", "A5678" ]

                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                initialModel.barcodeScannerData

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { initialBarcodeScannerData
                                    | athleteBarcodesOnly = List.filter (\x -> x.athlete /= "A5678") initialBarcodeScannerData.athleteBarcodesOnly
                                    , files = deleteLinesWithinFile (ifAthlete "A5678") initialModel.barcodeScannerData.files
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveUnassociatedAthlete "A5678") initialModel
                        in
                        Expect.equal
                            { initialModel
                                | barcodeScannerData = expectedBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                , test "Removing unassociated athlete when it never occurs has no effect" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingUnassociatedAthletes [ "A1234", "A3456", "A5678", "A9012" ]

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveUnassociatedAthlete "A9090") initialModel
                        in
                        Expect.equal initialModel actualModel
                ]
            , describe "Removing duplicate scans test"
                [ test "Can remove scan that occurs twice" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 2

                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                initialModel.barcodeScannerData

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { initialBarcodeScannerData
                                    | scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A1234" "14/03/2018 09:47:03" ]
                                    , files = deleteLinesWithinFile ifLineNumberGreaterThanOne initialBarcodeScannerData.files
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveDuplicateScans 27 "A1234") initialModel
                        in
                        Expect.equal
                            { initialModel
                                | barcodeScannerData = expectedBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                , test "Can remove scan that occurs more than twice" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 5

                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                initialModel.barcodeScannerData

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { initialBarcodeScannerData
                                    | scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A1234" "14/03/2018 09:47:03" ]
                                    , files = deleteLinesWithinFile ifLineNumberGreaterThanOne initialBarcodeScannerData.files
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveDuplicateScans 27 "A1234") initialModel
                        in
                        Expect.equal
                            { initialModel
                                | barcodeScannerData = expectedBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                , test "Can remove scan that occurs twice in two different files" <|
                    \() ->
                        let
                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                createBarcodeScannerData (Dict.singleton 27 [ "A1234", "A1234" ]) [] []

                            startingBarcodeScannerData : BarcodeScannerData
                            startingBarcodeScannerData =
                                { initialBarcodeScannerData
                                    | files =
                                        [ BarcodeScannerFile "barcodes1.txt" [ ordinaryFileLine 1 "A1234" (Just 27) "14/03/2018 09:47:03" ] defaultTime
                                        , BarcodeScannerFile "barcodes2.txt" [ ordinaryFileLine 1 "A1234" (Just 27) "14/03/2018 09:47:03" ] defaultTime
                                        ]
                                    , lastScanDate = defaultTime
                                }

                            finalBarcodeScannerData : BarcodeScannerData
                            finalBarcodeScannerData =
                                { initialBarcodeScannerData
                                    | scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A1234" "14/03/2018 09:47:03" ]
                                    , files =
                                        [ BarcodeScannerFile
                                            "barcodes1.txt"
                                            [ ordinaryFileLine 1 "A1234" (Just 27) "14/03/2018 09:47:03" ]
                                            defaultTime
                                        , BarcodeScannerFile
                                            "barcodes2.txt"
                                            [ BarcodeScannerFileLine 1 (Ordinary "A1234" (Just 27)) "14/03/2018 09:47:03" (Deleted (DuplicateScan "A1234" 27)) NotWrongWayAround ]
                                            defaultTime
                                        ]
                                    , lastScanDate = defaultTime
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem
                                    (RemoveDuplicateScans 27 "A1234")
                                    { initModel | barcodeScannerData = startingBarcodeScannerData }
                        in
                        Expect.equal
                            { initModel
                                | barcodeScannerData = finalBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                , test "Attempting to remove duplicate scan when not duplicate does nothing" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 1

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveDuplicateScans 27 "A1234") initialModel
                        in
                        Expect.equal initialModel actualModel
                , test "Attempting to remove duplicate scan when finish position wrong does nothing" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 2

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveDuplicateScans 25 "A1234") initialModel
                        in
                        Expect.equal initialModel actualModel
                , test "Attempting to remove duplicate scan when athlete wrong does nothing" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 2

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveDuplicateScans 27 "A9999") initialModel
                        in
                        Expect.equal initialModel actualModel
                , test "Attempting to remove duplicate scan when other athletes in same position does nothing" <|
                    \() ->
                        let
                            file : BarcodeScannerFile
                            file =
                                BarcodeScannerFile
                                    "barcodes1.txt"
                                    [ ordinaryFileLine 1 "A1234" (Just 27) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A5678" (Just 27) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 3 "A3456" (Just 27) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 4 "A9012" (Just 27) "14/03/2018 09:47:03"
                                    ]
                                    defaultTime

                            barcodeScannerData : BarcodeScannerData
                            barcodeScannerData =
                                createBarcodeScannerData (Dict.singleton 27 [ "A1234", "A5678", "A3456", "A9012" ]) [] []

                            barcodeScannerDataWithFiles : BarcodeScannerData
                            barcodeScannerDataWithFiles =
                                { barcodeScannerData | files = [ file ], lastScanDate = defaultTime }

                            initialModel : Model
                            initialModel =
                                { initModel | barcodeScannerData = barcodeScannerDataWithFiles }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveDuplicateScans 27 "A1234") initialModel
                        in
                        Expect.equal initialModel actualModel
                ]
            , describe "Removing scans before event start time"
                [ test "Removing scans before 9am clears nothing" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                { initModel | barcodeScannerData = barcodeScannerDataForEventStartTimeFiltering }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveScansBeforeEventStart baseEventStartTime) initialModel
                        in
                        Expect.equal initialModel actualModel
                , test "Removing scans before 9:30am clears genuine scan" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                { initModel | barcodeScannerData = barcodeScannerDataForEventStartTimeFiltering }

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { barcodeScannerDataForEventStartTimeFiltering
                                    | scannedBarcodes = Dict.empty
                                    , files = deleteLinesWithinFile (ifLineNumberIn [ 1 ]) barcodeScannerDataForEventStartTimeFiltering.files
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveScansBeforeEventStart (baseEventStartTime + 30 * 60 * 1000)) initialModel
                        in
                        Expect.equal
                            { initialModel
                                | barcodeScannerData = expectedBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                , test "Removing scans before 10:00am clears genuine scan and athlete-barcode only" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                { initModel | barcodeScannerData = barcodeScannerDataForEventStartTimeFiltering }

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { barcodeScannerDataForEventStartTimeFiltering
                                    | scannedBarcodes = Dict.empty
                                    , athleteBarcodesOnly = []
                                    , files = deleteLinesWithinFile (ifLineNumberIn [ 1, 2 ]) barcodeScannerDataForEventStartTimeFiltering.files
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveScansBeforeEventStart (baseEventStartTime + 60 * 60 * 1000)) initialModel
                        in
                        Expect.equal
                            { initialModel
                                | barcodeScannerData = expectedBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                , test "Removing scans before 10:30am clears everything" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                { initModel | barcodeScannerData = barcodeScannerDataForEventStartTimeFiltering }

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { barcodeScannerDataForEventStartTimeFiltering
                                    | scannedBarcodes = Dict.empty
                                    , athleteBarcodesOnly = []
                                    , finishTokensOnly = []
                                    , files = deleteLinesWithinFile (ifLineNumberIn [ 1, 2, 3 ]) barcodeScannerDataForEventStartTimeFiltering.files
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveScansBeforeEventStart (baseEventStartTime + 90 * 60 * 1000)) initialModel
                        in
                        Expect.equal
                            { initialModel
                                | barcodeScannerData = expectedBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                , test "A scan with an invalid time never gets removed" <|
                    \() ->
                        let
                            fileChanger : BarcodeScannerFile -> BarcodeScannerFile
                            fileChanger file =
                                case file.lines of
                                    first :: second :: rest ->
                                        { file
                                            | lines = first :: BarcodeScannerFileLine 2 (Ordinary "A345678" Nothing) "This is not a valid time" NotDeleted NotWrongWayAround :: rest
                                        }

                                    _ ->
                                        file

                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                { barcodeScannerDataForEventStartTimeFiltering
                                    | athleteBarcodesOnly = [ AthleteAndTimePair "A345678" "This is not a valid time" ]
                                    , files = List.map fileChanger barcodeScannerDataForEventStartTimeFiltering.files
                                }

                            initialModel : Model
                            initialModel =
                                { initModel | barcodeScannerData = initialBarcodeScannerData }

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { initialBarcodeScannerData
                                    | scannedBarcodes = Dict.empty
                                    , finishTokensOnly = []
                                    , files = deleteLinesWithinFile (ifLineNumberIn [ 1, 3 ]) initialBarcodeScannerData.files
                                }

                            actualModel : Model
                            actualModel =
                                fixProblem (RemoveScansBeforeEventStart (baseEventStartTime + 2 * 60 * 60 * 1000)) initialModel
                        in
                        Expect.equal
                            { initialModel
                                | barcodeScannerData = expectedBarcodeScannerData
                                , problems = actualModel.problems
                            }
                            actualModel
                ]
            , describe "AdjustStopwatch tests"
                [ test "Can adjust stopwatch 1 adding some time to it" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                { initModel | stopwatches = stopwatchesForAdjusting -19 0 }

                            actualModel : Model
                            actualModel =
                                fixProblem (AdjustStopwatch StopwatchOne 19) initialModel
                        in
                        Expect.equal
                            { initialModel | stopwatches = doubleStopwatches }
                            actualModel
                , test "Can adjust stopwatch 1 taking some time from it" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                { initModel | stopwatches = stopwatchesForAdjusting 46 0 }

                            actualModel : Model
                            actualModel =
                                fixProblem (AdjustStopwatch StopwatchOne -46) initialModel
                        in
                        Expect.equal
                            { initialModel | stopwatches = doubleStopwatches }
                            actualModel
                , test "Can adjust stopwatch 2 adding some time to it" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                { initModel | stopwatches = stopwatchesForAdjusting 0 -22 }

                            actualModel : Model
                            actualModel =
                                fixProblem (AdjustStopwatch StopwatchTwo 22) initialModel
                        in
                        Expect.equal
                            { initialModel | stopwatches = doubleStopwatches }
                            actualModel
                , test "Can adjust stopwatch 2 taking some time from it" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                { initModel | stopwatches = stopwatchesForAdjusting 0 37 }

                            actualModel : Model
                            actualModel =
                                fixProblem (AdjustStopwatch StopwatchTwo -37) initialModel
                        in
                        Expect.equal
                            { initialModel | stopwatches = doubleStopwatches }
                            actualModel
                ]
            ]
        ]
