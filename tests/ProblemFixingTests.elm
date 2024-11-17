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
        , empty
        , regenerate
        )
import BarcodeScannerTests exposing (createBarcodeScannerData)
import Dict
import Expect exposing (Expectation)
import Model exposing (Model, initModel)
import ProblemFixing exposing (ProblemFix(..), ProblemIgnorance(..), fixProblem, ignoreProblem)
import Problems exposing (IgnoredProblems, noIgnoredProblems)
import Test exposing (Test, describe, test)
import TestData exposing (defaultDateTime, doubleTimers, ordinaryFileLine, timersForAdjusting)
import Timer exposing (WhichTimer(..))


createBarcodeScannerDataForRemovingUnassociatedAthletes : List String -> BarcodeScannerData
createBarcodeScannerDataForRemovingUnassociatedAthletes athletes =
    let
        fileLines : List BarcodeScannerFileLine
        fileLines =
            athletes
                |> List.indexedMap
                    (\index athlete ->
                        [ BarcodeScannerFileLine (index * 2 + 1) (Ordinary athlete (Just (index + 1))) "14/03/2018 09:47:03" NotDeleted
                        , BarcodeScannerFileLine (index * 2 + 2) (Ordinary athlete Nothing) "14/03/2018 09:47:03" NotDeleted
                        ]
                    )
                |> List.concat
    in
    regenerate { empty | files = [ BarcodeScannerFile "barcodes1.txt" "Name" fileLines Nothing ] }


createBarcodeScannerDataForRemovingDuplicateScans : Int -> BarcodeScannerData
createBarcodeScannerDataForRemovingDuplicateScans numberOfTimes =
    let
        fileLines : List BarcodeScannerFileLine
        fileLines =
            List.range 1 numberOfTimes
                |> List.map
                    (\index ->
                        BarcodeScannerFileLine index (Ordinary "A1234" (Just 27)) "14/03/2018 09:47:03" NotDeleted
                    )
    in
    regenerate { empty | files = [ BarcodeScannerFile "barcodes1.txt" "Name" fileLines defaultDateTime ] }


deleteLinesWithinFile : (BarcodeScannerFileLine -> BarcodeScannerFileLine) -> List BarcodeScannerFile -> List BarcodeScannerFile
deleteLinesWithinFile deleter files =
    let
        deleteInFile : BarcodeScannerFile -> BarcodeScannerFile
        deleteInFile file =
            { file | lines = List.map deleter file.lines }
    in
    List.map deleteInFile files


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


ifLineNumberGreaterThanOne : BarcodeScannerFileLine -> BarcodeScannerFileLine
ifLineNumberGreaterThanOne line =
    if line.lineNumber > 1 then
        { line | deletionStatus = Deleted (DuplicateScan "A1234" 27) }

    else
        line


runTestForFixingProblem : ProblemFix -> BarcodeScannerData -> BarcodeScannerData -> Expectation
runTestForFixingProblem problemFix initialBarcodeScannerData expectedBarcodeScannerData =
    let
        initialModel : Model
        initialModel =
            { initModel | barcodeScannerData = initialBarcodeScannerData }

        actualModel : Model
        actualModel =
            fixProblem problemFix initialModel
    in
    Expect.equal
        { initialModel | barcodeScannerData = expectedBarcodeScannerData }
        actualModel


suite : Test
suite =
    describe "ProblemFixing tests"
        [ describe "Removing unassociated athlete tests"
            [ test "Can remove unassociated athlete" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataForRemovingUnassociatedAthletes [ "A1234", "A3456", "A5678", "A9012" ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            { initialBarcodeScannerData
                                | athleteBarcodesOnly = List.filter (\x -> x.athlete /= "A5678") initialBarcodeScannerData.athleteBarcodesOnly
                                , files = deleteLinesWithinFile (ifAthlete "A5678") initialBarcodeScannerData.files
                            }
                    in
                    runTestForFixingProblem (RemoveUnassociatedAthlete "A5678") initialBarcodeScannerData expectedBarcodeScannerData
            , test "Can remove unassociated athlete if they occur multiple times" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataForRemovingUnassociatedAthletes [ "A1234", "A5678", "A3456", "A5678", "A5678", "A9012", "A5678" ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            { initialBarcodeScannerData
                                | athleteBarcodesOnly = List.filter (\x -> x.athlete /= "A5678") initialBarcodeScannerData.athleteBarcodesOnly
                                , files = deleteLinesWithinFile (ifAthlete "A5678") initialBarcodeScannerData.files
                            }
                    in
                    runTestForFixingProblem (RemoveUnassociatedAthlete "A5678") initialBarcodeScannerData expectedBarcodeScannerData
            , test "Removing unassociated athlete when it never occurs has no effect" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataForRemovingUnassociatedAthletes [ "A1234", "A3456", "A5678", "A9012" ]
                    in
                    runTestForFixingProblem (RemoveUnassociatedAthlete "A9090") initialBarcodeScannerData initialBarcodeScannerData
            ]
        , describe "Removing duplicate scans test"
            [ test "Can remove scan that occurs twice" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataForRemovingDuplicateScans 2

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            { initialBarcodeScannerData
                                | scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A1234" "14/03/2018 09:47:03" ]
                                , files = deleteLinesWithinFile ifLineNumberGreaterThanOne initialBarcodeScannerData.files
                            }
                    in
                    runTestForFixingProblem (RemoveDuplicateScans 27 "A1234") initialBarcodeScannerData expectedBarcodeScannerData
            , test "Can remove scan that occurs more than twice" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataForRemovingDuplicateScans 5

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            { initialBarcodeScannerData
                                | scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A1234" "14/03/2018 09:47:03" ]
                                , files = deleteLinesWithinFile ifLineNumberGreaterThanOne initialBarcodeScannerData.files
                            }
                    in
                    runTestForFixingProblem (RemoveDuplicateScans 27 "A1234") initialBarcodeScannerData expectedBarcodeScannerData
            , test "Can remove scan that occurs twice in two different files" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerData (Dict.singleton 27 [ "A1234", "A1234" ]) []

                        startingBarcodeScannerData : BarcodeScannerData
                        startingBarcodeScannerData =
                            { initialBarcodeScannerData
                                | files =
                                    [ BarcodeScannerFile "barcodes1.txt" "Name1" [ ordinaryFileLine 1 "A1234" (Just 27) "14/03/2018 09:47:03" ] defaultDateTime
                                    , BarcodeScannerFile "barcodes2.txt" "Name2" [ ordinaryFileLine 1 "A1234" (Just 27) "14/03/2018 09:47:03" ] defaultDateTime
                                    ]
                                , lastScanDateTime = defaultDateTime
                            }

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            { initialBarcodeScannerData
                                | scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A1234" "14/03/2018 09:47:03" ]
                                , files =
                                    [ BarcodeScannerFile
                                        "barcodes1.txt"
                                        "Name1"
                                        [ ordinaryFileLine 1 "A1234" (Just 27) "14/03/2018 09:47:03" ]
                                        defaultDateTime
                                    , BarcodeScannerFile
                                        "barcodes2.txt"
                                        "Name2"
                                        [ BarcodeScannerFileLine 1 (Ordinary "A1234" (Just 27)) "14/03/2018 09:47:03" (Deleted (DuplicateScan "A1234" 27)) ]
                                        defaultDateTime
                                    ]
                                , lastScanDateTime = defaultDateTime
                            }
                    in
                    runTestForFixingProblem (RemoveDuplicateScans 27 "A1234") startingBarcodeScannerData expectedBarcodeScannerData
            , test "Attempting to remove duplicate scan when not duplicate does nothing" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataForRemovingDuplicateScans 1
                    in
                    runTestForFixingProblem (RemoveDuplicateScans 27 "A1234") initialBarcodeScannerData initialBarcodeScannerData
            , test "Attempting to remove duplicate scan when finish position wrong does nothing" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataForRemovingDuplicateScans 2
                    in
                    runTestForFixingProblem (RemoveDuplicateScans 25 "A1234") initialBarcodeScannerData initialBarcodeScannerData
            , test "Attempting to remove duplicate scan when athlete wrong does nothing" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataForRemovingDuplicateScans 2
                    in
                    runTestForFixingProblem (RemoveDuplicateScans 27 "A9999") initialBarcodeScannerData initialBarcodeScannerData
            , test "Attempting to remove duplicate scan when other athletes in same position does nothing" <|
                \() ->
                    let
                        file : BarcodeScannerFile
                        file =
                            BarcodeScannerFile
                                "barcodes1.txt"
                                "Name1"
                                [ ordinaryFileLine 1 "A1234" (Just 27) "14/03/2018 09:47:03"
                                , ordinaryFileLine 2 "A5678" (Just 27) "14/03/2018 09:47:03"
                                , ordinaryFileLine 3 "A3456" (Just 27) "14/03/2018 09:47:03"
                                , ordinaryFileLine 4 "A9012" (Just 27) "14/03/2018 09:47:03"
                                ]
                                defaultDateTime

                        barcodeScannerData : BarcodeScannerData
                        barcodeScannerData =
                            createBarcodeScannerData (Dict.singleton 27 [ "A1234", "A5678", "A3456", "A9012" ]) []

                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            { barcodeScannerData | files = [ file ], lastScanDateTime = defaultDateTime }
                    in
                    runTestForFixingProblem (RemoveDuplicateScans 27 "A1234") initialBarcodeScannerData initialBarcodeScannerData
            ]
        , describe "AdjustTimer tests"
            [ test "Can adjust timer 1 adding some time to it" <|
                \() ->
                    let
                        initialModel : Model
                        initialModel =
                            { initModel | timers = timersForAdjusting -19 0 }

                        actualModel : Model
                        actualModel =
                            fixProblem (AdjustTimer TimerOne 19) initialModel
                    in
                    Expect.equal
                        { initialModel | timers = doubleTimers }
                        actualModel
            , test "Can adjust timer 1 taking some time from it" <|
                \() ->
                    let
                        initialModel : Model
                        initialModel =
                            { initModel | timers = timersForAdjusting 46 0 }

                        actualModel : Model
                        actualModel =
                            fixProblem (AdjustTimer TimerOne -46) initialModel
                    in
                    Expect.equal
                        { initialModel | timers = doubleTimers }
                        actualModel
            , test "Can adjust timer 2 adding some time to it" <|
                \() ->
                    let
                        initialModel : Model
                        initialModel =
                            { initModel | timers = timersForAdjusting 0 -22 }

                        actualModel : Model
                        actualModel =
                            fixProblem (AdjustTimer TimerTwo 22) initialModel
                    in
                    Expect.equal
                        { initialModel | timers = doubleTimers }
                        actualModel
            , test "Can adjust timer 2 taking some time from it" <|
                \() ->
                    let
                        initialModel : Model
                        initialModel =
                            { initModel | timers = timersForAdjusting 0 37 }

                        actualModel : Model
                        actualModel =
                            fixProblem (AdjustTimer TimerTwo -37) initialModel
                    in
                    Expect.equal
                        { initialModel | timers = doubleTimers }
                        actualModel
            ]
        , describe "ignoreProblem tests"
            [ test "Ignoring timer time offsets marks the timer offsets as ignored" <|
                \() ->
                    ignoreProblem IgnoreTimerTimeOffsets noIgnoredProblems
                        |> Expect.equal { noIgnoredProblems | ignoreTimerTimeOffsets = True }
            , test "Ignoring timer time offsets when already ignored does nothing" <|
                \() ->
                    let
                        ignoredProblems : IgnoredProblems
                        ignoredProblems =
                            { noIgnoredProblems | ignoreTimerTimeOffsets = True }
                    in
                    ignoreProblem IgnoreTimerTimeOffsets ignoredProblems
                        |> Expect.equal ignoredProblems
            ]
        ]
