module UpdateLogicTests exposing (suite)

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
import BarcodeScannerTests exposing (createBarcodeScannerData, expectSingleUnrecognisedLine)
import DataStructures exposing (EventDateAndTime, InteropFile, ProblemFix(..), SecondTab(..), WhichStopwatch(..))
import Dict exposing (Dict)
import Error exposing (FileError)
import Errors exposing (expectError)
import Expect exposing (Expectation)
import FileHandling exposing (crlf)
import MergedTable exposing (MergedTableRow, Stopwatches(..), noUnderlines)
import Merger exposing (MergeEntry(..))
import Model
    exposing
        ( Model
        , NumberCheckerManualEntryRow
        , ProblemEntry
        , emptyNumberCheckerManualEntryRow
        , initModel
        )
import Msg exposing (Msg(..), NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import NumericEntry exposing (NumericEntry, emptyNumericEntry)
import Problems exposing (FixableProblem(..), NonFixableProblem(..), Problem(..))
import Set
import Stopwatch exposing (Stopwatch(..))
import StopwatchTests
import Test exposing (Test, describe, test)
import TestData exposing (..)
import Time
import UpdateLogic exposing (createStopwatchFileForDownload, regenerateWithWrongWayArounds, update)


expectNoCommand : ( Model, Cmd Msg ) -> Expectation
expectNoCommand ( _, cmd ) =
    Expect.equal Cmd.none cmd


expectACommand : ( Model, Cmd Msg ) -> Expectation
expectACommand ( _, cmd ) =
    Expect.notEqual Cmd.none cmd


expectStopwatches : Stopwatches -> ( Model, Cmd Msg ) -> Expectation
expectStopwatches expectedStopwatches ( model, _ ) =
    Expect.equal expectedStopwatches model.stopwatches


expectLastError : String -> ( Model, Cmd Msg ) -> Expectation
expectLastError expectedCode ( model, _ ) =
    case model.lastErrors of
        [ singleError ] ->
            Expect.equal expectedCode singleError.code

        [] ->
            Expect.fail ("Expected to fail with error " ++ expectedCode ++ ", but no error was present")

        _ ->
            let
                codes : String
                codes =
                    List.map .code model.lastErrors
                        |> String.join ", "
            in
            Expect.fail ("Expected to fail with error " ++ expectedCode ++ ", but multiple errors were present: " ++ codes)


expectNumberCheckerEntries : List AnnotatedNumberCheckerEntry -> ( Model, Cmd Msg ) -> Expectation
expectNumberCheckerEntries expectedNumberCheckerEntries ( model, _ ) =
    Expect.equal expectedNumberCheckerEntries model.numberCheckerEntries


expectLastHeight : Maybe Int -> ( Model, Cmd Msg ) -> Expectation
expectLastHeight expectedLastHeight ( model, _ ) =
    Expect.equal expectedLastHeight model.lastHeight


expectHighlightedNumberCheckerId : Maybe Int -> ( Model, Cmd Msg ) -> Expectation
expectHighlightedNumberCheckerId expectedHighlightedNumberCheckerId ( model, _ ) =
    Expect.equal expectedHighlightedNumberCheckerId model.highlightedNumberCheckerId


expectBarcodeScannerData : BarcodeScannerData -> ( Model, Cmd Msg ) -> Expectation
expectBarcodeScannerData expectedBarcodeScannerData ( model, _ ) =
    Expect.equal expectedBarcodeScannerData model.barcodeScannerData


expectProblems : List Problem -> ( Model, Cmd Msg ) -> Expectation
expectProblems expectedProblems ( model, _ ) =
    let
        expectedProblemEntries : List ProblemEntry
        expectedProblemEntries =
            List.indexedMap (\index problem -> ProblemEntry problem index False) expectedProblems
    in
    Expect.equal expectedProblemEntries model.problems


expectProblemEntries : List ProblemEntry -> ( Model, Cmd Msg ) -> Expectation
expectProblemEntries expectedProblemEntries ( model, _ ) =
    Expect.equal expectedProblemEntries model.problems


expectEventDateAndTime : EventDateAndTime -> ( Model, Cmd Msg ) -> Expectation
expectEventDateAndTime expectedEventDateAndTime ( model, _ ) =
    Expect.equal expectedEventDateAndTime model.eventDateAndTime


expectNumberCheckerManualEntryRow : NumberCheckerManualEntryRow -> ( Model, Cmd Msg ) -> Expectation
expectNumberCheckerManualEntryRow expectedManualEntryRow ( model, _ ) =
    Expect.equal expectedManualEntryRow model.numberCheckerManualEntryRow


type Assertion
    = Command
    | Stopwatches
    | LastError
    | NumberCheckerEntries
    | LastHeight
    | HighlightedNumberCheckerId
    | BarcodeScannerDataAssertion
    | Problems
    | EventDateAndTimeAssertion
    | NumberCheckerManualEntryRowAssertion


defaultAssertionsExcept : List Assertion -> List (( Model, Cmd Msg ) -> Expectation)
defaultAssertionsExcept exceptions =
    let
        allMaybeAssertions : List (Maybe (( Model, Cmd Msg ) -> Expectation))
        allMaybeAssertions =
            [ if List.member Command exceptions then
                Nothing

              else
                Just expectNoCommand
            , if List.member Stopwatches exceptions then
                Nothing

              else
                Just (expectStopwatches None)
            , if List.member LastError exceptions then
                Nothing

              else
                Just (\( model, _ ) -> Expect.equal [] model.lastErrors)
            , if List.member NumberCheckerEntries exceptions then
                Nothing

              else
                Just (expectNumberCheckerEntries [])
            , if List.member LastHeight exceptions then
                Nothing

              else
                Just (expectLastHeight Nothing)
            , if List.member HighlightedNumberCheckerId exceptions then
                Nothing

              else
                Just (expectHighlightedNumberCheckerId Nothing)
            , if List.member BarcodeScannerDataAssertion exceptions then
                Nothing

              else
                Just (expectBarcodeScannerData BarcodeScanner.empty)
            , if List.member Problems exceptions then
                Nothing

              else
                Just (expectProblems [])
            , if List.member EventDateAndTimeAssertion exceptions then
                Nothing

              else
                Just (expectEventDateAndTime (EventDateAndTime "" Nothing "" Nothing))
            , if List.member NumberCheckerManualEntryRowAssertion exceptions then
                Nothing

              else
                Just (expectNumberCheckerManualEntryRow emptyNumberCheckerManualEntryRow)
            ]
    in
    List.filterMap identity allMaybeAssertions


defaultAssertions : List (( Model, Cmd Msg ) -> Expectation)
defaultAssertions =
    defaultAssertionsExcept []


singleStopwatch : Stopwatches
singleStopwatch =
    case StopwatchTests.expectedParsedSampleData of
        StopwatchData times ->
            Single "stopwatch1.txt" times


deduplicate : List comparable -> List comparable
deduplicate list =
    Set.fromList list
        |> Set.toList


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


getBarcodeScannerDataWithFiles : List Int -> BarcodeScannerData
getBarcodeScannerDataWithFiles numbers =
    let
        files : List BarcodeScannerFile
        files =
            List.map String.fromInt numbers
                |> List.map (\num -> BarcodeScannerFile (num ++ ".txt") [] defaultTime)

        lastScanDate : Maybe Time.Posix
        lastScanDate =
            if List.isEmpty numbers then
                Nothing

            else
                defaultTime
    in
    { empty | files = files, lastScanDate = lastScanDate }


createInitialBarcodeScannerDataForSwappingBarcodes : List BarcodeScannerFile -> BarcodeScannerData
createInitialBarcodeScannerDataForSwappingBarcodes files =
    BarcodeScannerData files Dict.empty [] [] [] [] Nothing
        |> regenerateWithWrongWayArounds


createExpectedBarcodeScannerDataForSwappingBarcodes : List BarcodeScannerFile -> BarcodeScannerData
createExpectedBarcodeScannerDataForSwappingBarcodes files =
    BarcodeScannerData files Dict.empty [] [] [] [] Nothing
        |> regenerate


{-| 2018-03-14T09:00:00
-}
baseEventStartTime : Int
baseEventStartTime =
    1521018000000


suite : Test
suite =
    describe "Update logic tests"
        [ describe "No-op tests"
            [ test "No-operation should do nothing" <|
                \() ->
                    update NoOp initModel
                        |> Expect.all defaultAssertions
            ]
        , describe "File drop tests"
            [ describe "Invalid file tests"
                [ test "Empty file should not match against any type" <|
                    \() ->
                        update (FilesDropped [ InteropFile "empty.txt" "" ]) initModel
                            |> Expect.all (expectLastError "UNRECOGNISED_FILE" :: defaultAssertionsExcept [ LastError ])
                , test "Binary file should not match against any type" <|
                    \() ->
                        update (FilesDropped [ InteropFile "binary.txt" "\u{0000}\u{0001}" ]) initModel
                            |> Expect.all (expectLastError "UNRECOGNISED_FILE" :: defaultAssertionsExcept [ LastError ])
                , test "Unrecognised file should not match against any type" <|
                    \() ->
                        update (FilesDropped [ InteropFile "Unrecognised.txt" "This file contents should not be recognised" ]) initModel
                            |> Expect.all (expectLastError "UNRECOGNISED_FILE" :: defaultAssertionsExcept [ LastError ])
                ]
            , describe "Stopwatch file tests"
                [ test "Can upload a single stopwatch data file" <|
                    \() ->
                        update (FilesDropped [ InteropFile "stopwatch1.txt" StopwatchTests.sampleData ]) initModel
                            |> Expect.all (expectStopwatches singleStopwatch :: defaultAssertionsExcept [ Stopwatches ])
                , test "Cannot upload a single invalid stopwatch data file" <|
                    \() ->
                        update (FilesDropped [ InteropFile "stopwatch1.txt" (String.replace "00" "XX" StopwatchTests.sampleData) ]) initModel
                            |> Expect.all (expectLastError "UNRECOGNISED_TIME" :: defaultAssertionsExcept [ LastError ])
                , test "Cannot upload the same single stopwatch data file twice" <|
                    \() ->
                        initModel
                            |> update (FilesDropped [ InteropFile "stopwatch1.txt" StopwatchTests.sampleData ])
                            |> Tuple.first
                            |> update (FilesDropped [ InteropFile "stopwatch1.txt" StopwatchTests.sampleData ])
                            |> Expect.all
                                (expectLastError "STOPWATCH_FILE_ALREADY_LOADED"
                                    :: expectStopwatches singleStopwatch
                                    :: defaultAssertionsExcept [ LastError, Stopwatches ]
                                )
                , test "Can upload two different stopwatch data files" <|
                    \() ->
                        initModel
                            |> update (FilesDropped [ InteropFile "stopwatch1.txt" StopwatchTests.sampleData ])
                            |> Tuple.first
                            |> update (FilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ])
                            |> Expect.all
                                (expectStopwatches doubleStopwatches
                                    :: defaultAssertionsExcept [ Stopwatches ]
                                )
                , test "Uploading a third stopwatch data file has no effect" <|
                    \() ->
                        initModel
                            |> update (FilesDropped [ InteropFile "stopwatch1.txt" StopwatchTests.sampleData ])
                            |> Tuple.first
                            |> update (FilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ])
                            |> Tuple.first
                            |> update (FilesDropped [ InteropFile "stopwatch3.txt" StopwatchTests.sampleData ])
                            |> Expect.all
                                (expectStopwatches doubleStopwatches
                                    :: defaultAssertionsExcept [ Stopwatches ]
                                )
                ]
            , describe "Barcode scanner file tests"
                [ test "Can upload a single barcode scanner file" <|
                    \() ->
                        update (FilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerData1 ]) initModel
                            |> Expect.all
                                (expectBarcodeScannerData parsedBarcodeScannerData1
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, EventDateAndTimeAssertion ]
                                )
                , test "Can upload a single barcode scanner file with position token without leading zeroes" <|
                    \() ->
                        update (FilesDropped [ InteropFile "barcodes1.txt" (String.replace "P0047" "P47" validBarcodeScannerData1) ]) initModel
                            |> Expect.all
                                (expectBarcodeScannerData parsedBarcodeScannerData1
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, EventDateAndTimeAssertion ]
                                )
                , test "Can upload a single barcode scanner file where the first line in the file is incomplete" <|
                    \() ->
                        update (FilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerDataWithIncompleteRecordFirst ]) initModel
                            |> Expect.all
                                (expectBarcodeScannerData parsedBarcodeScannerDataWithIncompleteRecordFirst
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: expectProblems [ NonFixable (PositionMissingAthlete 33) ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, EventDateAndTimeAssertion, Problems ]
                                )
                , test "Can upload a single invalid barcode scanner file" <|
                    \() ->
                        update (FilesDropped [ InteropFile "barcodes1.txt" invalidBarcodeScannerData ]) initModel
                            |> Expect.all
                                ((\( model, _ ) ->
                                    expectSingleUnrecognisedLine (String.replace crlf "" invalidBarcodeScannerData) "INVALID_POSITION_ZERO" (Ok model.barcodeScannerData)
                                 )
                                    :: expectProblems [ NonFixable (UnrecognisedBarcodeScannerLine "A4580442,P0000,14/03/2018 09:47:03") ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, EventDateAndTimeAssertion, Problems ]
                                )
                , test "Cannot upload the same barcode scanner file twice" <|
                    \() ->
                        initModel
                            |> update (FilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerData1 ])
                            |> Tuple.first
                            |> update (FilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerData1 ])
                            |> Expect.all
                                (expectBarcodeScannerData parsedBarcodeScannerData1
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: expectLastError "BARCODE_DATA_ALREADY_LOADED"
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, EventDateAndTimeAssertion, LastError ]
                                )
                , test "Can upload two different barcode scanner files" <|
                    \() ->
                        initModel
                            |> update (FilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerData1 ])
                            |> Tuple.first
                            |> update (FilesDropped [ InteropFile "barcodes2.txt" validBarcodeScannerData2 ])
                            |> Expect.all
                                (expectBarcodeScannerData parsedBarcodeScannerData1And2
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, EventDateAndTimeAssertion ]
                                )
                ]
            , describe "Number checker file tests"
                [ test "Can upload a single number checker file" <|
                    \() ->
                        update (FilesDropped [ InteropFile "numberChecker1.txt" validNumberCheckerData ]) initModel
                            |> Expect.all
                                (expectNumberCheckerEntries parsedNumberCheckerData
                                    :: defaultAssertionsExcept [ NumberCheckerEntries ]
                                )
                , test "Cannot upload an invalid number checker file" <|
                    \() ->
                        update (FilesDropped [ InteropFile "numberChecker1.txt" invalidNumberCheckerData ]) initModel
                            |> Expect.all
                                (expectLastError "WRONG_PART_COUNT"
                                    :: defaultAssertionsExcept [ LastError ]
                                )
                ]
            ]
        , describe "Delete Stopwatch tests"
            [ test "Deleting stopwatch 1 when none to delete does nothing" <|
                \() ->
                    update (DeleteStopwatch StopwatchOne) initModel
                        |> Expect.all defaultAssertions
            , test "Deleting stopwatch 2 when none to delete does nothing" <|
                \() ->
                    update (DeleteStopwatch StopwatchTwo) initModel
                        |> Expect.all defaultAssertions
            , test "Deleting stopwatch 1 when one to delete deletes that stopwatch" <|
                \() ->
                    { initModel | stopwatches = singleStopwatch }
                        |> update (DeleteStopwatch StopwatchOne)
                        |> Expect.all defaultAssertions
            , test "Deleting stopwatch 2 when one to delete does nothing" <|
                \() ->
                    { initModel | stopwatches = singleStopwatch }
                        |> update (DeleteStopwatch StopwatchTwo)
                        |> Expect.all
                            (expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Deleting stopwatch 1 when two to delete deletes stopwatch 1" <|
                \() ->
                    { initModel | stopwatches = doubleStopwatches }
                        |> update (DeleteStopwatch StopwatchOne)
                        |> Expect.all
                            (expectStopwatches (Single "stopwatch2.txt" parsedStopwatchTimes2)
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Deleting stopwatch 2 when two to delete deletes stopwatch 2" <|
                \() ->
                    { initModel | stopwatches = doubleStopwatches }
                        |> update (DeleteStopwatch StopwatchTwo)
                        |> Expect.all
                            (expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            ]
        , describe "Flip Stopwatch tests"
            [ test "Flipping stopwatches when no stopwatches loaded does nothing" <|
                \() ->
                    update FlipStopwatches initModel
                        |> Expect.all defaultAssertions
            , test "Flipping stopwatches when one stopwatch loaded does nothing" <|
                \() ->
                    { initModel | stopwatches = singleStopwatch }
                        |> update FlipStopwatches
                        |> Expect.all
                            (expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Flipping stopwatches when two stopwatches loaded flips both stopwatches" <|
                \() ->
                    { initModel | stopwatches = doubleStopwatches }
                        |> update FlipStopwatches
                        |> Expect.all
                            (expectStopwatches flippedDoubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            ]
        , describe "Clear All Data tests"
            [ test "Clearing all data when nothing to clear does nothing" <|
                \() ->
                    update ClearAllData initModel
                        |> Expect.all defaultAssertions
            , test "Clearing all data when some to clear clears it" <|
                \() ->
                    { initModel
                        | barcodeScannerData = createBarcodeScannerData (Dict.singleton 47 [ "A4580484" ]) [ "A123456" ] [ 11 ]
                        , eventDateAndTime = { parsedEventDateOnly | enteredTime = "09:00", validatedTime = Just (9 * 60) }
                        , stopwatches = doubleStopwatches
                        , lastErrors = [ FileError "TEST_ERROR" "Some test error message" "somefile.txt" ]
                        , lastHeight = Just 700
                        , highlightedNumberCheckerId = Just 2
                        , numberCheckerEntries = [ AnnotatedNumberCheckerEntry 2 2 0 2 0 2 0 2 ]
                        , numberCheckerManualEntryRow = NumberCheckerManualEntryRow (NumericEntry "2" (Just 2)) (NumericEntry "2" (Just 2)) (NumericEntry "2" (Just 2))
                        , problems =
                            [ ProblemEntry (Fixable (PositionWithAndWithoutAthlete 5 "A123")) 1 False
                            , ProblemEntry (NonFixable (Problems.MisScan "something")) 0 False
                            ]
                    }
                        |> update ClearAllData
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime "" Nothing "09:00" (Just (9 * 60)))
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            ]
        , describe "Get current date for download file tests"
            [ test "Getting current date issues a task and returns the same model" <|
                \() ->
                    update (GetCurrentDateForDownloadFile DownloadMergedStopwatchData) initModel
                        |> Expect.all
                            (expectACommand
                                :: defaultAssertionsExcept [ Command ]
                            )
            ]
        , describe "Download merged stopwatch data tests"
            [ test "Does not download merged data for no stopwatches" <|
                \() ->
                    initModel
                        |> update (DownloadMergedStopwatchData Time.utc recentTime)
                        |> Expect.all defaultAssertions
            , test "Does not download merged data for one stopwatch" <|
                \() ->
                    initModel
                        |> update (FilesDropped [ InteropFile "stopwatch1.txt" StopwatchTests.sampleData ])
                        |> Tuple.first
                        |> update (DownloadMergedStopwatchData Time.utc recentTime)
                        |> Expect.all
                            (expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Can download merged data for two stopwatches" <|
                \() ->
                    initModel
                        |> update (FilesDropped [ InteropFile "stopwatch1.txt" StopwatchTests.sampleData ])
                        |> Tuple.first
                        |> update (FilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ])
                        |> Tuple.first
                        |> update (DownloadMergedStopwatchData Time.utc recentTime)
                        |> Expect.all
                            (expectACommand
                                :: expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches, Command ]
                            )
            ]
        , describe "Create stopwatch file for download tests"
            [ test "Can create a stopwatch file for download" <|
                \() ->
                    let
                        model : Model
                        model =
                            initModel
                                |> update (FilesDropped [ InteropFile "stopwatch1.txt" StopwatchTests.sampleData ])
                                |> Tuple.first
                                |> update (FilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ])
                                |> Tuple.first
                    in
                    case model.stopwatches of
                        Double doubleStopwatchData ->
                            createStopwatchFileForDownload Time.utc recentTime doubleStopwatchData.mergedTableRows
                                |> Expect.equal (InteropFile "results_tidyup_timer_14072017024000.txt" expectedMergedStopwatchFileContents)

                        _ ->
                            Expect.fail ("Expected merged data from two stopwatches, got '" ++ Debug.toString model.stopwatches ++ "' instead.")
            ]
        , describe "Container height changed tests"
            [ test "Can update the container height" <|
                \() ->
                    update (ContainerHeightChanged 567) initModel
                        |> Expect.all
                            (expectLastHeight (Just 567)
                                :: defaultAssertionsExcept [ LastHeight ]
                            )
            ]
        , describe "Mouse enter number-checker row tests"
            [ test "Can enter a number-checker row" <|
                \() ->
                    update (MouseEnterNumberCheckerRow 6) initModel
                        |> Expect.all
                            (expectHighlightedNumberCheckerId (Just 6)
                                :: defaultAssertionsExcept [ HighlightedNumberCheckerId ]
                            )
            , test "Can leave a number-checker row that has been entered" <|
                \() ->
                    { initModel | highlightedNumberCheckerId = Just 6 }
                        |> update (MouseLeaveNumberCheckerRow 6)
                        |> Expect.all defaultAssertions
            , test "Attempting to leave a number-checker row that has not been entered has no effect" <|
                \() ->
                    { initModel | highlightedNumberCheckerId = Just 8 }
                        |> update (MouseLeaveNumberCheckerRow 6)
                        |> Expect.all
                            (expectHighlightedNumberCheckerId (Just 8)
                                :: defaultAssertionsExcept [ HighlightedNumberCheckerId ]
                            )
            ]
        , describe "Delete number checker row tests"
            [ test "Can delete a number-checker row when no stopwatches loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData }
                        |> update (DeleteNumberCheckerRow 2)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerDataWithSecondItemRemoved
                                :: defaultAssertionsExcept [ NumberCheckerEntries ]
                            )
            ]
        , describe "Event date changed tests"
            [ test "Setting a valid date sets the validated date" <|
                \() ->
                    update (EventDateChanged "26/05/2018") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime "26/05/2018" (toPosix "2018-05-26T00:00:00.000Z") "" Nothing)
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            ]
        , describe "Event time changed tests"
            [ test "Setting a valid time sets the validated time and issues a command" <|
                \() ->
                    update (EventTimeChanged "09:30") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime "" Nothing "09:30" (Just (9 * 60 + 30)))
                                :: expectACommand
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion, Command ]
                            )
            , test "Setting an invalid time clears the validated time" <|
                \() ->
                    update (EventTimeChanged "This is not a valid time") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime "" Nothing "This is not a valid time" Nothing)
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            ]
        , describe "Number checker field changed tests"
            [ test "Entering a valid value for stopwatch 1 sets the value" <|
                \() ->
                    update (NumberCheckerFieldChanged Stopwatch1 "24") initModel
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (NumericEntry "24" (Just 24)) emptyNumericEntry emptyNumericEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a valid value for stopwatch 2 sets the value" <|
                \() ->
                    update (NumberCheckerFieldChanged Stopwatch2 "38") initModel
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyNumericEntry (NumericEntry "38" (Just 38)) emptyNumericEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a valid value for finish tokens sets the value" <|
                \() ->
                    update (NumberCheckerFieldChanged FinishTokens "17") initModel
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyNumericEntry emptyNumericEntry (NumericEntry "17" (Just 17)))
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a negative value for stopwatch 1 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow (NumericEntry "24" (Just 24)) emptyNumericEntry emptyNumericEntry }
                        |> update (NumberCheckerFieldChanged Stopwatch1 "-2")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (NumericEntry "-2" Nothing) emptyNumericEntry emptyNumericEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a negative value for stopwatch 2 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyNumericEntry (NumericEntry "38" (Just 38)) emptyNumericEntry }
                        |> update (NumberCheckerFieldChanged Stopwatch2 "-3")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyNumericEntry (NumericEntry "-3" Nothing) emptyNumericEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a negative value for finish tokens sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyNumericEntry emptyNumericEntry (NumericEntry "17" (Just 17)) }
                        |> update (NumberCheckerFieldChanged FinishTokens "-1")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyNumericEntry emptyNumericEntry (NumericEntry "-1" Nothing))
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering an invalid value for stopwatch 1 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow (NumericEntry "24" (Just 24)) emptyNumericEntry emptyNumericEntry }
                        |> update (NumberCheckerFieldChanged Stopwatch1 "nonsense")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (NumericEntry "nonsense" Nothing) emptyNumericEntry emptyNumericEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering an invalid value for stopwatch 2 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyNumericEntry (NumericEntry "38" (Just 38)) emptyNumericEntry }
                        |> update (NumberCheckerFieldChanged Stopwatch2 "nonsense")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyNumericEntry (NumericEntry "nonsense" Nothing) emptyNumericEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering an invalid value for finish tokens sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyNumericEntry emptyNumericEntry (NumericEntry "17" (Just 17)) }
                        |> update (NumberCheckerFieldChanged FinishTokens "nonsense")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyNumericEntry emptyNumericEntry (NumericEntry "nonsense" Nothing))
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            ]
        , describe "Add number checker row tests"
            [ test "Can enter a number-checker row with all valid values" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = createNumberCheckerManualEntryRow 12 12 12 }
                        |> update AddNumberCheckerRow
                        |> Expect.all
                            (expectNumberCheckerEntries
                                [ { entryNumber = 1
                                  , finishTokens = 12
                                  , finishTokensDelta = 0
                                  , stopwatch1 = 12
                                  , stopwatch1Delta = 0
                                  , stopwatch2 = 12
                                  , stopwatch2Delta = 0
                                  , actual = 12
                                  }
                                ]
                                :: expectACommand
                                :: defaultAssertionsExcept [ Command, NumberCheckerEntries ]
                            )
            ]
        , describe "IncrementNumberCheckerRow tests"
            [ test "Can increment an actual entry of a number-checker row" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData }
                        |> update (IncrementNumberCheckerRowActualCount 2)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerDataIncremented
                                :: defaultAssertionsExcept [ NumberCheckerEntries ]
                            )
            ]
        , describe "DecrementNumberCheckerRow tests"
            [ test "Can decrement an actual entry of a number-checker row" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData }
                        |> update (DecrementNumberCheckerRowActualCount 2)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerDataDecremented
                                :: defaultAssertionsExcept [ NumberCheckerEntries ]
                            )
            ]
        , describe "Edit number checker row tests"
            [ test "Can edit a number-checker row when no stopwatches loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData }
                        |> update (EditNumberCheckerRow 2)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerDataWithSecondItemRemoved
                                :: expectNumberCheckerManualEntryRow (createNumberCheckerManualEntryRow 11 10 11)
                                :: defaultAssertionsExcept [ NumberCheckerEntries, NumberCheckerManualEntryRowAssertion ]
                            )
            ]
        , describe "Fixing problems tests"
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
                                { initialBarcodeScannerData
                                    | finishTokensOnly = List.filter (\x -> x.position /= 39) initialBarcodeScannerData.finishTokensOnly
                                    , files = deleteLinesWithinFile (ifFinishPosition 39) initialModel.barcodeScannerData.files
                                }
                        in
                        initialModel
                            |> update (FixProblem (RemoveUnassociatedFinishToken 39))
                            |> Expect.all
                                (expectBarcodeScannerData expectedBarcodeScannerData
                                    :: expectProblems
                                        [ Fixable (PositionWithAndWithoutAthlete 14 "A1")
                                        , Fixable (PositionWithAndWithoutAthlete 18 "A2")
                                        , Fixable (PositionWithAndWithoutAthlete 44 "A4")
                                        ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
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
                        in
                        initialModel
                            |> update (FixProblem (RemoveUnassociatedFinishToken 39))
                            |> Expect.all
                                (expectBarcodeScannerData expectedBarcodeScannerData
                                    :: expectProblems
                                        [ Fixable (PositionWithAndWithoutAthlete 14 "A1")
                                        , Fixable (PositionWithAndWithoutAthlete 18 "A3")
                                        , Fixable (PositionWithAndWithoutAthlete 44 "A6")
                                        , NonFixable (PositionWithMultipleAthletes 39 [ "A2", "A4", "A5", "A7" ])
                                        ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
                , test "Removing unassociated finish token when it never occurs has no effect" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingUnassociatedFinishTokens [ 14, 18, 39, 44 ]
                        in
                        initialModel
                            |> update (FixProblem (RemoveUnassociatedFinishToken 27))
                            |> Expect.all
                                (expectBarcodeScannerData initialModel.barcodeScannerData
                                    :: expectProblems
                                        [ Fixable (PositionWithAndWithoutAthlete 14 "A1")
                                        , Fixable (PositionWithAndWithoutAthlete 18 "A2")
                                        , Fixable (PositionWithAndWithoutAthlete 39 "A3")
                                        , Fixable (PositionWithAndWithoutAthlete 44 "A4")
                                        ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
                ]
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
                        in
                        initialModel
                            |> update (FixProblem (RemoveUnassociatedAthlete "A5678"))
                            |> Expect.all
                                (expectBarcodeScannerData expectedBarcodeScannerData
                                    :: expectProblems
                                        [ Fixable (AthleteWithAndWithoutPosition "A1234" 1)
                                        , Fixable (AthleteWithAndWithoutPosition "A3456" 2)
                                        , Fixable (AthleteWithAndWithoutPosition "A9012" 4)
                                        ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
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
                        in
                        initialModel
                            |> update (FixProblem (RemoveUnassociatedAthlete "A5678"))
                            |> Expect.all
                                (expectBarcodeScannerData expectedBarcodeScannerData
                                    :: expectProblems
                                        [ Fixable (AthleteWithAndWithoutPosition "A1234" 1)
                                        , Fixable (AthleteWithAndWithoutPosition "A3456" 3)
                                        , Fixable (AthleteWithAndWithoutPosition "A9012" 6)
                                        , NonFixable (AthleteWithMultiplePositions "A5678" [ 2, 4, 5, 7 ])
                                        ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
                , test "Removing unassociated athlete when it never occurs has no effect" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingUnassociatedAthletes [ "A1234", "A3456", "A5678", "A9012" ]
                        in
                        initialModel
                            |> update (FixProblem (RemoveUnassociatedAthlete "A9090"))
                            |> Expect.all
                                (expectBarcodeScannerData initialModel.barcodeScannerData
                                    :: expectProblems
                                        [ Fixable (AthleteWithAndWithoutPosition "A1234" 1)
                                        , Fixable (AthleteWithAndWithoutPosition "A3456" 2)
                                        , Fixable (AthleteWithAndWithoutPosition "A5678" 3)
                                        , Fixable (AthleteWithAndWithoutPosition "A9012" 4)
                                        ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
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
                        in
                        initialModel
                            |> update (FixProblem (RemoveDuplicateScans 27 "A1234"))
                            |> Expect.all
                                (expectBarcodeScannerData
                                    { initialBarcodeScannerData
                                        | scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A1234" "14/03/2018 09:47:03" ]
                                        , files = deleteLinesWithinFile ifLineNumberGreaterThanOne initialBarcodeScannerData.files
                                    }
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                                )
                , test "Can remove scan that occurs more than twice" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 5

                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                initialModel.barcodeScannerData
                        in
                        initialModel
                            |> update (FixProblem (RemoveDuplicateScans 27 "A1234"))
                            |> Expect.all
                                (expectBarcodeScannerData
                                    { initialBarcodeScannerData
                                        | scannedBarcodes = Dict.singleton 27 [ AthleteAndTimePair "A1234" "14/03/2018 09:47:03" ]
                                        , files = deleteLinesWithinFile ifLineNumberGreaterThanOne initialBarcodeScannerData.files
                                    }
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                                )
                , test "Can remove scan that occurs twice in two different files" <|
                    \() ->
                        let
                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                createBarcodeScannerData (Dict.singleton 27 [ "A1234", "A1234" ]) [] []

                            startingScannerData : BarcodeScannerData
                            startingScannerData =
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
                        in
                        { initModel | barcodeScannerData = startingScannerData }
                            |> update (FixProblem (RemoveDuplicateScans 27 "A1234"))
                            |> Expect.all
                                (expectBarcodeScannerData finalBarcodeScannerData
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                                )
                , test "Attempting to remove duplicate scan when not duplicate does nothing" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 1
                        in
                        initialModel
                            |> update (FixProblem (RemoveDuplicateScans 27 "A1234"))
                            |> Expect.all
                                (expectBarcodeScannerData initialModel.barcodeScannerData
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                                )
                , test "Attempting to remove duplicate scan when finish position wrong does nothing" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 2
                        in
                        initialModel
                            |> update (FixProblem (RemoveDuplicateScans 25 "A1234"))
                            |> Expect.all
                                (expectBarcodeScannerData initialModel.barcodeScannerData
                                    :: expectProblems [ Fixable (AthleteInSamePositionMultipleTimes "A1234" 27) ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
                , test "Attempting to remove duplicate scan when athlete wrong does nothing" <|
                    \() ->
                        let
                            initialModel : Model
                            initialModel =
                                createBarcodeScannerDataForRemovingDuplicateScans 2
                        in
                        initialModel
                            |> update (FixProblem (RemoveDuplicateScans 27 "A9999"))
                            |> Expect.all
                                (expectBarcodeScannerData initialModel.barcodeScannerData
                                    :: expectProblems [ Fixable (AthleteInSamePositionMultipleTimes "A1234" 27) ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
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
                        in
                        { initModel | barcodeScannerData = barcodeScannerDataWithFiles }
                            |> update (FixProblem (RemoveDuplicateScans 27 "A1234"))
                            |> Expect.all
                                (expectBarcodeScannerData barcodeScannerDataWithFiles
                                    :: expectProblems [ NonFixable (PositionWithMultipleAthletes 27 [ "A1234", "A3456", "A5678", "A9012" ]) ]
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
                ]
            ]
        , describe "Removing scans before event start time"
            [ test "Removing scans before 9am clears nothing" <|
                \() ->
                    { initModel | barcodeScannerData = barcodeScannerDataForEventStartTimeFiltering }
                        |> update (FixProblem (RemoveScansBeforeEventStart baseEventStartTime))
                        |> Expect.all
                            (expectBarcodeScannerData barcodeScannerDataForEventStartTimeFiltering
                                :: expectProblems [ NonFixable (AthleteMissingPosition "A345678"), NonFixable (PositionMissingAthlete 19) ]
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                            )
            , test "Removing scans before 9:30am clears genuine scan" <|
                \() ->
                    { initModel | barcodeScannerData = barcodeScannerDataForEventStartTimeFiltering }
                        |> update (FixProblem (RemoveScansBeforeEventStart (baseEventStartTime + 30 * 60 * 1000)))
                        |> Expect.all
                            (expectBarcodeScannerData
                                { barcodeScannerDataForEventStartTimeFiltering
                                    | scannedBarcodes = Dict.empty
                                    , files = deleteLinesWithinFile (ifLineNumberIn [ 1 ]) barcodeScannerDataForEventStartTimeFiltering.files
                                }
                                :: expectProblems [ NonFixable (AthleteMissingPosition "A345678"), NonFixable (PositionMissingAthlete 19) ]
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                            )
            , test "Removing scans before 10:00am clears genuine scan and athlete-barcode only" <|
                \() ->
                    { initModel | barcodeScannerData = barcodeScannerDataForEventStartTimeFiltering }
                        |> update (FixProblem (RemoveScansBeforeEventStart (baseEventStartTime + 60 * 60 * 1000)))
                        |> Expect.all
                            (expectBarcodeScannerData
                                { barcodeScannerDataForEventStartTimeFiltering
                                    | scannedBarcodes = Dict.empty
                                    , athleteBarcodesOnly = []
                                    , files = deleteLinesWithinFile (ifLineNumberIn [ 1, 2 ]) barcodeScannerDataForEventStartTimeFiltering.files
                                }
                                :: expectProblems [ NonFixable (PositionMissingAthlete 19) ]
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                            )
            , test "Removing scans before 10:30am clears everything" <|
                \() ->
                    { initModel | barcodeScannerData = barcodeScannerDataForEventStartTimeFiltering }
                        |> update (FixProblem (RemoveScansBeforeEventStart (baseEventStartTime + 90 * 60 * 1000)))
                        |> Expect.all
                            (expectBarcodeScannerData
                                { barcodeScannerDataForEventStartTimeFiltering
                                    | scannedBarcodes = Dict.empty
                                    , athleteBarcodesOnly = []
                                    , finishTokensOnly = []
                                    , files = deleteLinesWithinFile (ifLineNumberIn [ 1, 2, 3 ]) barcodeScannerDataForEventStartTimeFiltering.files
                                }
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
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
                    in
                    { initModel | barcodeScannerData = initialBarcodeScannerData }
                        |> update (FixProblem (RemoveScansBeforeEventStart (baseEventStartTime + 2 * 60 * 60 * 1000)))
                        |> Expect.all
                            (expectBarcodeScannerData
                                { initialBarcodeScannerData
                                    | scannedBarcodes = Dict.empty
                                    , finishTokensOnly = []
                                    , files = deleteLinesWithinFile (ifLineNumberIn [ 1, 3 ]) initialBarcodeScannerData.files
                                }
                                :: expectProblems [ NonFixable (AthleteMissingPosition "A345678") ]
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                            )
            ]
        , describe "AdjustStopwatch tests"
            [ test "Can adjust stopwatch 1 adding some time to it" <|
                \() ->
                    { initModel | stopwatches = stopwatchesForAdjusting -19 0 }
                        |> update (FixProblem (AdjustStopwatch StopwatchOne 19))
                        |> Expect.all
                            (expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Can adjust stopwatch 1 taking some time from it" <|
                \() ->
                    { initModel | stopwatches = stopwatchesForAdjusting 46 0 }
                        |> update (FixProblem (AdjustStopwatch StopwatchOne -46))
                        |> Expect.all
                            (expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Can adjust stopwatch 2 adding some time to it" <|
                \() ->
                    { initModel | stopwatches = stopwatchesForAdjusting 0 -22 }
                        |> update (FixProblem (AdjustStopwatch StopwatchTwo 22))
                        |> Expect.all
                            (expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Can adjust stopwatch 2 taking some time from it" <|
                \() ->
                    { initModel | stopwatches = stopwatchesForAdjusting 0 37 }
                        |> update (FixProblem (AdjustStopwatch StopwatchTwo -37))
                        |> Expect.all
                            (expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            ]
        , describe "ClearErrors tests"
            [ test "ClearErrors removes the errors" <|
                \() ->
                    { initModel | lastErrors = [ FileError "CODE" "Message" "fileName" ] }
                        |> update ClearErrors
                        |> Expect.all defaultAssertions
            ]
        , describe "DownloadBarcodeScannerFile tests"
            [ test "DownloadBarcodeScannerFile does nothing for empty data" <|
                \() ->
                    update (DownloadBarcodeScannerFile 0 Time.utc recentTime) initModel
                        |> Expect.all defaultAssertions
            , test "DownloadBarcodeScannerFile downloads file at first index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile 0 Time.utc recentTime)
                        |> Expect.all
                            (expectACommand
                                :: expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ Command, BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile deletes file at middle index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile 1 Time.utc recentTime)
                        |> Expect.all
                            (expectACommand
                                :: expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ Command, BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile deletes file at last index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile 2 Time.utc recentTime)
                        |> Expect.all
                            (expectACommand
                                :: expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ Command, BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile does nothing with index off the end of the list" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile 3 Time.utc recentTime)
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile does nothing with index off the beginning of the list" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile -1 Time.utc recentTime)
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            ]
        , describe "DeleteBarcodeScannerFile tests"
            [ test "DeleteBarcodeScannerFile does nothing for empty data" <|
                \() ->
                    update (DeleteBarcodeScannerFile 0) initModel
                        |> Expect.all defaultAssertions
            , test "DeleteBarcodeScannerFile deletes single file" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1 ] }
                        |> update (DeleteBarcodeScannerFile 0)
                        |> Expect.all defaultAssertions
            , test "DeleteBarcodeScannerFile deletes file at first index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile 0)
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "DeleteBarcodeScannerFile deletes file at middle index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile 1)
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "DeleteBarcodeScannerFile deletes file at last index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile 2)
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "DeleteBarcodeScannerFile does nothing with index off the end of the list" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile 3)
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "DeleteBarcodeScannerFile does nothing with index off the beginning of the list" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile -1)
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            ]
        , describe "swapBarcodes tests"
            [ test "swapBarcodes does nothing for empty data" <|
                \() ->
                    update (SwapBarcodes "barcodes1.txt" 1 2) initModel
                        |> Expect.all defaultAssertions
            , test "swapBarcodes swaps data for single barcode pair the wrong way around" <|
                \() ->
                    let
                        initialBarcodeData : BarcodeScannerData
                        initialBarcodeData =
                            createInitialBarcodeScannerDataForSwappingBarcodes
                                [ BarcodeScannerFile "barcodes1.txt"
                                    [ ordinaryFileLine 1 "" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A123456" Nothing "14/03/2018 09:48:41"
                                    ]
                                    (toPosix "2018-03-14T09:48:41.000Z")
                                ]

                        expectedBarcodeData : BarcodeScannerData
                        expectedBarcodeData =
                            createExpectedBarcodeScannerDataForSwappingBarcodes
                                [ BarcodeScannerFile "barcodes1.txt"
                                    [ ordinaryFileLine 1 "A123456" (Just 47) "14/03/2018 09:47:03"
                                    , BarcodeScannerFileLine 2 (Ordinary "A123456" Nothing) "14/03/2018 09:48:41" (Deleted EndOfWrongWayAroundSection) NotWrongWayAround
                                    ]
                                    (toPosix "2018-03-14T09:48:41.000Z")
                                ]
                    in
                    { initModel | barcodeScannerData = initialBarcodeData }
                        |> update (SwapBarcodes "barcodes1.txt" 1 2)
                        |> Expect.all
                            (expectBarcodeScannerData expectedBarcodeData
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "swapBarcodes swaps data for double barcode pair the wrong way around" <|
                \() ->
                    let
                        initialBarcodeData : BarcodeScannerData
                        initialBarcodeData =
                            createInitialBarcodeScannerDataForSwappingBarcodes
                                [ BarcodeScannerFile "barcodes1.txt"
                                    [ ordinaryFileLine 1 "" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A123456" (Just 40) "14/03/2018 09:48:41"
                                    , ordinaryFileLine 3 "A565656" Nothing "14/03/2018 09:49:06"
                                    ]
                                    (toPosix "2018-03-14T09:49:06.000Z")
                                ]

                        expectedBarcodeData : BarcodeScannerData
                        expectedBarcodeData =
                            createExpectedBarcodeScannerDataForSwappingBarcodes
                                [ BarcodeScannerFile "barcodes1.txt"
                                    [ ordinaryFileLine 1 "A123456" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A565656" (Just 40) "14/03/2018 09:48:41"
                                    , BarcodeScannerFileLine 3 (Ordinary "A565656" Nothing) "14/03/2018 09:49:06" (Deleted EndOfWrongWayAroundSection) NotWrongWayAround
                                    ]
                                    (toPosix "2018-03-14T09:49:06.000Z")
                                ]
                    in
                    { initModel | barcodeScannerData = initialBarcodeData }
                        |> update (SwapBarcodes "barcodes1.txt" 1 3)
                        |> Expect.all
                            (expectBarcodeScannerData expectedBarcodeData
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "swapBarcodes swaps data for single barcode pair the wrong way around with ordinary record afterwards" <|
                \() ->
                    let
                        initialBarcodeData : BarcodeScannerData
                        initialBarcodeData =
                            createInitialBarcodeScannerDataForSwappingBarcodes
                                [ BarcodeScannerFile "barcodes1.txt"
                                    [ ordinaryFileLine 1 "" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A123456" Nothing "14/03/2018 09:48:41"
                                    , ordinaryFileLine 3 "A565656" (Just 40) "14/03/2018 09:49:06"
                                    ]
                                    (toPosix "2018-03-14T09:49:06.000Z")
                                ]

                        expectedBarcodeData : BarcodeScannerData
                        expectedBarcodeData =
                            createExpectedBarcodeScannerDataForSwappingBarcodes
                                [ BarcodeScannerFile "barcodes1.txt"
                                    [ ordinaryFileLine 1 "A123456" (Just 47) "14/03/2018 09:47:03"
                                    , BarcodeScannerFileLine 2 (Ordinary "A123456" Nothing) "14/03/2018 09:48:41" (Deleted EndOfWrongWayAroundSection) NotWrongWayAround
                                    , ordinaryFileLine 3 "A565656" (Just 40) "14/03/2018 09:49:06"
                                    ]
                                    (toPosix "2018-03-14T09:49:06.000Z")
                                ]
                    in
                    { initModel | barcodeScannerData = initialBarcodeData }
                        |> update (SwapBarcodes "barcodes1.txt" 1 2)
                        |> Expect.all
                            (expectBarcodeScannerData expectedBarcodeData
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "swapBarcodes does nothing for mismatched filename" <|
                \() ->
                    let
                        initialBarcodeData : BarcodeScannerData
                        initialBarcodeData =
                            createInitialBarcodeScannerDataForSwappingBarcodes
                                [ BarcodeScannerFile "barcodes1.txt"
                                    [ ordinaryFileLine 1 "" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A123456" Nothing "14/03/2018 09:48:41"
                                    ]
                                    (toPosix "2018-03-14T09:48:41.000Z")
                                ]
                    in
                    { initModel | barcodeScannerData = initialBarcodeData }
                        |> update (SwapBarcodes "wrong_filename.txt" 1 2)
                        |> Expect.all
                            (expectBarcodeScannerData initialBarcodeData
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            ]
        , describe "IgnoreProblems tests"
            [ test "Can ignore a problem with a given index" <|
                \() ->
                    { initModel
                        | problems =
                            [ ProblemEntry (NonFixable (Problems.MisScan "&junk")) 0 False
                            , ProblemEntry (NonFixable (Problems.MisScan "&junk2")) 1 False
                            ]
                    }
                        |> update (IgnoreProblem 1)
                        |> Expect.all
                            (expectProblemEntries
                                [ ProblemEntry (NonFixable (Problems.MisScan "&junk")) 0 False
                                , ProblemEntry (NonFixable (Problems.MisScan "&junk2")) 1 True
                                ]
                                :: defaultAssertionsExcept [ Problems ]
                            )
            , test "Does nothing for a problem with a given index already ignored" <|
                \() ->
                    let
                        problemEntries : List ProblemEntry
                        problemEntries =
                            [ ProblemEntry (NonFixable (Problems.MisScan "&junk")) 0 False
                            , ProblemEntry (NonFixable (Problems.MisScan "&junk2")) 1 True
                            ]
                    in
                    { initModel | problems = problemEntries }
                        |> update (IgnoreProblem 1)
                        |> Expect.all
                            (expectProblemEntries problemEntries
                                :: defaultAssertionsExcept [ Problems ]
                            )
            , test "Does nothing for a problem with an unrecognised index being ignored" <|
                \() ->
                    let
                        problemEntries : List ProblemEntry
                        problemEntries =
                            [ ProblemEntry (NonFixable (Problems.MisScan "&junk")) 0 False
                            , ProblemEntry (NonFixable (Problems.MisScan "&junk2")) 1 False
                            ]
                    in
                    { initModel | problems = problemEntries }
                        |> update (IgnoreProblem 2)
                        |> Expect.all
                            (expectProblemEntries problemEntries
                                :: defaultAssertionsExcept [ Problems ]
                            )
            ]
        ]



-- TODO: some tests with everything: stopwatches, barcodes and number-checker data.
