module UpdateLogicTests exposing (suite)

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        , empty
        , regenerate
        )
import BarcodeScannerEditing exposing (BarcodeScannerRowEditLocation)
import BarcodeScannerTests exposing (createBarcodeScannerData)
import Commands exposing (Command(..), DownloadOperation, ElementToFocus(..))
import Dict exposing (Dict)
import Error exposing (FileError)
import EventDateAndTime exposing (EventDateAndTime)
import Expect exposing (Expectation)
import FileHandling exposing (InteropFile)
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
import ProblemFixing exposing (ProblemFix(..))
import Problems exposing (FixableProblem(..), NonFixableProblem(..), Problem(..))
import Stopwatch exposing (Stopwatch(..), Stopwatches(..), WhichStopwatch(..))
import Test exposing (Test, describe, test)
import TestData exposing (..)
import Time
import UpdateLogic exposing (barcodeScannerFileMimeType, stopwatchFileMimeType, update)


expectNoCommand : ( Model, Command ) -> Expectation
expectNoCommand ( _, cmd ) =
    Expect.equal NoCommand cmd


expectCommand : Command -> ( Model, Command ) -> Expectation
expectCommand command ( _, cmd ) =
    Expect.equal command cmd


expectStopwatches : Stopwatches -> ( Model, Command ) -> Expectation
expectStopwatches expectedStopwatches ( model, _ ) =
    Expect.equal expectedStopwatches model.stopwatches


expectLastError : String -> ( Model, Command ) -> Expectation
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


expectNumberCheckerEntries : List AnnotatedNumberCheckerEntry -> ( Model, Command ) -> Expectation
expectNumberCheckerEntries expectedNumberCheckerEntries ( model, _ ) =
    Expect.equal expectedNumberCheckerEntries model.numberCheckerEntries


expectLastHeight : Maybe Int -> ( Model, Command ) -> Expectation
expectLastHeight expectedLastHeight ( model, _ ) =
    Expect.equal expectedLastHeight model.lastHeight


expectHighlightedNumberCheckerId : Maybe Int -> ( Model, Command ) -> Expectation
expectHighlightedNumberCheckerId expectedHighlightedNumberCheckerId ( model, _ ) =
    Expect.equal expectedHighlightedNumberCheckerId model.highlightedNumberCheckerId


expectBarcodeScannerData : BarcodeScannerData -> ( Model, Command ) -> Expectation
expectBarcodeScannerData expectedBarcodeScannerData ( model, _ ) =
    Expect.equal expectedBarcodeScannerData model.barcodeScannerData


expectProblems : List Problem -> ( Model, Command ) -> Expectation
expectProblems expectedProblems ( model, _ ) =
    let
        expectedProblemEntries : List ProblemEntry
        expectedProblemEntries =
            List.indexedMap (\index problem -> ProblemEntry problem index False) expectedProblems
    in
    Expect.equal expectedProblemEntries model.problems


expectProblemEntries : List ProblemEntry -> ( Model, Command ) -> Expectation
expectProblemEntries expectedProblemEntries ( model, _ ) =
    Expect.equal expectedProblemEntries model.problems


expectEventDateAndTime : EventDateAndTime -> ( Model, Command ) -> Expectation
expectEventDateAndTime expectedEventDateAndTime ( model, _ ) =
    Expect.equal expectedEventDateAndTime model.eventDateAndTime


expectNumberCheckerManualEntryRow : NumberCheckerManualEntryRow -> ( Model, Command ) -> Expectation
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


defaultAssertionsExcept : List Assertion -> List (( Model, Command ) -> Expectation)
defaultAssertionsExcept exceptions =
    let
        allMaybeAssertions : List (Maybe (( Model, Command ) -> Expectation))
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
                Just (expectBarcodeScannerData empty)
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


defaultAssertions : List (( Model, Command ) -> Expectation)
defaultAssertions =
    defaultAssertionsExcept []


singleStopwatch : Stopwatches
singleStopwatch =
    case expectedParsedSampleStopwatchData of
        StopwatchData times ->
            Single "stopwatch1.txt" times


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


deleteLinesWithinFile : (BarcodeScannerFileLine -> BarcodeScannerFileLine) -> List BarcodeScannerFile -> List BarcodeScannerFile
deleteLinesWithinFile deleter files =
    let
        deleteInFile : BarcodeScannerFile -> BarcodeScannerFile
        deleteInFile file =
            { file | lines = List.map deleter file.lines }
    in
    List.map deleteInFile files


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
            [ describe "Barcode scanner file tests"
                [ test "Can upload a single barcode scanner file" <|
                    \() ->
                        update (FilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerData1 ]) initModel
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
            [ test "Getting current date issues a command and returns the same model" <|
                \() ->
                    update (GetCurrentDateForDownloadFile Commands.DownloadMergedStopwatches) initModel
                        |> Expect.all
                            (expectCommand (GetCurrentDateAndTime Commands.DownloadMergedStopwatches)
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
                        |> update (FilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ])
                        |> Tuple.first
                        |> update (DownloadMergedStopwatchData Time.utc recentTime)
                        |> Expect.all
                            (expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Can download merged data for two stopwatches" <|
                \() ->
                    initModel
                        |> update (FilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ])
                        |> Tuple.first
                        |> update (FilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ])
                        |> Tuple.first
                        |> update (DownloadMergedStopwatchData Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile stopwatchFileMimeType (InteropFile "results_tidyup_timer_14072017024000.txt" expectedMergedStopwatchFileContents))
                                :: expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches, Command ]
                            )
            ]
        , describe "Create single stopwatch file for download tests"
            [ test "Cannot create a stopwatch file for stopwatch 1 when no stopwatches" <|
                \() ->
                    initModel
                        |> update (DownloadStopwatch StopwatchOne Time.utc recentTime)
                        |> Expect.all defaultAssertions
            , test "Cannot create a stopwatch file for stopwatch 2 when no stopwatches" <|
                \() ->
                    initModel
                        |> update (DownloadStopwatch StopwatchTwo Time.utc recentTime)
                        |> Expect.all defaultAssertions
            , test "Can create a stopwatch file for stopwatch 1 when single stopwatch" <|
                \() ->
                    initModel
                        |> update (FilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ])
                        |> Tuple.first
                        |> update (DownloadStopwatch StopwatchOne Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile stopwatchFileMimeType (InteropFile "results_tidyup_timer_14072017024000.txt" expectedDownloadedStopwatchData1))
                                :: expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches, Command ]
                            )
            , test "Cannot create a stopwatch file for stopwatch 2 when only one stopwatch" <|
                \() ->
                    initModel
                        |> update (FilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ])
                        |> Tuple.first
                        |> update (DownloadStopwatch StopwatchTwo Time.utc recentTime)
                        |> Expect.all (defaultAssertionsExcept [ Stopwatches ])
            , test "Can create a stopwatch file for stopwatch 1 when two stopwatches uploaded" <|
                \() ->
                    initModel
                        |> update (FilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ])
                        |> Tuple.first
                        |> update (FilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ])
                        |> Tuple.first
                        |> update (DownloadStopwatch StopwatchOne Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile stopwatchFileMimeType (InteropFile "results_tidyup_timer_14072017024000.txt" expectedDownloadedStopwatchData1))
                                :: expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches, Command ]
                            )
            , test "Can create a stopwatch file for stopwatch 2 when two stopwatches uploaded" <|
                \() ->
                    initModel
                        |> update (FilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ])
                        |> Tuple.first
                        |> update (FilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ])
                        |> Tuple.first
                        |> update (DownloadStopwatch StopwatchTwo Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile stopwatchFileMimeType (InteropFile "results_tidyup_timer_14072017024000.txt" expectedDownloadedStopwatchData2))
                                :: expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches, Command ]
                            )
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
                                :: expectCommand (SaveEventStartTime (9 * 60 + 30))
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
                                :: expectCommand (FocusElement NumberCheckerManualEntryRowFirstCell)
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
                    update (DownloadBarcodeScannerFile "0.txt" Time.utc recentTime) initModel
                        |> Expect.all defaultAssertions
            , test "DownloadBarcodeScannerFile downloads file at first index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile "1.txt" Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile barcodeScannerFileMimeType (InteropFile "results_tidyup_barcode_14072017024000.txt" ""))
                                :: expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ Command, BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile downloads file at middle index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile "2.txt" Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile barcodeScannerFileMimeType (InteropFile "results_tidyup_barcode_14072017024000.txt" ""))
                                :: expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ Command, BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile downloads file at last index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile "3.txt" Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile barcodeScannerFileMimeType (InteropFile "results_tidyup_barcode_14072017024000.txt" ""))
                                :: expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ Command, BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile does nothing with nonexistent filename" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile "nonexistent.txt" Time.utc recentTime)
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            ]
        , describe "DeleteBarcodeScannerFile tests"
            [ test "DeleteBarcodeScannerFile does nothing for empty data" <|
                \() ->
                    update (DeleteBarcodeScannerFile "1.txt") initModel
                        |> Expect.all defaultAssertions
            , test "DeleteBarcodeScannerFile deletes single file" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1 ] }
                        |> update (DeleteBarcodeScannerFile "1.txt")
                        |> Expect.all defaultAssertions
            , test "DeleteBarcodeScannerFile deletes file at first index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile "1.txt")
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "DeleteBarcodeScannerFile deletes file at middle index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile "2.txt")
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "DeleteBarcodeScannerFile deletes file at last index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile "3.txt")
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "DeleteBarcodeScannerFile does nothing with non-existent filename" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DeleteBarcodeScannerFile "nonexistent.txt")
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
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
        , describe "UpdateRowFromBarcodeScannerEditModal tests"
            [ test "Can update a row in a barcode scanner file" <|
                \() ->
                    let
                        lineToDelete : BarcodeScannerFileLine
                        lineToDelete =
                            ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"

                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A2022807" (Just 31) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    { initModel | barcodeScannerData = initialBarcodeScannerData }
                        |> update (UpdateRowFromBarcodeScannerEditModal (BarcodeScannerRowEditLocation "barcodes6.txt" 1) "A2022807" (Just 31))
                        |> Expect.all
                            (expectBarcodeScannerData expectedBarcodeScannerData
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            ]
        , describe "DeleteRowFromBarcodeScannerEditModal tests"
            [ test "Can delete a row from a barcode scanner file" <|
                \() ->
                    let
                        lineToDelete : BarcodeScannerFileLine
                        lineToDelete =
                            ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"

                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ lineToDelete
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ { lineToDelete | deletionStatus = Deleted DeletedByUser }
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    { initModel | barcodeScannerData = initialBarcodeScannerData }
                        |> update (DeleteRowFromBarcodeScannerEditModal (BarcodeScannerRowEditLocation "barcodes6.txt" 1))
                        |> Expect.all
                            (expectBarcodeScannerData expectedBarcodeScannerData
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            ]
        ]



-- TODO: some tests with everything: stopwatches, barcodes and number-checker data.
