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
import BarcodeScannerEditing exposing (BarcodeScannerFieldBeingEdited(..), BarcodeScannerRowEditDetails, BarcodeScannerRowEditLocation)
import BarcodeScannerTests exposing (createBarcodeScannerData)
import Commands exposing (Command(..), DownloadOperation, ElementToFocus(..))
import DataEntry exposing (DateEntry, IntegerEntry, emptyEntry)
import Dict exposing (Dict)
import Error exposing (FileError)
import EventDateAndTime exposing (EventDateAndTime)
import Expect exposing (Expectation)
import FileHandling exposing (InteropFile)
import Model
    exposing
        ( DialogDetails(..)
        , Model
        , NumberCheckerManualEntryRow
        , emptyNumberCheckerManualEntryRow
        , initModel
        )
import Msg exposing (Msg(..), NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import ProblemFixing exposing (ProblemFix(..), ProblemIgnorance(..))
import Problems exposing (AthleteAndPositionPair, IgnoredProblems, Problems, noIgnoredProblems, noProblems)
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


expectHighlightedNumberCheckerId : Maybe Int -> ( Model, Command ) -> Expectation
expectHighlightedNumberCheckerId expectedHighlightedNumberCheckerId ( model, _ ) =
    Expect.equal expectedHighlightedNumberCheckerId model.highlightedNumberCheckerId


expectBarcodeScannerData : BarcodeScannerData -> ( Model, Command ) -> Expectation
expectBarcodeScannerData expectedBarcodeScannerData ( model, _ ) =
    Expect.equal expectedBarcodeScannerData model.barcodeScannerData


expectProblems : Problems -> ( Model, Command ) -> Expectation
expectProblems expectedProblems ( model, _ ) =
    Expect.equal expectedProblems model.problems


expectEventDateAndTime : EventDateAndTime -> ( Model, Command ) -> Expectation
expectEventDateAndTime expectedEventDateAndTime ( model, _ ) =
    Expect.equal expectedEventDateAndTime model.eventDateAndTime


expectNumberCheckerManualEntryRow : NumberCheckerManualEntryRow -> ( Model, Command ) -> Expectation
expectNumberCheckerManualEntryRow expectedManualEntryRow ( model, _ ) =
    Expect.equal expectedManualEntryRow model.numberCheckerManualEntryRow


expectIgnoredProblems : IgnoredProblems -> ( Model, Command ) -> Expectation
expectIgnoredProblems ignoredProblems ( model, _ ) =
    Expect.equal ignoredProblems model.ignoredProblems


type Assertion
    = Command
    | Stopwatches
    | LastError
    | NumberCheckerEntries
    | HighlightedNumberCheckerId
    | BarcodeScannerDataAssertion
    | Problems
    | EventDateAndTimeAssertion
    | NumberCheckerManualEntryRowAssertion
    | IgnoredProblemsAssertion


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
                Just (expectProblems noProblems)
            , if List.member EventDateAndTimeAssertion exceptions then
                Nothing

              else
                Just (expectEventDateAndTime (EventDateAndTime emptyEntry emptyEntry))
            , if List.member NumberCheckerManualEntryRowAssertion exceptions then
                Nothing

              else
                Just (expectNumberCheckerManualEntryRow emptyNumberCheckerManualEntryRow)
            , if List.member IgnoredProblemsAssertion exceptions then
                Nothing

              else
                Just (expectIgnoredProblems noIgnoredProblems)
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
        , problems = { noProblems | positionsWithAndWithoutAthlete = List.indexedMap (\index position -> AthleteAndPositionPair (fakeAthlete index) position) finishTokens }
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
                |> List.map (\num -> BarcodeScannerFile (num ++ ".txt") [] defaultDateTime)

        lastScanDateTime : Maybe Time.Posix
        lastScanDateTime =
            if List.isEmpty numbers then
                Nothing

            else
                defaultDateTime
    in
    { empty | files = files, lastScanDateTime = lastScanDateTime }


makeBarcodeScannerRowEditDetails : BarcodeScannerRowEditLocation -> Maybe Int -> Maybe Int -> BarcodeScannerRowEditDetails
makeBarcodeScannerRowEditDetails location athlete finishPosition =
    BarcodeScannerRowEditDetails
        location
        (Ordinary "" Nothing)
        (IntegerEntry "" athlete)
        (IntegerEntry "" finishPosition)
        Both
        Nothing
        False


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
                                    :: expectProblems { noProblems | positionsMissingAthlete = [ 33 ] }
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, EventDateAndTimeAssertion, Problems ]
                                )
                ]
            ]
        , describe "Delete Stopwatch tests"
            [ test "Deleting stopwatch 1 when none to delete does nothing" <|
                \() ->
                    update (RemoveStopwatch StopwatchOne) initModel
                        |> Expect.all defaultAssertions
            , test "Deleting stopwatch 2 when none to delete does nothing" <|
                \() ->
                    update (RemoveStopwatch StopwatchTwo) initModel
                        |> Expect.all defaultAssertions
            , test "Deleting stopwatch 1 when one to delete deletes that stopwatch" <|
                \() ->
                    { initModel | stopwatches = singleStopwatch }
                        |> update (RemoveStopwatch StopwatchOne)
                        |> Expect.all defaultAssertions
            , test "Deleting stopwatch 2 when one to delete does nothing" <|
                \() ->
                    { initModel | stopwatches = singleStopwatch }
                        |> update (RemoveStopwatch StopwatchTwo)
                        |> Expect.all
                            (expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Deleting stopwatch 1 when two to delete deletes stopwatch 1" <|
                \() ->
                    { initModel | stopwatches = doubleStopwatches }
                        |> update (RemoveStopwatch StopwatchOne)
                        |> Expect.all
                            (expectStopwatches (Single "stopwatch2.txt" parsedStopwatchTimes2)
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Deleting stopwatch 2 when two to delete deletes stopwatch 2" <|
                \() ->
                    { initModel | stopwatches = doubleStopwatches }
                        |> update (RemoveStopwatch StopwatchTwo)
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
                        , eventDateAndTime = { parsedEventDateOnly | time = IntegerEntry "09:00" (Just (9 * 60)) }
                        , stopwatches = doubleStopwatches
                        , lastErrors = [ FileError "TEST_ERROR" "Some test error message" "somefile.txt" ]
                        , highlightedNumberCheckerId = Just 2
                        , numberCheckerEntries = [ AnnotatedNumberCheckerEntry 2 2 0 2 0 2 0 2 ]
                        , numberCheckerManualEntryRow = NumberCheckerManualEntryRow (IntegerEntry "2" (Just 2)) (IntegerEntry "2" (Just 2)) (IntegerEntry "2" (Just 2))
                        , problems =
                            { noProblems
                                | positionsWithAndWithoutAthlete = [ AthleteAndPositionPair "A123" 5 ]
                                , misScans = [ "something" ]
                            }
                        , ignoredProblems =
                            { noIgnoredProblems | ignoreStopwatchTimeOffsets = True }
                    }
                        |> update ClearAllData
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime emptyEntry (IntegerEntry "09:00" (Just (9 * 60))))
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
                            (expectEventDateAndTime (EventDateAndTime (DateEntry "26/05/2018" (toPosix "2018-05-26T00:00:00.000Z")) emptyEntry)
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            ]
        , describe "Event time changed tests"
            [ test "Setting a valid time sets the validated time and issues a command" <|
                \() ->
                    update (EventTimeChanged "09:30") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime emptyEntry (IntegerEntry "09:30" (Just (9 * 60 + 30))))
                                :: expectCommand (SaveEventStartTime (9 * 60 + 30))
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion, Command ]
                            )
            , test "Setting an invalid time clears the validated time" <|
                \() ->
                    update (EventTimeChanged "This is not a valid time") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime emptyEntry (IntegerEntry "This is not a valid time" Nothing))
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            ]
        , describe "Number checker field changed tests"
            [ test "Entering a valid value for stopwatch 1 sets the value" <|
                \() ->
                    update (NumberCheckerFieldChanged Stopwatch1 "24") initModel
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (IntegerEntry "24" (Just 24)) emptyEntry emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a valid value for stopwatch 2 sets the value" <|
                \() ->
                    update (NumberCheckerFieldChanged Stopwatch2 "38") initModel
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyEntry (IntegerEntry "38" (Just 38)) emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a valid value for finish tokens sets the value" <|
                \() ->
                    update (NumberCheckerFieldChanged FinishTokens "17") initModel
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyEntry emptyEntry (IntegerEntry "17" (Just 17)))
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a negative value for stopwatch 1 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow (IntegerEntry "24" (Just 24)) emptyEntry emptyEntry }
                        |> update (NumberCheckerFieldChanged Stopwatch1 "-2")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (IntegerEntry "-2" Nothing) emptyEntry emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a negative value for stopwatch 2 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyEntry (IntegerEntry "38" (Just 38)) emptyEntry }
                        |> update (NumberCheckerFieldChanged Stopwatch2 "-3")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyEntry (IntegerEntry "-3" Nothing) emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a negative value for finish tokens sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyEntry emptyEntry (IntegerEntry "17" (Just 17)) }
                        |> update (NumberCheckerFieldChanged FinishTokens "-1")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyEntry emptyEntry (IntegerEntry "-1" Nothing))
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering an invalid value for stopwatch 1 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow (IntegerEntry "24" (Just 24)) emptyEntry emptyEntry }
                        |> update (NumberCheckerFieldChanged Stopwatch1 "nonsense")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (IntegerEntry "nonsense" Nothing) emptyEntry emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering an invalid value for stopwatch 2 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyEntry (IntegerEntry "38" (Just 38)) emptyEntry }
                        |> update (NumberCheckerFieldChanged Stopwatch2 "nonsense")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyEntry (IntegerEntry "nonsense" Nothing) emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering an invalid value for finish tokens sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyEntry emptyEntry (IntegerEntry "17" (Just 17)) }
                        |> update (NumberCheckerFieldChanged FinishTokens "nonsense")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow emptyEntry emptyEntry (IntegerEntry "nonsense" Nothing))
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
                                    { noProblems
                                        | positionsWithAndWithoutAthlete = [ AthleteAndPositionPair "A1" 14, AthleteAndPositionPair "A2" 18, AthleteAndPositionPair "A4" 44 ]
                                    }
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                            )
            ]
        , describe "Ignoring problems tests"
            [ test "Can ignore stopwatch-offsets problem" <|
                \() ->
                    let
                        expectedIgnoredProblems : IgnoredProblems
                        expectedIgnoredProblems =
                            { noIgnoredProblems | ignoreStopwatchTimeOffsets = True }

                        initialModel : Model
                        initialModel =
                            { initModel | problems = { noProblems | stopwatchTimeOffset = Just 5 } }
                    in
                    update (IgnoreProblem IgnoreStopwatchTimeOffsets) initialModel
                        |> Expect.all
                            (expectIgnoredProblems expectedIgnoredProblems
                                :: defaultAssertionsExcept [ IgnoredProblemsAssertion ]
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
        , describe "RemoveBarcodeScannerFile tests"
            [ test "RemoveBarcodeScannerFile does nothing for empty data" <|
                \() ->
                    update (RemoveBarcodeScannerFile "1.txt") initModel
                        |> Expect.all defaultAssertions
            , test "RemoveBarcodeScannerFile deletes single file" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1 ] }
                        |> update (RemoveBarcodeScannerFile "1.txt")
                        |> Expect.all defaultAssertions
            , test "RemoveBarcodeScannerFile deletes file at first index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (RemoveBarcodeScannerFile "1.txt")
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "RemoveBarcodeScannerFile deletes file at middle index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (RemoveBarcodeScannerFile "2.txt")
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "RemoveBarcodeScannerFile deletes file at last index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (RemoveBarcodeScannerFile "3.txt")
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            , test "RemoveBarcodeScannerFile does nothing with non-existent filename" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (RemoveBarcodeScannerFile "nonexistent.txt")
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            ]
        , describe "UpdateRowFromBarcodeScannerEditModal tests"
            [ test "Can update a row in a barcode scanner file" <|
                \() ->
                    let
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
                        |> update (UpdateRowFromBarcodeScannerEditModal (makeBarcodeScannerRowEditDetails (BarcodeScannerRowEditLocation "barcodes6.txt" 1) (Just 2022807) (Just 31)))
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
        , describe "OpenUploadFileDialog tests"
            [ test "Can open the file upload dialog" <|
                \() ->
                    update OpenUploadFileDialog initModel
                        |> Expect.all
                            (expectCommand SelectFileForUpload
                                :: defaultAssertionsExcept [ Command ]
                            )
            ]
        , describe "ReturnKeyPressed tests"
            [ test "Pressing Return when no dialog opened does nothing." <|
                \() ->
                    update ReturnKeyPressed initModel
                        |> Expect.all defaultAssertions
            , test "Can update a row in a barcode scanner file by pressing Return when dialog opened" <|
                \() ->
                    let
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
                    { initModel
                        | barcodeScannerData = initialBarcodeScannerData
                        , dialogDetails = BarcodeScannerRowEditDialog (makeBarcodeScannerRowEditDetails (BarcodeScannerRowEditLocation "barcodes6.txt" 1) (Just 2022807) (Just 31))
                    }
                        |> update ReturnKeyPressed
                        |> Expect.all
                            (expectBarcodeScannerData expectedBarcodeScannerData
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                            )
            ]
        ]



-- TODO: some tests with everything: stopwatches, barcodes and number-checker data.
