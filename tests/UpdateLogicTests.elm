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
import Commands exposing (Command(..), ElementToFocus(..))
import DataEntry exposing (IntegerEntry, emptyEntry)
import Dict
import Error exposing (FileError)
import Expect exposing (Expectation)
import FileHandling exposing (AddedFile, InteropFile)
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
import PastedFile exposing (PastedFileDetails, PastedFileInterpretation(..))
import PastedFileTests exposing (timerFileContents)
import ProblemFixing exposing (ProblemFix(..), ProblemIgnorance(..))
import Problems exposing (AthleteWithAndWithoutPositionProblem, IgnoredProblems, Problems, noIgnoredProblems, noProblems)
import Test exposing (Test, describe, test)
import TestData exposing (..)
import Time
import Timer exposing (Timer(..), TimerFile, Timers(..), WhichTimer(..))
import UpdateLogic exposing (barcodeScannerFileMimeType, timerFileMimeType, update)


expectNoCommand : ( Model, Command ) -> Expectation
expectNoCommand ( _, cmd ) =
    Expect.equal NoCommand cmd


expectCommand : Command -> ( Model, Command ) -> Expectation
expectCommand command ( _, cmd ) =
    Expect.equal command cmd


expectTimers : Timers -> ( Model, Command ) -> Expectation
expectTimers expectedTimers ( model, _ ) =
    Expect.equal expectedTimers model.timers


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


expectNumberCheckerManualEntryRow : NumberCheckerManualEntryRow -> ( Model, Command ) -> Expectation
expectNumberCheckerManualEntryRow expectedManualEntryRow ( model, _ ) =
    Expect.equal expectedManualEntryRow model.numberCheckerManualEntryRow


expectIgnoredProblems : IgnoredProblems -> ( Model, Command ) -> Expectation
expectIgnoredProblems ignoredProblems ( model, _ ) =
    Expect.equal ignoredProblems model.ignoredProblems


expectDialogDetails : DialogDetails -> ( Model, Command ) -> Expectation
expectDialogDetails dialogDetails ( model, _ ) =
    Expect.equal dialogDetails model.dialogDetails


setNameInScannerFile : String -> String -> BarcodeScannerFile -> BarcodeScannerFile
setNameInScannerFile filename name file =
    { file | filename = filename, name = name }


type Assertion
    = Command
    | Timers
    | LastError
    | NumberCheckerEntries
    | HighlightedNumberCheckerId
    | BarcodeScannerDataAssertion
    | Problems
    | NumberCheckerManualEntryRowAssertion
    | IgnoredProblemsAssertion
    | DialogDetailsAssertion


defaultAssertionsExcept : List Assertion -> List (( Model, Command ) -> Expectation)
defaultAssertionsExcept exceptions =
    let
        allMaybeAssertions : List (Maybe (( Model, Command ) -> Expectation))
        allMaybeAssertions =
            [ if List.member Command exceptions then
                Nothing

              else
                Just expectNoCommand
            , if List.member Timers exceptions then
                Nothing

              else
                Just (expectTimers None)
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
            , if List.member NumberCheckerManualEntryRowAssertion exceptions then
                Nothing

              else
                Just (expectNumberCheckerManualEntryRow emptyNumberCheckerManualEntryRow)
            , if List.member IgnoredProblemsAssertion exceptions then
                Nothing

              else
                Just (expectIgnoredProblems noIgnoredProblems)
            , if List.member DialogDetailsAssertion exceptions then
                Nothing

              else
                Just (expectDialogDetails NoDialog)
            ]
    in
    List.filterMap identity allMaybeAssertions


defaultAssertions : List (( Model, Command ) -> Expectation)
defaultAssertions =
    defaultAssertionsExcept []


singleTimer : Timers
singleTimer =
    case expectedParsedSampleTimerData of
        TimerData times ->
            Single (TimerFile "timer1.txt" "Name1") times


createBarcodeScannerDataForRemovingUnassociatedAthletes : List String -> Model
createBarcodeScannerDataForRemovingUnassociatedAthletes athletes =
    let
        fileLines : List BarcodeScannerFileLine
        fileLines =
            athletes
                |> List.indexedMap
                    (\index athlete ->
                        [ ordinaryFileLine (index * 2 + 1) athlete (Just (index + 1)) "14/03/2018 09:47:03"
                        , ordinaryFileLine (index * 2 + 2) athlete Nothing "14/03/2018 09:47:03"
                        ]
                    )
                |> List.concat
    in
    { initModel
        | barcodeScannerData = regenerate { empty | files = [ BarcodeScannerFile "barcodes1.txt" "Name1" fileLines Nothing ] }
        , problems = { noProblems | athletesWithAndWithoutPosition = List.indexedMap (\index athlete -> AthleteWithAndWithoutPositionProblem athlete 1 (index + 1)) athletes }
    }


deleteLinesWithinFile : (BarcodeScannerFileLine -> BarcodeScannerFileLine) -> List BarcodeScannerFile -> List BarcodeScannerFile
deleteLinesWithinFile deleter files =
    let
        deleteInFile : BarcodeScannerFile -> BarcodeScannerFile
        deleteInFile file =
            { file | lines = List.map deleter file.lines }
    in
    List.map deleteInFile files


ifAthleteBarcode : String -> BarcodeScannerFileLine -> BarcodeScannerFileLine
ifAthleteBarcode athlete line =
    case line.contents of
        Ordinary someAthlete Nothing ->
            if someAthlete == athlete then
                { line | deletionStatus = Deleted (AthleteScannedWithFinishTokenElsewhere athlete) }

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
                |> List.map (\num -> BarcodeScannerFile (num ++ ".txt") ("Name" ++ num) [] defaultDateTime)

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
        , describe "File dropping tests"
            [ describe "Barcode scanner file tests"
                [ test "Can drop a single barcode scanner file" <|
                    \() ->
                        let
                            droppedFilename : String
                            droppedFilename =
                                "vv_Scanner_JoeBLOGGS_1234567_20240714103842.txt"

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { parsedBarcodeScannerData1 | files = List.map (setNameInScannerFile droppedFilename "Joe BLOGGS") parsedBarcodeScannerData1.files }
                        in
                        update (FilesDropped [ InteropFile droppedFilename validBarcodeScannerData1 ]) initModel
                            |> Expect.all
                                (expectBarcodeScannerData expectedBarcodeScannerData
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                                )
                , test "Can drop a single barcode scanner file where the first line in the file is incomplete" <|
                    \() ->
                        let
                            droppedFilename : String
                            droppedFilename =
                                "vv_Scanner_JaneDOE_7654321_20240714104206.txt"

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                { parsedBarcodeScannerDataWithIncompleteRecordFirst
                                    | files = List.map (setNameInScannerFile droppedFilename "Jane DOE") parsedBarcodeScannerDataWithIncompleteRecordFirst.files
                                }
                        in
                        update (FilesDropped [ InteropFile droppedFilename validBarcodeScannerDataWithIncompleteRecordFirst ]) initModel
                            |> Expect.all
                                (expectBarcodeScannerData expectedBarcodeScannerData
                                    :: expectProblems { noProblems | athletesMissingPosition = [ "A2044293" ] }
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
                ]
            , describe "Timer file tests"
                [ test "Can drop a single timer file" <|
                    \() ->
                        let
                            droppedFilename : String
                            droppedFilename =
                                "vv_Stopwatch_JoeBLOGGS_1234567_20240714103842.csv"
                        in
                        update (FilesDropped [ InteropFile droppedFilename sampleTimerData ]) initModel
                            |> Expect.all
                                (expectTimers (Single (TimerFile droppedFilename "Joe BLOGGS") parsedTimerTimes1)
                                    :: defaultAssertionsExcept [ Timers ]
                                )
                ]
            ]
        , describe "File adding tests"
            [ describe "Barcode scanner file tests"
                [ test "Can add a single barcode scanner file" <|
                    \() ->
                        update (FilesAdded [ AddedFile "barcodes1.txt" "Name1" validBarcodeScannerData1 ]) initModel
                            |> Expect.all
                                (expectBarcodeScannerData parsedBarcodeScannerData1
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion ]
                                )
                , test "Can add a single barcode scanner file where the first line in the file is incomplete" <|
                    \() ->
                        update (FilesAdded [ AddedFile "barcodes1.txt" "Name1" validBarcodeScannerDataWithIncompleteRecordFirst ]) initModel
                            |> Expect.all
                                (expectBarcodeScannerData parsedBarcodeScannerDataWithIncompleteRecordFirst
                                    :: expectProblems { noProblems | athletesMissingPosition = [ "A2044293" ] }
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                                )
                ]
            , describe "Timer file tests"
                [ test "Can add a timer file" <|
                    \() ->
                        update (FilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ]) initModel
                            |> Expect.all
                                (expectTimers singleTimer
                                    :: defaultAssertionsExcept [ Timers ]
                                )
                ]
            ]
        , describe "Delete Timer tests"
            [ test "Deleting timer 1 when none to delete does nothing" <|
                \() ->
                    update (RemoveTimer TimerOne) initModel
                        |> Expect.all defaultAssertions
            , test "Deleting timer 2 when none to delete does nothing" <|
                \() ->
                    update (RemoveTimer TimerTwo) initModel
                        |> Expect.all defaultAssertions
            , test "Deleting timer 1 when one to delete deletes that timer" <|
                \() ->
                    { initModel | timers = singleTimer }
                        |> update (RemoveTimer TimerOne)
                        |> Expect.all defaultAssertions
            , test "Deleting timer 2 when one to delete does nothing" <|
                \() ->
                    { initModel | timers = singleTimer }
                        |> update (RemoveTimer TimerTwo)
                        |> Expect.all
                            (expectTimers singleTimer
                                :: defaultAssertionsExcept [ Timers ]
                            )
            , test "Deleting timer 1 when two to delete deletes timer 1" <|
                \() ->
                    { initModel | timers = doubleTimers }
                        |> update (RemoveTimer TimerOne)
                        |> Expect.all
                            (expectTimers (Single (TimerFile "timer2.txt" "Name2") parsedTimerTimes2)
                                :: defaultAssertionsExcept [ Timers ]
                            )
            , test "Deleting timer 2 when two to delete deletes timer 2" <|
                \() ->
                    { initModel | timers = doubleTimers }
                        |> update (RemoveTimer TimerTwo)
                        |> Expect.all
                            (expectTimers singleTimer
                                :: defaultAssertionsExcept [ Timers ]
                            )
            ]
        , describe "Flip Timer tests"
            [ test "Flipping timers when no timers loaded does nothing" <|
                \() ->
                    update FlipTimers initModel
                        |> Expect.all defaultAssertions
            , test "Flipping timers when one timer loaded does nothing" <|
                \() ->
                    { initModel | timers = singleTimer }
                        |> update FlipTimers
                        |> Expect.all
                            (expectTimers singleTimer
                                :: defaultAssertionsExcept [ Timers ]
                            )
            , test "Flipping timers when two timers loaded flips both timers" <|
                \() ->
                    { initModel | timers = doubleTimers }
                        |> update FlipTimers
                        |> Expect.all
                            (expectTimers flippedDoubleTimers
                                :: expectProblems { noProblems | timerTimeOffset = Just -283 }
                                :: defaultAssertionsExcept [ Timers, Problems ]
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
                        | barcodeScannerData = createBarcodeScannerData (Dict.singleton 47 [ "A4580484" ]) [ "A123456" ]
                        , timers = doubleTimers
                        , lastErrors = [ FileError "TEST_ERROR" "Some test error message" "somefile.txt" ]
                        , highlightedNumberCheckerId = Just 2
                        , numberCheckerEntries = [ AnnotatedNumberCheckerEntry 2 2 0 2 0 2 0 2 ]
                        , numberCheckerManualEntryRow = NumberCheckerManualEntryRow (IntegerEntry "2" (Just 2)) (IntegerEntry "2" (Just 2)) (IntegerEntry "2" (Just 2))
                        , problems =
                            { noProblems
                                | athletesWithAndWithoutPosition = [ AthleteWithAndWithoutPositionProblem "A123" 2 5 ]
                                , misScans = [ "something" ]
                            }
                        , ignoredProblems =
                            { noIgnoredProblems | ignoreTimerTimeOffsets = True }
                    }
                        |> update ClearAllData
                        |> Expect.all defaultAssertions
            ]
        , describe "Request current date and time tests"
            [ test "Requesting current date and time issues a command and returns the same model" <|
                \() ->
                    update (RequestCurrentDateAndTime Commands.DownloadMergedTimers) initModel
                        |> Expect.all
                            (expectCommand (GetCurrentDateAndTime Commands.DownloadMergedTimers)
                                :: defaultAssertionsExcept [ Command ]
                            )
            ]
        , describe "Download merged timer data tests"
            [ test "Does not download merged data for no timers" <|
                \() ->
                    initModel
                        |> update (DownloadMergedTimerData Time.utc recentTime)
                        |> Expect.all defaultAssertions
            , test "Does not download merged data for one timer" <|
                \() ->
                    initModel
                        |> update (FilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ])
                        |> Tuple.first
                        |> update (DownloadMergedTimerData Time.utc recentTime)
                        |> Expect.all
                            (expectTimers singleTimer
                                :: defaultAssertionsExcept [ Timers ]
                            )
            , test "Can download merged data for two timers" <|
                \() ->
                    initModel
                        |> update (FilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ])
                        |> Tuple.first
                        |> update (FilesAdded [ AddedFile "timer2.txt" "Name2" sampleTimerData2 ])
                        |> Tuple.first
                        |> update (DownloadMergedTimerData Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile timerFileMimeType (InteropFile "results_tidyup_timer_20170714024000.txt" expectedMergedTimerFileContents))
                                :: expectTimers doubleTimers
                                :: defaultAssertionsExcept [ Timers, Command ]
                            )
            ]
        , describe "Create single timer file for download tests"
            [ test "Cannot create a timer file for timer 1 when no timers" <|
                \() ->
                    initModel
                        |> update (DownloadTimer TimerOne Time.utc recentTime)
                        |> Expect.all defaultAssertions
            , test "Cannot create a timer file for timer 2 when no timers" <|
                \() ->
                    initModel
                        |> update (DownloadTimer TimerTwo Time.utc recentTime)
                        |> Expect.all defaultAssertions
            , test "Can create a timer file for timer 1 when single timer" <|
                \() ->
                    initModel
                        |> update (FilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ])
                        |> Tuple.first
                        |> update (DownloadTimer TimerOne Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile timerFileMimeType (InteropFile "results_tidyup_timer_20170714024000.txt" expectedDownloadedTimerData1))
                                :: expectTimers singleTimer
                                :: defaultAssertionsExcept [ Timers, Command ]
                            )
            , test "Cannot create a timer file for timer 2 when only one timer" <|
                \() ->
                    initModel
                        |> update (FilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ])
                        |> Tuple.first
                        |> update (DownloadTimer TimerTwo Time.utc recentTime)
                        |> Expect.all (defaultAssertionsExcept [ Timers ])
            , test "Can create a timer file for timer 1 when two timers uploaded" <|
                \() ->
                    initModel
                        |> update (FilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ])
                        |> Tuple.first
                        |> update (FilesAdded [ AddedFile "timer2.txt" "Name2" sampleTimerData2 ])
                        |> Tuple.first
                        |> update (DownloadTimer TimerOne Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile timerFileMimeType (InteropFile "results_tidyup_timer_20170714024000.txt" expectedDownloadedTimerData1))
                                :: expectTimers doubleTimers
                                :: defaultAssertionsExcept [ Timers, Command ]
                            )
            , test "Can create a timer file for timer 2 when two timers uploaded" <|
                \() ->
                    initModel
                        |> update (FilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ])
                        |> Tuple.first
                        |> update (FilesAdded [ AddedFile "timer2.txt" "Name2" sampleTimerData2 ])
                        |> Tuple.first
                        |> update (DownloadTimer TimerTwo Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile timerFileMimeType (InteropFile "results_tidyup_timer_20170714024000.txt" expectedDownloadedTimerData2))
                                :: expectTimers doubleTimers
                                :: defaultAssertionsExcept [ Timers, Command ]
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
            [ test "Can delete a number-checker row when no timers loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData }
                        |> update (DeleteNumberCheckerRow 2)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerDataWithSecondItemRemoved
                                :: defaultAssertionsExcept [ NumberCheckerEntries ]
                            )
            ]
        , describe "Number checker field changed tests"
            [ test "Entering a valid value for timer 1 sets the value" <|
                \() ->
                    update (NumberCheckerFieldChanged Timer1 "24") initModel
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (IntegerEntry "24" (Just 24)) emptyEntry emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a valid value for timer 2 sets the value" <|
                \() ->
                    update (NumberCheckerFieldChanged Timer2 "38") initModel
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
            , test "Entering a negative value for timer 1 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow (IntegerEntry "24" (Just 24)) emptyEntry emptyEntry }
                        |> update (NumberCheckerFieldChanged Timer1 "-2")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (IntegerEntry "-2" Nothing) emptyEntry emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering a negative value for timer 2 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyEntry (IntegerEntry "38" (Just 38)) emptyEntry }
                        |> update (NumberCheckerFieldChanged Timer2 "-3")
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
            , test "Entering an invalid value for timer 1 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow (IntegerEntry "24" (Just 24)) emptyEntry emptyEntry }
                        |> update (NumberCheckerFieldChanged Timer1 "nonsense")
                        |> Expect.all
                            (expectNumberCheckerManualEntryRow (NumberCheckerManualEntryRow (IntegerEntry "nonsense" Nothing) emptyEntry emptyEntry)
                                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Entering an invalid value for timer 2 sets a blank value" <|
                \() ->
                    { initModel | numberCheckerManualEntryRow = NumberCheckerManualEntryRow emptyEntry (IntegerEntry "38" (Just 38)) emptyEntry }
                        |> update (NumberCheckerFieldChanged Timer2 "nonsense")
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
                                  , timer1 = 12
                                  , timer1Delta = 0
                                  , timer2 = 12
                                  , timer2Delta = 0
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
            [ test "Can edit a number-checker row when no timers loaded" <|
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
            [ test "Can remove unassociated athlete" <|
                \() ->
                    let
                        initialModel : Model
                        initialModel =
                            createBarcodeScannerDataForRemovingUnassociatedAthletes [ "A447", "A1193762", "A4492701", "A2366890" ]

                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            initialModel.barcodeScannerData

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            { initialBarcodeScannerData
                                | athleteBarcodesOnly = List.filter (\x -> x.athlete /= "A1193762") initialBarcodeScannerData.athleteBarcodesOnly
                                , files = deleteLinesWithinFile (ifAthleteBarcode "A1193762") initialModel.barcodeScannerData.files
                            }
                    in
                    initialModel
                        |> update (FixProblem (RemoveUnassociatedAthlete "A1193762"))
                        |> Expect.all
                            (expectBarcodeScannerData expectedBarcodeScannerData
                                :: expectProblems
                                    { noProblems
                                        | athletesWithAndWithoutPosition =
                                            [ AthleteWithAndWithoutPositionProblem "A447" 1 1
                                            , AthleteWithAndWithoutPositionProblem "A4492701" 1 3
                                            , AthleteWithAndWithoutPositionProblem "A2366890" 1 4
                                            ]
                                    }
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems ]
                            )
            ]
        , describe "Ignoring problems tests"
            [ test "Can ignore timer-offsets problem" <|
                \() ->
                    let
                        expectedIgnoredProblems : IgnoredProblems
                        expectedIgnoredProblems =
                            { noIgnoredProblems | ignoreTimerTimeOffsets = True }

                        initialModel : Model
                        initialModel =
                            { initModel | problems = { noProblems | timerTimeOffset = Just 5 } }
                    in
                    update (IgnoreProblem IgnoreTimerTimeOffsets) initialModel
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
                            (expectCommand (DownloadFile barcodeScannerFileMimeType (InteropFile "results_tidyup_barcode_20170714024000.txt" ""))
                                :: expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ Command, BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile downloads file at middle index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile "2.txt" Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile barcodeScannerFileMimeType (InteropFile "results_tidyup_barcode_20170714024000.txt" ""))
                                :: expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2, 3 ])
                                :: defaultAssertionsExcept [ Command, BarcodeScannerDataAssertion ]
                            )
            , test "DownloadBarcodeScannerFile downloads file at last index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update (DownloadBarcodeScannerFile "3.txt" Time.utc recentTime)
                        |> Expect.all
                            (expectCommand (DownloadFile barcodeScannerFileMimeType (InteropFile "results_tidyup_barcode_20170714024000.txt" ""))
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
                                    "Name6"
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
                                    "Name6"
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
                                    "Name6"
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
                                    "Name6"
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
        , describe "OpenPasteFileDialog tests"
            [ test "Can open the paste file dialog" <|
                \() ->
                    update OpenPasteFileDialog initModel
                        |> Expect.all
                            (expectCommand (FocusElement PasteFileDialogTextArea)
                                :: expectDialogDetails (PasteFileDialog PastedFile.empty)
                                :: defaultAssertionsExcept [ Command, DialogDetailsAssertion ]
                            )
            ]
        , describe "PastedFileChanged tests"
            [ test "Can update the text" <|
                \() ->
                    let
                        expectedPastedFile : PastedFileDetails
                        expectedPastedFile =
                            PastedFileDetails timerFileContents (TimerFilePasted 3)
                    in
                    update (PastedFileChanged timerFileContents) initModel
                        |> Expect.all
                            (expectDialogDetails (PasteFileDialog expectedPastedFile)
                                :: defaultAssertionsExcept [ DialogDetailsAssertion ]
                            )
            ]
        , describe "PastedFileUploaded tests"
            [ test "Can upload the pasted file" <|
                \() ->
                    update (PastedFileUploaded sampleTimerData Time.utc recentTime) initModel
                        |> Expect.all
                            (expectTimers (Single (TimerFile "pasted_file_20170714024000.txt" "File pasted at 2017-07-14 02:40:00") parsedTimerTimes1)
                                :: defaultAssertionsExcept [ Timers ]
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
                                    "Name6"
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
                                    "Name6"
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



-- TODO: some tests with everything: timers, barcodes and number-checker data.
