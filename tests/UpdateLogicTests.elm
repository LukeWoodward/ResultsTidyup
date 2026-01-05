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
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails, BarcodeScannerRowEditLocation)
import BarcodeScannerTests exposing (createBarcodeScannerData)
import Commands exposing (Command(..), ElementToFocus(..))
import DataEntry exposing (IntegerEntry)
import Dict
import Error exposing (FileError)
import Expect exposing (Expectation)
import FileHandling exposing (AddedFile, InteropFile)
import Model
    exposing
        ( DialogDetails(..)
        , Model
        , initModel
        )
import Msg exposing (Msg(..))
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


expectBarcodeScannerData : BarcodeScannerData -> ( Model, Command ) -> Expectation
expectBarcodeScannerData expectedBarcodeScannerData ( model, _ ) =
    Expect.equal expectedBarcodeScannerData model.barcodeScannerData


expectProblems : Problems -> ( Model, Command ) -> Expectation
expectProblems expectedProblems ( model, _ ) =
    Expect.equal expectedProblems model.problems


expectIgnoredProblems : IgnoredProblems -> ( Model, Command ) -> Expectation
expectIgnoredProblems ignoredProblems ( model, _ ) =
    Expect.equal ignoredProblems model.ignoredProblems


expectDialogDetails : DialogDetails -> ( Model, Command ) -> Expectation
expectDialogDetails dialogDetails ( model, _ ) =
    Expect.equal dialogDetails model.dialogDetails


expectBarcodeScannerTab : Maybe Int -> ( Model, Command ) -> Expectation
expectBarcodeScannerTab barcodeScannerTab ( model, _ ) =
    Expect.equal barcodeScannerTab model.barcodeScannerTab


setNameInScannerFile : String -> String -> BarcodeScannerFile -> BarcodeScannerFile
setNameInScannerFile filename name file =
    { file | filename = filename, name = name }


type Assertion
    = Command
    | Timers
    | LastError
    | BarcodeScannerDataAssertion
    | Problems
    | IgnoredProblemsAssertion
    | DialogDetailsAssertion
    | BarcodeScannerTabAssertion


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
            , if List.member BarcodeScannerDataAssertion exceptions then
                Nothing

              else
                Just (expectBarcodeScannerData empty)
            , if List.member Problems exceptions then
                Nothing

              else
                Just (expectProblems noProblems)
            , if List.member IgnoredProblemsAssertion exceptions then
                Nothing

              else
                Just (expectIgnoredProblems noIgnoredProblems)
            , if List.member DialogDetailsAssertion exceptions then
                Nothing

              else
                Just (expectDialogDetails NoDialog)
            , if List.member BarcodeScannerTabAssertion exceptions then
                Nothing

              else
                Just (expectBarcodeScannerTab Nothing)
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
        | barcodeScannerData = regenerate { empty | files = [ BarcodeScannerFile "barcodes1.txt" "Name1" fileLines ] }
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
                |> List.map (\num -> BarcodeScannerFile (num ++ ".txt") ("Name" ++ num) [])
    in
    { empty | files = files }


makeBarcodeScannerRowEditDetails : BarcodeScannerRowEditLocation -> Maybe Int -> Maybe Int -> BarcodeScannerRowEditDetails
makeBarcodeScannerRowEditDetails location athlete finishPosition =
    BarcodeScannerRowEditDetails
        location
        (Ordinary "" Nothing)
        (IntegerEntry "" athlete)
        (IntegerEntry "" finishPosition)
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
                                    :: expectBarcodeScannerTab (Just 0)
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, BarcodeScannerTabAssertion ]
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
                                    :: expectBarcodeScannerTab (Just 0)
                                    :: expectProblems { noProblems | athletesMissingPosition = [ "A2044293" ] }
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems, BarcodeScannerTabAssertion ]
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
                                    :: expectBarcodeScannerTab (Just 0)
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, BarcodeScannerTabAssertion ]
                                )
                , test "Can add a single barcode scanner file where the first line in the file is incomplete" <|
                    \() ->
                        update (FilesAdded [ AddedFile "barcodes1.txt" "Name1" validBarcodeScannerDataWithIncompleteRecordFirst ]) initModel
                            |> Expect.all
                                (expectBarcodeScannerData parsedBarcodeScannerDataWithIncompleteRecordFirst
                                    :: expectBarcodeScannerTab (Just 0)
                                    :: expectProblems { noProblems | athletesMissingPosition = [ "A2044293" ] }
                                    :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, Problems, BarcodeScannerTabAssertion ]
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
                        , problems =
                            { noProblems
                                | athletesWithAndWithoutPosition = [ AthleteWithAndWithoutPositionProblem "A123" 2 5 ]
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
                    update RemoveCurrentBarcodeScannerFile initModel
                        |> Expect.all defaultAssertions
            , test "RemoveBarcodeScannerFile deletes single file" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1 ], barcodeScannerTab = Just 0 }
                        |> update RemoveCurrentBarcodeScannerFile
                        |> Expect.all defaultAssertions
            , test "RemoveBarcodeScannerFile deletes file at first index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ], barcodeScannerTab = Just 0 }
                        |> update RemoveCurrentBarcodeScannerFile
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 2, 3 ])
                                :: expectBarcodeScannerTab (Just 0)
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, BarcodeScannerTabAssertion ]
                            )
            , test "RemoveBarcodeScannerFile deletes file at middle index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ], barcodeScannerTab = Just 1 }
                        |> update RemoveCurrentBarcodeScannerFile
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 3 ])
                                :: expectBarcodeScannerTab (Just 1)
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, BarcodeScannerTabAssertion ]
                            )
            , test "RemoveBarcodeScannerFile deletes file at last index of three" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ], barcodeScannerTab = Just 2 }
                        |> update RemoveCurrentBarcodeScannerFile
                        |> Expect.all
                            (expectBarcodeScannerData (getBarcodeScannerDataWithFiles [ 1, 2 ])
                                :: expectBarcodeScannerTab (Just 1)
                                :: defaultAssertionsExcept [ BarcodeScannerDataAssertion, BarcodeScannerTabAssertion ]
                            )
            , test "RemoveBarcodeScannerFile does nothing with no current scanner file" <|
                \() ->
                    { initModel | barcodeScannerData = getBarcodeScannerDataWithFiles [ 1, 2, 3 ] }
                        |> update RemoveCurrentBarcodeScannerFile
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
        , describe "OpenConfirmClearEverythingDialog tests"
            [ test "Can open the clear-everything confirmation dialog" <|
                \() ->
                    update OpenConfirmClearEverythingDialog initModel
                        |> Expect.all
                            (expectDialogDetails ConfirmClearEverythingDialog
                                :: defaultAssertionsExcept [ DialogDetailsAssertion ]
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
