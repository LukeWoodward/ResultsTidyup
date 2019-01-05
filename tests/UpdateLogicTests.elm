module UpdateLogicTests exposing (suite)

import BarcodeScanner exposing (AthleteAndTimePair, BarcodeScannerData)
import BarcodeScannerTests exposing (createBarcodeScannerData, expectSingleUnrecognisedLine, toPosix)
import DataStructures exposing (EventDateAndTime, InteropFile, WhichStopwatch(..))
import Dict
import Errors exposing (expectError)
import Expect exposing (Expectation)
import MergedTable exposing (MergedTableRow, Stopwatches(..), noUnderlines)
import Merger exposing (MergeEntry(..))
import Model exposing (Model, NumberCheckerManualEntryRow, NumericEntry, emptyNumberCheckerManualEntryRow, emptyNumericEntry, initModel)
import Msg exposing (Msg(..), NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import Problems exposing (Problem(..), ProblemsContainer)
import Stopwatch exposing (Stopwatch(..))
import StopwatchTests
import Test exposing (Test, describe, test)
import Time
import UpdateLogic exposing (createFileForDownload, update)


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
    case model.lastError of
        Just error ->
            Expect.equal expectedCode error.code

        Nothing ->
            Expect.fail ("Expected to fail with error " ++ expectedCode ++ ", but no error was present")


expectNumberCheckerEntries : List AnnotatedNumberCheckerEntry -> ( Model, Cmd Msg ) -> Expectation
expectNumberCheckerEntries expectedNumberCheckerEntries ( model, _ ) =
    Expect.equal expectedNumberCheckerEntries model.numberCheckerEntries


expectLastHeight : Maybe Int -> ( Model, Cmd Msg ) -> Expectation
expectLastHeight expectedLastHeight ( model, _ ) =
    Expect.equal expectedLastHeight model.lastHeight


expectHighlightedNumberCheckerId : Maybe Int -> ( Model, Cmd Msg ) -> Expectation
expectHighlightedNumberCheckerId expectedHighlightedNumberCheckerId ( model, _ ) =
    Expect.equal expectedHighlightedNumberCheckerId model.highlightedNumberCheckerId


expectBarcodeScannerFiles : List String -> ( Model, Cmd Msg ) -> Expectation
expectBarcodeScannerFiles expectedBarcodeScannerFiles ( model, _ ) =
    Expect.equal expectedBarcodeScannerFiles (List.sort model.barcodeScannerFiles)


expectBarcodeScannerData : BarcodeScannerData -> ( Model, Cmd Msg ) -> Expectation
expectBarcodeScannerData expectedBarcodeScannerData ( model, _ ) =
    Expect.equal expectedBarcodeScannerData model.barcodeScannerData


expectProblems : ProblemsContainer -> ( Model, Cmd Msg ) -> Expectation
expectProblems expectedProblems ( model, _ ) =
    Expect.equal expectedProblems model.problems


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
    | BarcodeScannerFiles
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
                Just (\( model, _ ) -> Expect.equal Nothing model.lastError)
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
            , if List.member BarcodeScannerFiles exceptions then
                Nothing

              else
                Just (expectBarcodeScannerFiles [])
            , if List.member BarcodeScannerDataAssertion exceptions then
                Nothing

              else
                Just (expectBarcodeScannerData BarcodeScanner.empty)
            , if List.member Problems exceptions then
                Nothing

              else
                Just (expectProblems Problems.empty)
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


sampleStopwatchData2 : String
sampleStopwatchData2 =
    "STARTOFEVENT,01/01/2001 00:00:00,klmnopqrst\n"
        ++ "0,01/01/2001 00:00:00\n"
        ++ "1,01/01/2001 00:03:11,00:03:11\n"
        ++ "2,01/01/2001 00:07:43,00:07:43\n"
        ++ "3,01/01/2001 00:10:11,00:10:11\n"
        ++ "ENDOFEVENT,01/01/2001 00:15:51\n"


parsedStopwatchTimes2 : List Int
parsedStopwatchTimes2 =
    [ 191, 463, 611 ]


doubleStopwatches : Stopwatches
doubleStopwatches =
    let
        expectedEntries : List MergedTableRow
        expectedEntries =
            [ { index = 0, rowNumber = Just 1, entry = ExactMatch 191, included = True, underlines = noUnderlines }
            , { index = 1, rowNumber = Just 2, entry = NearMatch 464 463, included = True, underlines = noUnderlines }
            , { index = 2, rowNumber = Just 3, entry = OneWatchOnly StopwatchOne 603, included = True, underlines = noUnderlines }
            , { index = 3, rowNumber = Just 4, entry = OneWatchOnly StopwatchTwo 611, included = True, underlines = noUnderlines }
            ]
    in
    Double "stopwatch1.txt" "stopwatch2.txt" expectedEntries


flippedDoubleStopwatches : Stopwatches
flippedDoubleStopwatches =
    let
        expectedEntries : List MergedTableRow
        expectedEntries =
            [ { index = 0, rowNumber = Just 1, entry = ExactMatch 191, included = True, underlines = noUnderlines }
            , { index = 1, rowNumber = Just 2, entry = NearMatch 463 464, included = True, underlines = noUnderlines }
            , { index = 2, rowNumber = Just 3, entry = OneWatchOnly StopwatchTwo 603, included = True, underlines = noUnderlines }
            , { index = 3, rowNumber = Just 4, entry = OneWatchOnly StopwatchOne 611, included = True, underlines = noUnderlines }
            ]
    in
    Double "stopwatch2.txt" "stopwatch1.txt" expectedEntries


validBarcodeScannerData1 : String
validBarcodeScannerData1 =
    "A4580442,P0047,14/03/2018 09:47:03"


validBarcodeScannerData2 : String
validBarcodeScannerData2 =
    "A2044293,P0059,14/03/2018 09:49:44"


invalidBarcodeScannerData : String
invalidBarcodeScannerData =
    "A4580442,P0000,14/03/2018 09:47:03"


parsedBarcodeScannerData1 : BarcodeScannerData
parsedBarcodeScannerData1 =
    BarcodeScannerData (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ]) [] [] [] (toPosix "2018-03-14T09:47:03.000Z")


parsedBarcodeScannerData1And2 : BarcodeScannerData
parsedBarcodeScannerData1And2 =
    BarcodeScannerData
        (Dict.fromList
            [ ( 47, [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ] )
            , ( 59, [ AthleteAndTimePair "A2044293" "14/03/2018 09:49:44" ] )
            ]
        )
        []
        []
        []
        (toPosix "2018-03-14T09:49:44.000Z")


parsedEventDateOnly : EventDateAndTime
parsedEventDateOnly =
    EventDateAndTime "14/03/2018" (toPosix "2018-03-14T00:00:00.000Z") "" Nothing


validNumberCheckerData : String
validNumberCheckerData =
    "5,4,5"


invalidNumberCheckerData : String
invalidNumberCheckerData =
    "1,2,3,4,5,6"


parsedNumberCheckerData : List AnnotatedNumberCheckerEntry
parsedNumberCheckerData =
    [ { entryNumber = 1
      , stopwatch1 = 5
      , stopwatch1Delta = 0
      , stopwatch2 = 4
      , stopwatch2Delta = -1
      , finishTokens = 5
      , finishTokensDelta = 0
      }
    ]


recentTime : Time.Posix
recentTime =
    Time.millisToPosix 1500000000000


expectedMergedStopwatchFileContents : String
expectedMergedStopwatchFileContents =
    "STARTOFEVENT,01/01/2001 00:00:00,junsd_stopwatch\u{000D}\n"
        ++ "0,01/01/2001 00:00:00\u{000D}\n"
        ++ "1,01/01/2001 00:03:11,00:03:11\u{000D}\n"
        ++ "2,01/01/2001 00:07:43,00:07:43\u{000D}\n"
        ++ "3,01/01/2001 00:10:03,00:10:03\u{000D}\n"
        ++ "4,01/01/2001 00:10:11,00:10:11\u{000D}\n"
        ++ "ENDOFEVENT,01/01/2001 01:59:59"


sampleNumberCheckerData : List AnnotatedNumberCheckerEntry
sampleNumberCheckerData =
    [ { entryNumber = 1
      , stopwatch1 = 5
      , stopwatch1Delta = 0
      , stopwatch2 = 4
      , stopwatch2Delta = -1
      , finishTokens = 5
      , finishTokensDelta = 0
      }
    , { entryNumber = 2
      , stopwatch1 = 11
      , stopwatch1Delta = 0
      , stopwatch2 = 10
      , stopwatch2Delta = 0
      , finishTokens = 11
      , finishTokensDelta = 0
      }
    , { entryNumber = 3
      , stopwatch1 = 18
      , stopwatch1Delta = 0
      , stopwatch2 = 17
      , stopwatch2Delta = 0
      , finishTokens = 17
      , finishTokensDelta = -1
      }
    ]


sampleNumberCheckerDataWithSecondItemRemoved : List AnnotatedNumberCheckerEntry
sampleNumberCheckerDataWithSecondItemRemoved =
    [ { entryNumber = 1
      , stopwatch1 = 5
      , stopwatch1Delta = 0
      , stopwatch2 = 4
      , stopwatch2Delta = -1
      , finishTokens = 5
      , finishTokensDelta = 0
      }
    , { entryNumber = 2
      , stopwatch1 = 18
      , stopwatch1Delta = 0
      , stopwatch2 = 17
      , stopwatch2Delta = 0
      , finishTokens = 17
      , finishTokensDelta = -1
      }
    ]


expectNoChangeForNumberCheckerManualEntryRow : NumberCheckerManualEntryRow -> Expectation
expectNoChangeForNumberCheckerManualEntryRow manualEntryRow =
    { initModel | numberCheckerManualEntryRow = manualEntryRow }
        |> update AddNumberCheckerRow
        |> Expect.all
            (expectNumberCheckerManualEntryRow manualEntryRow
                :: defaultAssertionsExcept [ NumberCheckerManualEntryRowAssertion ]
            )


createNumericEntry : Int -> NumericEntry
createNumericEntry value =
    NumericEntry (String.fromInt value) (Just value)


createNumberCheckerManualEntryRow : Int -> Int -> Int -> NumberCheckerManualEntryRow
createNumberCheckerManualEntryRow stopwatch1 stopwatch2 finishTokens =
    NumberCheckerManualEntryRow (createNumericEntry stopwatch1) (createNumericEntry stopwatch2) (createNumericEntry finishTokens)


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
                        update (FileDropped (InteropFile "empty.txt" "")) initModel
                            |> Expect.all (expectLastError "UNRECOGNISED_FILE" :: defaultAssertionsExcept [ LastError ])
                , test "Binary file should not match against any type" <|
                    \() ->
                        update (FileDropped (InteropFile "binary.txt" "\u{0000}\u{0001}")) initModel
                            |> Expect.all (expectLastError "UNRECOGNISED_FILE" :: defaultAssertionsExcept [ LastError ])
                , test "Unrecognised file should not match against any type" <|
                    \() ->
                        update (FileDropped (InteropFile "Unrecognised.txt" "This file contents should not be recognised")) initModel
                            |> Expect.all (expectLastError "UNRECOGNISED_FILE" :: defaultAssertionsExcept [ LastError ])
                ]
            , describe "Stopwatch file tests"
                [ test "Can upload a single stopwatch data file" <|
                    \() ->
                        update (FileDropped (InteropFile "stopwatch1.txt" StopwatchTests.sampleData)) initModel
                            |> Expect.all (expectStopwatches singleStopwatch :: defaultAssertionsExcept [ Stopwatches ])
                , test "Cannot upload a single invalid stopwatch data file" <|
                    \() ->
                        update (FileDropped (InteropFile "stopwatch1.txt" (String.replace "00" "XX" StopwatchTests.sampleData))) initModel
                            |> Expect.all (expectLastError "UNRECOGNISED_TIME" :: defaultAssertionsExcept [ LastError ])
                , test "Cannot upload the same single stopwatch data file twice" <|
                    \() ->
                        initModel
                            |> update (FileDropped (InteropFile "stopwatch1.txt" StopwatchTests.sampleData))
                            |> Tuple.first
                            |> update (FileDropped (InteropFile "stopwatch1.txt" StopwatchTests.sampleData))
                            |> Expect.all
                                (expectLastError "STOPWATCH_FILE_ALREADY_LOADED"
                                    :: expectStopwatches singleStopwatch
                                    :: defaultAssertionsExcept [ LastError, Stopwatches ]
                                )
                , test "Can upload two different stopwatch data files" <|
                    \() ->
                        initModel
                            |> update (FileDropped (InteropFile "stopwatch1.txt" StopwatchTests.sampleData))
                            |> Tuple.first
                            |> update (FileDropped (InteropFile "stopwatch2.txt" sampleStopwatchData2))
                            |> Expect.all
                                (expectStopwatches doubleStopwatches
                                    :: defaultAssertionsExcept [ Stopwatches ]
                                )
                , test "Uploading a third stopwatch data file has no effect" <|
                    \() ->
                        initModel
                            |> update (FileDropped (InteropFile "stopwatch1.txt" StopwatchTests.sampleData))
                            |> Tuple.first
                            |> update (FileDropped (InteropFile "stopwatch2.txt" sampleStopwatchData2))
                            |> Tuple.first
                            |> update (FileDropped (InteropFile "stopwatch3.txt" StopwatchTests.sampleData))
                            |> Expect.all
                                (expectStopwatches doubleStopwatches
                                    :: defaultAssertionsExcept [ Stopwatches ]
                                )
                ]
            , describe "Barcode scanner file tests"
                [ test "Can upload a single barcode scanner file" <|
                    \() ->
                        update (FileDropped (InteropFile "barcodes1.txt" validBarcodeScannerData1)) initModel
                            |> Expect.all
                                (expectBarcodeScannerFiles [ "barcodes1.txt" ]
                                    :: expectBarcodeScannerData parsedBarcodeScannerData1
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: defaultAssertionsExcept [ BarcodeScannerFiles, BarcodeScannerDataAssertion, EventDateAndTimeAssertion ]
                                )
                , test "Can upload a single barcode scanner file with position token without leading zeroes" <|
                    \() ->
                        update (FileDropped (InteropFile "barcodes1.txt" (String.replace "P0047" "P47" validBarcodeScannerData1))) initModel
                            |> Expect.all
                                (expectBarcodeScannerFiles [ "barcodes1.txt" ]
                                    :: expectBarcodeScannerData parsedBarcodeScannerData1
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: defaultAssertionsExcept [ BarcodeScannerFiles, BarcodeScannerDataAssertion, EventDateAndTimeAssertion ]
                                )
                , test "Can upload a single invalid barcode scanner file" <|
                    \() ->
                        update (FileDropped (InteropFile "barcodes1.txt" invalidBarcodeScannerData)) initModel
                            |> Expect.all
                                (expectBarcodeScannerFiles [ "barcodes1.txt" ]
                                    :: (\( model, _ ) ->
                                            expectSingleUnrecognisedLine invalidBarcodeScannerData "INVALID_POSITION_ZERO" (Ok model.barcodeScannerData)
                                       )
                                    :: expectProblems (ProblemsContainer [ UnrecognisedBarcodeScannerLine "A4580442,P0000,14/03/2018 09:47:03" ] [])
                                    :: defaultAssertionsExcept [ BarcodeScannerFiles, BarcodeScannerDataAssertion, EventDateAndTimeAssertion, Problems ]
                                )
                , test "Cannot upload the same barcode scanner file twice" <|
                    \() ->
                        initModel
                            |> update (FileDropped (InteropFile "barcodes1.txt" validBarcodeScannerData1))
                            |> Tuple.first
                            |> update (FileDropped (InteropFile "barcodes1.txt" validBarcodeScannerData1))
                            |> Expect.all
                                (expectBarcodeScannerFiles [ "barcodes1.txt" ]
                                    :: expectBarcodeScannerData parsedBarcodeScannerData1
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: expectLastError "BARCODE_DATA_ALREADY_LOADED"
                                    :: defaultAssertionsExcept [ BarcodeScannerFiles, BarcodeScannerDataAssertion, EventDateAndTimeAssertion, LastError ]
                                )
                , test "Can upload two different barcode scanner files" <|
                    \() ->
                        initModel
                            |> update (FileDropped (InteropFile "barcodes1.txt" validBarcodeScannerData1))
                            |> Tuple.first
                            |> update (FileDropped (InteropFile "barcodes2.txt" validBarcodeScannerData2))
                            |> Expect.all
                                (expectBarcodeScannerFiles [ "barcodes1.txt", "barcodes2.txt" ]
                                    :: expectBarcodeScannerData parsedBarcodeScannerData1And2
                                    :: expectEventDateAndTime parsedEventDateOnly
                                    :: defaultAssertionsExcept [ BarcodeScannerFiles, BarcodeScannerDataAssertion, EventDateAndTimeAssertion ]
                                )
                ]
            , describe "Number checker file tests"
                [ test "Can upload a single number checker file" <|
                    \() ->
                        update (FileDropped (InteropFile "numberChecker1.txt" validNumberCheckerData)) initModel
                            |> Expect.all
                                (expectNumberCheckerEntries parsedNumberCheckerData
                                    :: defaultAssertionsExcept [ NumberCheckerEntries ]
                                )
                , test "Cannot upload an invalid number checker file" <|
                    \() ->
                        update (FileDropped (InteropFile "numberChecker1.txt" invalidNumberCheckerData)) initModel
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
        , describe "Clear Barcode Scanner Data tests"
            [ test "Clearing barcode scanner data when none to clear does nothing" <|
                \() ->
                    update ClearBarcodeScannerData initModel
                        |> Expect.all defaultAssertions
            , test "Clearing barcode scanner data when some to clear clears it" <|
                \() ->
                    { initModel | barcodeScannerData = createBarcodeScannerData (Dict.singleton 47 [ "A4580484" ]) [ "A123456" ] [ 11 ] }
                        |> update ClearBarcodeScannerData
                        |> Expect.all defaultAssertions
            ]
        , describe "Get current date for download file tests"
            [ test "Getting current date issues a task and returns the same model" <|
                \() ->
                    update GetCurrentDateForDownloadFile initModel
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
                        |> update (FileDropped (InteropFile "stopwatch1.txt" StopwatchTests.sampleData))
                        |> Tuple.first
                        |> update (DownloadMergedStopwatchData Time.utc recentTime)
                        |> Expect.all
                            (expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches ]
                            )
            , test "Can download merged data for two stopwatches" <|
                \() ->
                    initModel
                        |> update (FileDropped (InteropFile "stopwatch1.txt" StopwatchTests.sampleData))
                        |> Tuple.first
                        |> update (FileDropped (InteropFile "stopwatch2.txt" sampleStopwatchData2))
                        |> Tuple.first
                        |> update (DownloadMergedStopwatchData Time.utc recentTime)
                        |> Expect.all
                            (expectACommand
                                :: expectStopwatches doubleStopwatches
                                :: defaultAssertionsExcept [ Stopwatches, Command ]
                            )
            ]
        , describe "Create file for download tests"
            [ test "Can create a file for download" <|
                \() ->
                    let
                        model : Model
                        model =
                            initModel
                                |> update (FileDropped (InteropFile "stopwatch1.txt" StopwatchTests.sampleData))
                                |> Tuple.first
                                |> update (FileDropped (InteropFile "stopwatch2.txt" sampleStopwatchData2))
                                |> Tuple.first
                    in
                    case model.stopwatches of
                        Double _ _ mergedTableRows ->
                            createFileForDownload Time.utc recentTime mergedTableRows
                                |> Expect.equal (InteropFile "parkrun_timer_14072017024000.txt" expectedMergedStopwatchFileContents)

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
            , test "Deleting a non-existent number-checker row has no effect when no stopwatches loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData }
                        |> update (DeleteNumberCheckerRow 7)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerData
                                :: defaultAssertionsExcept [ NumberCheckerEntries ]
                            )
            , test "Can delete a number-checker row when one stopwatch loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData, stopwatches = singleStopwatch }
                        |> update (DeleteNumberCheckerRow 2)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerDataWithSecondItemRemoved
                                :: expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches, NumberCheckerEntries ]
                            )
            , test "Deleting a non-existent number-checker row has no effect when one stopwatch loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData, stopwatches = singleStopwatch }
                        |> update (DeleteNumberCheckerRow 7)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerData
                                :: expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches, NumberCheckerEntries ]
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
            , test "Setting a nonexistent date clears the validated date" <|
                \() ->
                    update (EventDateChanged "29/02/2018") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime "29/02/2018" Nothing "" Nothing)
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            , test "Setting an invalid date clears the validated date" <|
                \() ->
                    update (EventDateChanged "This is not a valid date") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime "This is not a valid date" Nothing "" Nothing)
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            , test "Setting an empty date clears the validated date" <|
                \() ->
                    update (EventDateChanged "") initModel
                        |> Expect.all defaultAssertions
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
            , test "Setting a nonexistent time clears the validated time" <|
                \() ->
                    update (EventTimeChanged "25:30") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime "" Nothing "25:30" Nothing)
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            , test "Setting an invalid time clears the validated time" <|
                \() ->
                    update (EventTimeChanged "This is not a valid time") initModel
                        |> Expect.all
                            (expectEventDateAndTime (EventDateAndTime "" Nothing "This is not a valid time" Nothing)
                                :: defaultAssertionsExcept [ EventDateAndTimeAssertion ]
                            )
            , test "Setting an empty time clears the validated time" <|
                \() ->
                    update (EventTimeChanged "") initModel
                        |> Expect.all defaultAssertions
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
            [ test "Cannot enter a number-checker row with no valid entries" <|
                \() ->
                    expectNoChangeForNumberCheckerManualEntryRow emptyNumberCheckerManualEntryRow
            , test "Cannot enter a number-checker row with only a valid value for stopwatch 1" <|
                \() ->
                    NumberCheckerManualEntryRow (NumericEntry "24" (Just 24)) emptyNumericEntry emptyNumericEntry
                        |> expectNoChangeForNumberCheckerManualEntryRow
            , test "Cannot enter a number-checker row with only a valid value for stopwatch 2" <|
                \() ->
                    NumberCheckerManualEntryRow emptyNumericEntry (NumericEntry "38" (Just 38)) emptyNumericEntry
                        |> expectNoChangeForNumberCheckerManualEntryRow
            , test "Cannot enter a number-checker row with only a valid value for stopwatches 1 and 2" <|
                \() ->
                    NumberCheckerManualEntryRow (NumericEntry "24" (Just 24)) (NumericEntry "38" (Just 38)) emptyNumericEntry
                        |> expectNoChangeForNumberCheckerManualEntryRow
            , test "Cannot enter a number-checker row with only a valid value for finish tokens" <|
                \() ->
                    NumberCheckerManualEntryRow emptyNumericEntry emptyNumericEntry (NumericEntry "17" (Just 17))
                        |> expectNoChangeForNumberCheckerManualEntryRow
            , test "Cannot enter a number-checker row with only a valid value for stopwatch 1 and finish tokens" <|
                \() ->
                    NumberCheckerManualEntryRow (NumericEntry "24" (Just 24)) emptyNumericEntry (NumericEntry "17" (Just 17))
                        |> expectNoChangeForNumberCheckerManualEntryRow
            , test "Cannot enter a number-checker row with only a valid value for stopwatch 2 and finish tokens" <|
                \() ->
                    NumberCheckerManualEntryRow emptyNumericEntry (NumericEntry "38" (Just 38)) (NumericEntry "17" (Just 17))
                        |> expectNoChangeForNumberCheckerManualEntryRow
            , test "Can enter a number-checker row with all valid values" <|
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
                                  }
                                ]
                                :: expectACommand
                                :: defaultAssertionsExcept [ Command, NumberCheckerEntries ]
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
            , test "Editing a non-existent number-checker row has no effect when no stopwatches loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData }
                        |> update (EditNumberCheckerRow 7)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerData
                                :: defaultAssertionsExcept [ NumberCheckerEntries ]
                            )
            , test "Can edit a number-checker row when one stopwatch loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData, stopwatches = singleStopwatch }
                        |> update (EditNumberCheckerRow 2)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerDataWithSecondItemRemoved
                                :: expectNumberCheckerManualEntryRow (createNumberCheckerManualEntryRow 11 10 11)
                                :: expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches, NumberCheckerEntries, NumberCheckerManualEntryRowAssertion ]
                            )
            , test "Editing a non-existent number-checker row has no effect when one stopwatch loaded" <|
                \() ->
                    { initModel | numberCheckerEntries = sampleNumberCheckerData, stopwatches = singleStopwatch }
                        |> update (EditNumberCheckerRow 7)
                        |> Expect.all
                            (expectNumberCheckerEntries sampleNumberCheckerData
                                :: expectStopwatches singleStopwatch
                                :: defaultAssertionsExcept [ Stopwatches, NumberCheckerEntries ]
                            )
            ]
        ]



-- TODO: some tests with everything: stopwatches, barcodes and number-checker data.
