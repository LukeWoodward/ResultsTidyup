module FileDropHandlingTests exposing (suite)

import Expect exposing (Expectation)
import FileDropHandling exposing (handleFilesDropped)
import FileHandling exposing (InteropFile)
import Model exposing (Model, initModel)
import Stopwatch exposing (Stopwatch(..))
import Test exposing (Test, describe, test)
import TestData exposing (..)


expectLastError : String -> Model -> Expectation
expectLastError expectedCode model =
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


runTestWithSingleError : Model -> String -> String -> String -> Expectation
runTestWithSingleError startingModel fileName fileContents expectedErrorCode =
    let
        actualModel : Model
        actualModel =
            handleFilesDropped [ InteropFile fileName fileContents ] startingModel
    in
    Expect.all
        [ Expect.equal { startingModel | lastErrors = actualModel.lastErrors }
        , expectLastError expectedErrorCode
        ]
        actualModel


suite : Test
suite =
    describe "FileDropHandling tests"
        [ describe "File drop tests"
            [ describe "Invalid file tests"
                [ test "Empty file should not match against any type" <|
                    \() ->
                        runTestWithSingleError initModel "empty.txt" "" "UNRECOGNISED_FILE"
                , test "Binary file should not match against any type" <|
                    \() ->
                        runTestWithSingleError initModel "binary.txt" "\u{0000}\u{0001}" "UNRECOGNISED_FILE"
                , test "Unrecognised file should not match against any type" <|
                    \() ->
                        runTestWithSingleError initModel "unrecognised.txt" "This file contents should not be recognised" "UNRECOGNISED_FILE"
                ]
            , describe "Stopwatch file tests"
                [ test "Can upload a single stopwatch data file" <|
                    \() ->
                        handleFilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ] initModel
                            |> Expect.equal { initModel | stopwatches = singleStopwatch }
                , test "Cannot upload a single invalid stopwatch data file" <|
                    \() ->
                        runTestWithSingleError initModel "stopwatch1.txt" (String.replace "00" "XX" sampleStopwatchData) "UNRECOGNISED_TIME"
                , test "Cannot upload the same single stopwatch data file twice" <|
                    \() ->
                        let
                            intermediateModel : Model
                            intermediateModel =
                                handleFilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ] initModel
                        in
                        runTestWithSingleError intermediateModel "stopwatch1.txt" sampleStopwatchData "STOPWATCH_FILE_ALREADY_LOADED"
                , test "Can upload two different stopwatch data files in alphabetical order dropped together" <|
                    \() ->
                        initModel
                            |> handleFilesDropped
                                [ InteropFile "stopwatch1.txt" sampleStopwatchData
                                , InteropFile "stopwatch2.txt" sampleStopwatchData2
                                ]
                            |> Expect.equal { initModel | stopwatches = doubleStopwatches }
                , test "Can upload two different stopwatch data files in alphabetical order dropped one after the other" <|
                    \() ->
                        initModel
                            |> handleFilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ]
                            |> handleFilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ]
                            |> Expect.equal { initModel | stopwatches = doubleStopwatches }
                , test "Can upload two different stopwatch data files in reverse alphabetical order dropped one after the other" <|
                    \() ->
                        initModel
                            |> handleFilesDropped [ InteropFile "stopwatch2.txt" sampleStopwatchData2 ]
                            |> handleFilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ]
                            |> Expect.equal { initModel | stopwatches = doubleStopwatches }
                , test "Uploading a third stopwatch data file has no effect" <|
                    \() ->
                        let
                            intermediateModel : Model
                            intermediateModel =
                                handleFilesDropped
                                    [ InteropFile "stopwatch1.txt" sampleStopwatchData
                                    , InteropFile "stopwatch2.txt" sampleStopwatchData2
                                    ]
                                    initModel
                        in
                        Expect.equal
                            intermediateModel
                            (handleFilesDropped [ InteropFile "stopwatch3.txt" sampleStopwatchData ] intermediateModel)
                , test "Uploading a valid stopwatch file doesn't delete errors from an invalid stopwatch earlier in the same upload" <|
                    \() ->
                        let
                            actualModel : Model
                            actualModel =
                                initModel
                                    |> handleFilesDropped
                                        [ InteropFile "stopwatch2.txt" (String.replace "00" "XX" sampleStopwatchData2)
                                        , InteropFile "stopwatch1.txt" sampleStopwatchData
                                        ]
                        in
                        Expect.all
                            [ Expect.equal { initModel | stopwatches = singleStopwatch, lastErrors = actualModel.lastErrors }
                            , expectLastError "UNRECOGNISED_TIME"
                            ]
                            actualModel
                , test "Uploading a valid stopwatch file doesn't delete errors from an invalid stopwatch in a previous upload" <|
                    \() ->
                        let
                            actualModel : Model
                            actualModel =
                                initModel
                                    |> handleFilesDropped [ InteropFile "stopwatch2.txt" (String.replace "00" "XX" sampleStopwatchData2) ]
                                    |> handleFilesDropped [ InteropFile "stopwatch1.txt" sampleStopwatchData ]
                        in
                        Expect.all
                            [ Expect.equal { initModel | stopwatches = singleStopwatch, lastErrors = actualModel.lastErrors }
                            , expectLastError "UNRECOGNISED_TIME"
                            ]
                            actualModel
                ]
            , describe "Barcode scanner file tests"
                [ test "Can upload a single barcode scanner file" <|
                    \() ->
                        handleFilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerData1 ] initModel
                            |> Expect.equal { initModel | eventDateAndTime = parsedEventDateOnly, barcodeScannerData = parsedBarcodeScannerData1 }
                , test "Can upload a single barcode scanner file with position token without leading zeroes" <|
                    \() ->
                        handleFilesDropped [ InteropFile "barcodes1.txt" (String.replace "P0047" "P47" validBarcodeScannerData1) ] initModel
                            |> Expect.equal { initModel | eventDateAndTime = parsedEventDateOnly, barcodeScannerData = parsedBarcodeScannerData1 }
                , test "Can upload a single barcode scanner file where the first line in the file is incomplete" <|
                    \() ->
                        handleFilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerDataWithIncompleteRecordFirst ] initModel
                            |> Expect.equal { initModel | eventDateAndTime = parsedEventDateOnly, barcodeScannerData = parsedBarcodeScannerDataWithIncompleteRecordFirst }
                , test "Can upload a single invalid barcode scanner file" <|
                    \() ->
                        handleFilesDropped [ InteropFile "invalid.txt" invalidBarcodeScannerData ] initModel
                            |> Expect.equal { initModel | barcodeScannerData = parsedInvalidBarcodeScannerData }
                , test "Cannot upload the same barcode scanner file twice" <|
                    \() ->
                        let
                            intermediateModel : Model
                            intermediateModel =
                                handleFilesDropped [ InteropFile "barcodes1.txt" validBarcodeScannerData1 ] initModel
                        in
                        runTestWithSingleError intermediateModel "barcodes1.txt" validBarcodeScannerData1 "BARCODE_DATA_ALREADY_LOADED"
                , test "Can upload two different barcode scanner files" <|
                    \() ->
                        handleFilesDropped
                            [ InteropFile "barcodes1.txt" validBarcodeScannerData1
                            , InteropFile "barcodes2.txt" validBarcodeScannerData2
                            ]
                            initModel
                            |> Expect.equal { initModel | eventDateAndTime = parsedEventDateOnly, barcodeScannerData = parsedBarcodeScannerData1And2 }
                ]
            , describe "Number checker file tests"
                [ test "Can upload a single number checker file" <|
                    \() ->
                        handleFilesDropped [ InteropFile "numberChecker1.txt" validNumberCheckerData ] initModel
                            |> Expect.equal { initModel | numberCheckerEntries = parsedNumberCheckerData }
                , test "Cannot upload an invalid number checker file" <|
                    \() ->
                        runTestWithSingleError initModel "numberChecker1.txt" invalidNumberCheckerData "WRONG_PART_COUNT"
                ]
            ]
        ]
