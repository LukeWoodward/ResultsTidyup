module FileDropHandlingTests exposing (suite)

import Expect exposing (Expectation)
import FileDropHandling exposing (handleFilesAdded)
import FileHandling exposing (AddedFile)
import Model exposing (Model, initModel)
import Test exposing (Test, describe, test)
import TestData exposing (..)
import Timer exposing (Timer(..))


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
            handleFilesAdded [ AddedFile fileName "Name" fileContents ] startingModel
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
            , describe "Timer file tests"
                [ test "Can upload a single timer data file" <|
                    \() ->
                        handleFilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ] initModel
                            |> Expect.equal { initModel | timers = singleTimer }
                , test "Can upload a single downloaded timer data file" <|
                    \() ->
                        handleFilesAdded [ AddedFile "timer1.txt" "Name1" sampleDownloadedTimerData ] initModel
                            |> Expect.equal { initModel | timers = singleTimer }
                , test "Cannot upload a single invalid timer data file" <|
                    \() ->
                        runTestWithSingleError initModel "timer1.txt" (String.replace "00" "XX" sampleTimerData) "UNRECOGNISED_TIME"
                , test "Cannot upload the same single timer data file twice" <|
                    \() ->
                        let
                            intermediateModel : Model
                            intermediateModel =
                                handleFilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ] initModel
                        in
                        runTestWithSingleError intermediateModel "timer1.txt" sampleTimerData "TIMER_FILE_ALREADY_LOADED"
                , test "Can upload two different timer data files in alphabetical order dropped together" <|
                    \() ->
                        initModel
                            |> handleFilesAdded
                                [ AddedFile "timer1.txt" "Name1" sampleTimerData
                                , AddedFile "timer2.txt" "Name2" sampleTimerData2
                                ]
                            |> Expect.equal { initModel | timers = doubleTimers }
                , test "Can upload two different timer data files in alphabetical order dropped one after the other" <|
                    \() ->
                        initModel
                            |> handleFilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ]
                            |> handleFilesAdded [ AddedFile "timer2.txt" "Name2" sampleTimerData2 ]
                            |> Expect.equal { initModel | timers = doubleTimers }
                , test "Can upload two different timer data files in reverse alphabetical order dropped one after the other" <|
                    \() ->
                        initModel
                            |> handleFilesAdded [ AddedFile "timer2.txt" "Name2" sampleTimerData2 ]
                            |> handleFilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ]
                            |> Expect.equal { initModel | timers = doubleTimers }
                , test "Uploading a third timer data file has no effect" <|
                    \() ->
                        let
                            intermediateModel : Model
                            intermediateModel =
                                handleFilesAdded
                                    [ AddedFile "timer1.txt" "Name1" sampleTimerData
                                    , AddedFile "timer2.txt" "Name2" sampleTimerData2
                                    ]
                                    initModel
                        in
                        Expect.equal
                            intermediateModel
                            (handleFilesAdded [ AddedFile "timer3.txt" "Name3" sampleTimerData ] intermediateModel)
                , test "Uploading a valid timer file doesn't delete errors from an invalid timer earlier in the same upload" <|
                    \() ->
                        let
                            actualModel : Model
                            actualModel =
                                initModel
                                    |> handleFilesAdded
                                        [ AddedFile "timer2.txt" "Name2" (String.replace "00" "XX" sampleTimerData2)
                                        , AddedFile "timer1.txt" "Name1" sampleTimerData
                                        ]
                        in
                        Expect.all
                            [ Expect.equal { initModel | timers = singleTimer, lastErrors = actualModel.lastErrors }
                            , expectLastError "UNRECOGNISED_TIME"
                            ]
                            actualModel
                , test "Uploading a valid timer file doesn't delete errors from an invalid timer in a previous upload" <|
                    \() ->
                        let
                            actualModel : Model
                            actualModel =
                                initModel
                                    |> handleFilesAdded [ AddedFile "timer2.txt" "Name2" (String.replace "00" "XX" sampleTimerData2) ]
                                    |> handleFilesAdded [ AddedFile "timer1.txt" "Name1" sampleTimerData ]
                        in
                        Expect.all
                            [ Expect.equal { initModel | timers = singleTimer, lastErrors = actualModel.lastErrors }
                            , expectLastError "UNRECOGNISED_TIME"
                            ]
                            actualModel
                ]
            , describe "Barcode scanner file tests"
                [ test "Can upload a single barcode scanner file" <|
                    \() ->
                        handleFilesAdded [ AddedFile "barcodes1.txt" "Name1" validBarcodeScannerData1 ] initModel
                            |> Expect.equal { initModel | barcodeScannerData = parsedBarcodeScannerData1 }
                , test "Can upload a single barcode scanner file with position token without leading zeroes" <|
                    \() ->
                        handleFilesAdded [ AddedFile "barcodes1.txt" "Name1" (String.replace "P0047" "P47" validBarcodeScannerData1) ] initModel
                            |> Expect.equal { initModel | barcodeScannerData = parsedBarcodeScannerData1 }
                , test "Can upload a single barcode scanner file where the first line in the file is incomplete" <|
                    \() ->
                        handleFilesAdded [ AddedFile "barcodes1.txt" "Name1" validBarcodeScannerDataWithIncompleteRecordFirst ] initModel
                            |> Expect.equal { initModel | barcodeScannerData = parsedBarcodeScannerDataWithIncompleteRecordFirst }
                , test "Can upload a single invalid barcode scanner file" <|
                    \() ->
                        handleFilesAdded [ AddedFile "invalid.txt" "Invalid" invalidBarcodeScannerData ] initModel
                            |> Expect.equal { initModel | barcodeScannerData = parsedInvalidBarcodeScannerData }
                , test "Cannot upload the same barcode scanner file twice" <|
                    \() ->
                        let
                            intermediateModel : Model
                            intermediateModel =
                                handleFilesAdded [ AddedFile "barcodes1.txt" "Name1" validBarcodeScannerData1 ] initModel
                        in
                        runTestWithSingleError intermediateModel "barcodes1.txt" validBarcodeScannerData1 "BARCODE_DATA_ALREADY_LOADED"
                , test "Can upload two different barcode scanner files" <|
                    \() ->
                        handleFilesAdded
                            [ AddedFile "barcodes1.txt" "Name1" validBarcodeScannerData1
                            , AddedFile "barcodes2.txt" "Name2" validBarcodeScannerData2
                            ]
                            initModel
                            |> Expect.equal { initModel | barcodeScannerData = parsedBarcodeScannerData1And2 }
                , test "Cannot upload an invalid barcode scanner file" <|
                    \() ->
                        runTestWithSingleError
                            initModel
                            "barcodes1plusbinary.txt"
                            (validBarcodeScannerData1 ++ "\u{0000}\u{0000}\u{0000}Z\u{0001}j\u{0007}\u{0000}\u{0003}\u{0000}$\u{0000}")
                            "BINARY_FILE"
                ]
            , describe "Number checker file tests"
                [ test "Can upload a single number checker file" <|
                    \() ->
                        handleFilesAdded [ AddedFile "numberChecker1.txt" "Name1" validNumberCheckerData ] initModel
                            |> Expect.equal { initModel | numberCheckerEntries = parsedNumberCheckerData }
                , test "Cannot upload an invalid number checker file" <|
                    \() ->
                        runTestWithSingleError initModel "numberChecker1.txt" invalidNumberCheckerData "WRONG_PART_COUNT"
                ]
            ]
        ]
