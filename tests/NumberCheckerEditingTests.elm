module NumberCheckerEditingTests exposing (suite)

import Expect exposing (Expectation)
import Model exposing (Model, NumberCheckerManualEntryRow, emptyNumberCheckerManualEntryRow, initModel)
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import NumberCheckerEditing exposing (addNumberCheckerRow, deleteNumberCheckerEntry, editNumberCheckerRow, modifyNumberCheckerRows)
import NumericEntry exposing (NumericEntry, emptyNumericEntry)
import Test exposing (Test, describe, test)
import TestData exposing (..)


initialModel : Model
initialModel =
    { initModel | numberCheckerEntries = sampleNumberCheckerData }


expectNoChangeForNumberCheckerManualEntryRow : NumberCheckerManualEntryRow -> Expectation
expectNoChangeForNumberCheckerManualEntryRow manualEntryRow =
    { initModel | numberCheckerManualEntryRow = manualEntryRow }
        |> addNumberCheckerRow
        |> Expect.equal ( { initModel | numberCheckerManualEntryRow = manualEntryRow }, False )


suite : Test
suite =
    describe "NumberCheckerEditing tests"
        [ describe "addNumberCheckerRow row tests"
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
                    let
                        expectedNumberCheckerEntries : List AnnotatedNumberCheckerEntry
                        expectedNumberCheckerEntries =
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
                    in
                    { initModel | numberCheckerManualEntryRow = createNumberCheckerManualEntryRow 12 12 12 }
                        |> addNumberCheckerRow
                        |> Expect.equal ( { initModel | numberCheckerEntries = expectedNumberCheckerEntries }, True )
            ]
        , describe "editNumberCheckerRow tests"
            [ test "Can edit a number-checker row" <|
                \() ->
                    initialModel
                        |> editNumberCheckerRow 2
                        |> Expect.equal
                            { initModel
                                | numberCheckerEntries = sampleNumberCheckerDataWithSecondItemRemoved
                                , numberCheckerManualEntryRow = createNumberCheckerManualEntryRow 11 10 11
                            }
            , test "Editing a non-existent number-checker row has no effect" <|
                \() ->
                    Expect.equal initialModel (editNumberCheckerRow 7 initialModel)
            ]
        , describe "deleteNumberCheckerEntry tests"
            [ test "Can delete a number-checker row" <|
                \() ->
                    initialModel
                        |> deleteNumberCheckerEntry 2
                        |> Expect.equal { initModel | numberCheckerEntries = sampleNumberCheckerDataWithSecondItemRemoved }
            , test "Deleting a non-existent number-checker row has no effect when no stopwatches loaded" <|
                \() ->
                    Expect.equal initialModel (deleteNumberCheckerEntry 7 initialModel)
            ]
        , describe "modifyNumberCheckerRows tests"
            [ test "Can increment an actual entry of a number-checker row" <|
                \() ->
                    initialModel
                        |> modifyNumberCheckerRows 1 2
                        |> Expect.equal { initModel | numberCheckerEntries = sampleNumberCheckerDataIncremented }
            , test "Can decrement an actual entry of a number-checker row" <|
                \() ->
                    initialModel
                        |> modifyNumberCheckerRows -1 2
                        |> Expect.equal { initModel | numberCheckerEntries = sampleNumberCheckerDataDecremented }
            ]
        ]
