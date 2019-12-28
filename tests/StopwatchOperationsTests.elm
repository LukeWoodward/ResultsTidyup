module StopwatchOperationsTests exposing (suite)

import DataEntry exposing (IntegerEntry, emptyEntry, floatEntryFromFloat, integerEntryFromInt)
import Expect exposing (Expectation)
import StopwatchOperations
    exposing
        ( DistanceType(..)
        , OffsetDetails
        , OffsetType(..)
        , StopwatchField(..)
        , StopwatchOperation(..)
        , StopwatchOperationChangeType(..)
        , StopwatchOperationEditDetails
        , StopwatchOperationValidationError(..)
        , emptyEditDetails
        , isActualDistanceFieldInvalid
        , isAddOffsetFieldInvalid
        , isExpectedDistanceFieldInvalid
        , isScaleFactorFieldInvalid
        , isSubtractOffsetFieldInvalid
        , updateEditDetails
        , validateEditDetails
        )
import Test exposing (Test, describe, test)


editDetailsForDistanceBasedScaleFactorTest : IntegerEntry -> IntegerEntry -> StopwatchOperationEditDetails
editDetailsForDistanceBasedScaleFactorTest expectedDistanceEntry actualDistanceEntry =
    { emptyEditDetails
        | operation = ApplyDistanceBasedStopwatchScaleFactor
        , expectedDistance = expectedDistanceEntry
        , actualDistance = actualDistanceEntry
    }


allValidationErrors : List ( String, StopwatchOperationValidationError )
allValidationErrors =
    [ ( "noError", NoValidationError )
    , ( "operationNotSelected", StopwatchOperationNotSelected )
    , ( "invalidAddOffset", InvalidOffset AddOffset )
    , ( "invalidSubtractOffset", InvalidOffset SubtractOffset )
    , ( "noStopwatchesToAddOffsetTo", NoStopwatchesToApplyOffsetTo AddOffset )
    , ( "noStopwatchesToSubtractOffsetFrom", NoStopwatchesToApplyOffsetTo SubtractOffset )
    , ( "subtractOffsetTooLarge", SubtractOffsetTooLarge 1 2 )
    , ( "invalidScaleFactor", InvalidScaleFactor )
    , ( "scaleFactorOne", ScaleFactorOne )
    , ( "invalidExpectedDistance", InvalidDistance ExpectedDistance )
    , ( "invalidActualDistance", InvalidDistance ActualDistance )
    , ( "actualDistanceEqualsExpectedDistance", ActualDistanceEqualsExpectedDistance )
    ]


runFieldValidationTest : (StopwatchOperationEditDetails -> Bool) -> List String -> Expectation
runFieldValidationTest validationFunction expectedFields =
    allValidationErrors
        |> List.filter (\( name, error ) -> validationFunction { emptyEditDetails | validationError = error })
        |> List.map Tuple.first
        |> Expect.equal expectedFields


suite : Test
suite =
    describe "StopwatchOperationsTests tests"
        [ describe "validateEditDetails tests"
            [ describe "validateEditDetails: no operation selected"
                [ test "validateEditDetails when no operation selected is not valid" <|
                    \() ->
                        validateEditDetails 1000 emptyEditDetails
                            |> Expect.equal StopwatchOperationNotSelected
                ]
            , describe "validateEditDetails: adding an offset"
                [ test "validateEditDetails with an invalid offset to add is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails emptyEntry True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a zero offset to add is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 0) True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a negative offset to add is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt -60) True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a offset to add to no stopwatches is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal (NoStopwatchesToApplyOffsetTo AddOffset)
                , test "validateEditDetails with a offset to add to stopwatch 1 only is valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) True False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to stopwatch 2 only is valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to both stopwatches is valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) True True }
                            |> Expect.equal NoValidationError
                ]
            , describe "validateEditDetails: subtracting an offset"
                [ test "validateEditDetails with an invalid offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails emptyEntry True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a zero offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 0) True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a negative offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt -60) True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a offset to subtract from no stopwatches is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal (NoStopwatchesToApplyOffsetTo SubtractOffset)
                , test "validateEditDetails with a offset to subtract from stopwatch 1 only is valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) True False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to subtract from stopwatch 2 only is valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to subtract from both stopwatches is valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) True True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a too-large offset to subtract from both stopwatches is invalid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 1200) True True }
                            |> Expect.equal (SubtractOffsetTooLarge 1000 1200)
                ]
            , describe "validateEditDetails: applying a scale factor"
                [ test "validateEditDetails with an invalid scale factor is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a negative scale factor is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, manualScaleFactor = floatEntryFromFloat -1.0 }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a scale factor of zero is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, manualScaleFactor = floatEntryFromFloat 0.0 }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a scale factor of one is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, manualScaleFactor = floatEntryFromFloat 1.0 }
                            |> Expect.equal ScaleFactorOne
                , test "validateEditDetails with a scale factor to apply is valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, manualScaleFactor = floatEntryFromFloat 1.042 }
                            |> Expect.equal NoValidationError
                ]
            , describe "validateEditDetails: applying a distance-based scale factor"
                [ test "validateEditDetails with an invalid expected distance is not valid" <|
                    \() ->
                        validateEditDetails 1000 (editDetailsForDistanceBasedScaleFactorTest emptyEntry (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with a zero expected distance is not valid" <|
                    \() ->
                        validateEditDetails 1000 (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 0) (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with a negative expected distance is not valid" <|
                    \() ->
                        validateEditDetails 1000 (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt -5000) (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with an invalid actual distance is not valid" <|
                    \() ->
                        validateEditDetails 1000 (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) emptyEntry)
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with a zero actual distance is not valid" <|
                    \() ->
                        validateEditDetails 1000 (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 0))
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with a negative actual distance is not valid" <|
                    \() ->
                        validateEditDetails 1000 (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt -4600))
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with equal expected and actual distance is not valid" <|
                    \() ->
                        validateEditDetails 1000 (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 5000))
                            |> Expect.equal ActualDistanceEqualsExpectedDistance
                , test "validateEditDetails with different expected and actual distance is not valid" <|
                    \() ->
                        validateEditDetails 1000 (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 4600))
                            |> Expect.equal NoValidationError
                ]
            ]
        , describe "updateEditDetails tests"
            [ test "Can update an operation type" <|
                \() ->
                    updateEditDetails (ChangeOperation ApplyStopwatchScaleFactor) emptyEditDetails
                        |> Expect.equal { emptyEditDetails | operation = ApplyStopwatchScaleFactor }
            , test "Can update an offset to add" <|
                \() ->
                    updateEditDetails (StopwatchFieldEdited AddOffsetField "01:44") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | addOffsetDetails = OffsetDetails (IntegerEntry "01:44" (Just 104)) False False }
            , test "Can update an offset to subtract" <|
                \() ->
                    updateEditDetails (StopwatchFieldEdited SubtractOffsetField "03:26") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | subtractOffsetDetails = OffsetDetails (IntegerEntry "03:26" (Just 206)) False False }
            , test "Can update a scale factor" <|
                \() ->
                    updateEditDetails (StopwatchFieldEdited ScaleFactorField "0.974") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | manualScaleFactor = floatEntryFromFloat 0.974 }
            , test "Can update an expected distance" <|
                \() ->
                    updateEditDetails (StopwatchFieldEdited ExpectedDistanceManualField "5000") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | expectedDistance = integerEntryFromInt 5000 }
            , test "Can update an actual distance" <|
                \() ->
                    updateEditDetails (StopwatchFieldEdited ActualDistanceField "4875") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | actualDistance = integerEntryFromInt 4875 }
            ]
        , describe "Field validation function tests"
            [ test "isAddOffsetFieldInvalid reports correct errors" <|
                \() ->
                    runFieldValidationTest isAddOffsetFieldInvalid [ "invalidAddOffset", "noStopwatchesToAddOffsetTo" ]
            , test "isSubtractOffsetFieldInvalid reports correct errors" <|
                \() ->
                    runFieldValidationTest isSubtractOffsetFieldInvalid [ "invalidSubtractOffset", "noStopwatchesToSubtractOffsetFrom", "subtractOffsetTooLarge" ]
            , test "isScaleFactorFieldInvalid reports correct errors" <|
                \() ->
                    runFieldValidationTest isScaleFactorFieldInvalid [ "invalidScaleFactor", "scaleFactorOne" ]
            , test "isExpectedDistanceFieldInvalid reports correct errors" <|
                \() ->
                    runFieldValidationTest isExpectedDistanceFieldInvalid [ "invalidExpectedDistance", "actualDistanceEqualsExpectedDistance" ]
            , test "isActualDistanceFieldInvalid reports correct errors" <|
                \() ->
                    runFieldValidationTest isActualDistanceFieldInvalid [ "invalidActualDistance", "actualDistanceEqualsExpectedDistance" ]
            ]
        ]
