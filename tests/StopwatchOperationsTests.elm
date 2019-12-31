module StopwatchOperationsTests exposing (suite)

import DataEntry exposing (IntegerEntry, emptyEntry, floatEntryFromFloat, integerEntryFromInt, integerEntryFromString)
import Expect exposing (Expectation)
import Stopwatch exposing (DoubleStopwatchData, Stopwatches(..), createMergedTable)
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
        , tryApplyOperationToStopwatchData
        , updateEditDetails
        , validateEditDetails
        )
import Test exposing (Test, describe, test)
import TestData exposing (doubleStopwatches, singleStopwatch)


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


getTimes : Stopwatches -> ( List Int, List Int )
getTimes stopwatches =
    case stopwatches of
        None ->
            ( [], [] )

        Single _ times ->
            ( times, [] )

        Double doubleStopwatchData ->
            ( doubleStopwatchData.times1, doubleStopwatchData.times2 )


getAddOffsetEditDetails : Int -> Bool -> Bool -> StopwatchOperationEditDetails
getAddOffsetEditDetails offset applyToStopwatch1 applyToStopwatch2 =
    { emptyEditDetails
        | operation = AddStopwatchTimeOffset
        , addOffsetDetails = OffsetDetails (integerEntryFromInt offset) applyToStopwatch1 applyToStopwatch2
    }


getSubtractOffsetEditDetails : Int -> Bool -> Bool -> StopwatchOperationEditDetails
getSubtractOffsetEditDetails offset applyToStopwatch1 applyToStopwatch2 =
    { emptyEditDetails
        | operation = SubtractStopwatchTimeOffset
        , subtractOffsetDetails = OffsetDetails (integerEntryFromInt offset) applyToStopwatch1 applyToStopwatch2
    }


getApplyScaleFactorEditDetails : Float -> StopwatchOperationEditDetails
getApplyScaleFactorEditDetails scaleFactor =
    { emptyEditDetails
        | operation = ApplyStopwatchScaleFactor
        , manualScaleFactor = floatEntryFromFloat scaleFactor
    }


getApplyDistanceBasedScaleFactorEditDetails : Int -> Int -> StopwatchOperationEditDetails
getApplyDistanceBasedScaleFactorEditDetails expectedDistance actualDistance =
    { emptyEditDetails
        | operation = ApplyDistanceBasedStopwatchScaleFactor
        , expectedDistance = integerEntryFromInt expectedDistance
        , actualDistance = integerEntryFromInt actualDistance
    }


suite : Test
suite =
    describe "StopwatchOperationsTests tests"
        [ describe "validateEditDetails tests"
            [ describe "validateEditDetails: no operation selected"
                [ test "validateEditDetails when no operation selected is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches emptyEditDetails
                            |> Expect.equal StopwatchOperationNotSelected
                ]
            , describe "validateEditDetails: adding an offset"
                [ test "validateEditDetails with an invalid offset to add is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails emptyEntry True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a zero offset to add is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 0) True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a negative offset to add is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt -60) True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a offset to add to no stopwatches is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal (NoStopwatchesToApplyOffsetTo AddOffset)
                , test "validateEditDetails with a offset to add to no stopwatches but with only one stopwatch is valid" <|
                    \() ->
                        validateEditDetails singleStopwatch { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to no stopwatches but with no stopwatches is valid" <|
                    \() ->
                        validateEditDetails None { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to stopwatch 1 only is valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) True False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to stopwatch 2 only is valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to both stopwatches is valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) True True }
                            |> Expect.equal NoValidationError
                ]
            , describe "validateEditDetails: subtracting an offset"
                [ test "validateEditDetails with an invalid offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails emptyEntry True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a zero offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 0) True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a negative offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt -60) True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a offset to subtract from no stopwatches is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal (NoStopwatchesToApplyOffsetTo SubtractOffset)
                , test "validateEditDetails with a offset to subtract from no stopwatches but with a single stopwatch is valid" <|
                    \() ->
                        validateEditDetails singleStopwatch { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to subtract from stopwatch 1 only is valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) True False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to subtract from stopwatch 2 only is valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to subtract from both stopwatches is valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) True True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a too-large offset to subtract from both stopwatches is invalid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 200) True True }
                            |> Expect.equal (SubtractOffsetTooLarge 191 200)
                ]
            , describe "validateEditDetails: applying a scale factor"
                [ test "validateEditDetails with an invalid scale factor is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = ApplyStopwatchScaleFactor }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a negative scale factor is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = ApplyStopwatchScaleFactor, manualScaleFactor = floatEntryFromFloat -1.0 }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a scale factor of zero is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = ApplyStopwatchScaleFactor, manualScaleFactor = floatEntryFromFloat 0.0 }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a scale factor of one is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = ApplyStopwatchScaleFactor, manualScaleFactor = floatEntryFromFloat 1.0 }
                            |> Expect.equal ScaleFactorOne
                , test "validateEditDetails with a scale factor to apply is valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches { emptyEditDetails | operation = ApplyStopwatchScaleFactor, manualScaleFactor = floatEntryFromFloat 1.042 }
                            |> Expect.equal NoValidationError
                ]
            , describe "validateEditDetails: applying a distance-based scale factor"
                [ test "validateEditDetails with an invalid expected distance is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches (editDetailsForDistanceBasedScaleFactorTest emptyEntry (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with a zero expected distance is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 0) (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with a negative expected distance is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt -5000) (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with an invalid actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) emptyEntry)
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with a zero actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 0))
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with a negative actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt -4600))
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with equal expected and actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 5000))
                            |> Expect.equal ActualDistanceEqualsExpectedDistance
                , test "validateEditDetails with different expected and actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleStopwatches (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 4600))
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
        , describe "tryApplyOperationToStopwatchData tests"
            [ test "Returns the validation error if a field is invalid" <|
                \() ->
                    let
                        editDetails : StopwatchOperationEditDetails
                        editDetails =
                            { emptyEditDetails
                                | operation = AddStopwatchTimeOffset
                                , addOffsetDetails = OffsetDetails (integerEntryFromString "Not a valid number") True True
                            }
                    in
                    tryApplyOperationToStopwatchData editDetails doubleStopwatches
                        |> Expect.equal (Err (InvalidOffset AddOffset))
            , test "Adds offset to both stopwatches" <|
                \() ->
                    tryApplyOperationToStopwatchData (getAddOffsetEditDetails 30 True True) doubleStopwatches
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 221, 494, 633, 776, 912, 1059 ], [ 221, 493, 776, 821, 912 ] ))
            , test "Adds offset to stopwatch 1 only" <|
                \() ->
                    tryApplyOperationToStopwatchData (getAddOffsetEditDetails 30 True False) doubleStopwatches
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 221, 494, 633, 776, 912, 1059 ], [ 191, 463, 746, 791, 882 ] ))
            , test "Adds offset to stopwatch 2 only" <|
                \() ->
                    tryApplyOperationToStopwatchData (getAddOffsetEditDetails 30 False True) doubleStopwatches
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 191, 464, 603, 746, 882, 1029 ], [ 221, 493, 776, 821, 912 ] ))
            , test "Adds offset to single stopwatch" <|
                \() ->
                    tryApplyOperationToStopwatchData (getAddOffsetEditDetails 30 True True) (Single "stopwatch1.txt" [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single "stopwatch1.txt" [ 221, 494, 633, 776, 912, 1059 ]))
            , test "Adds offset to single stopwatch even if flag disabled" <|
                \() ->
                    tryApplyOperationToStopwatchData (getAddOffsetEditDetails 30 False True) (Single "stopwatch1.txt" [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single "stopwatch1.txt" [ 221, 494, 633, 776, 912, 1059 ]))
            , test "Adding offset to no stopwatches does nothing" <|
                \() ->
                    tryApplyOperationToStopwatchData (getAddOffsetEditDetails 30 True True) None
                        |> Expect.equal (Ok None)
            , test "Subtracts offset from both stopwatches" <|
                \() ->
                    tryApplyOperationToStopwatchData (getSubtractOffsetEditDetails 30 True True) doubleStopwatches
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 161, 434, 573, 716, 852, 999 ], [ 161, 433, 716, 761, 852 ] ))
            , test "Subtracts offset from stopwatch 1 only" <|
                \() ->
                    tryApplyOperationToStopwatchData (getSubtractOffsetEditDetails 30 True False) doubleStopwatches
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 161, 434, 573, 716, 852, 999 ], [ 191, 463, 746, 791, 882 ] ))
            , test "Subtracts offset from stopwatch 2 only" <|
                \() ->
                    tryApplyOperationToStopwatchData (getSubtractOffsetEditDetails 30 False True) doubleStopwatches
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 191, 464, 603, 746, 882, 1029 ], [ 161, 433, 716, 761, 852 ] ))
            , test "Subtracts offset from single stopwatch" <|
                \() ->
                    tryApplyOperationToStopwatchData (getSubtractOffsetEditDetails 30 True True) (Single "stopwatch1.txt" [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single "stopwatch1.txt" [ 161, 434, 573, 716, 852, 999 ]))
            , test "Subtracts offset from single stopwatch even if flag disabled" <|
                \() ->
                    tryApplyOperationToStopwatchData (getSubtractOffsetEditDetails 30 False True) (Single "stopwatch1.txt" [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single "stopwatch1.txt" [ 161, 434, 573, 716, 852, 999 ]))
            , test "Applies a scale factor to two stopwatches" <|
                \() ->
                    tryApplyOperationToStopwatchData (getApplyScaleFactorEditDetails 1.37) doubleStopwatches
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 262, 636, 826, 1022, 1208, 1410 ], [ 262, 634, 1022, 1084, 1208 ] ))
            , test "Applies a scale factor to a single stopwatch" <|
                \() ->
                    tryApplyOperationToStopwatchData (getApplyScaleFactorEditDetails 1.37) (Single "stopwatch1.txt" [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single "stopwatch1.txt" [ 262, 636, 826, 1022, 1208, 1410 ]))
            , test "Applies a scale factor to no stopwatches" <|
                \() ->
                    tryApplyOperationToStopwatchData (getApplyScaleFactorEditDetails 1.37) None
                        |> Expect.equal (Ok None)
            , test "Applies a distance-based scale factor to two stopwatches" <|
                \() ->
                    tryApplyOperationToStopwatchData (getApplyDistanceBasedScaleFactorEditDetails 5000 4600) doubleStopwatches
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 208, 504, 655, 811, 959, 1118 ], [ 208, 503, 811, 860, 959 ] ))
            , test "Applies a distance-based scale factor to a single stopwatch" <|
                \() ->
                    tryApplyOperationToStopwatchData (getApplyDistanceBasedScaleFactorEditDetails 5000 4600) (Single "stopwatch1.txt" [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single "stopwatch1.txt" [ 208, 504, 655, 811, 959, 1118 ]))
            , test "Applies a distance-based scale factor to no stopwatches" <|
                \() ->
                    tryApplyOperationToStopwatchData (getApplyDistanceBasedScaleFactorEditDetails 5000 4600) None
                        |> Expect.equal (Ok None)
            ]
        ]
