module TimerOperationsTests exposing (suite)

import DataEntry exposing (IntegerEntry, emptyEntry, floatEntryFromFloat, integerEntryFromInt, integerEntryFromString)
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestData exposing (doubleTimers, singleTimer)
import Timer exposing (TimerFile, Timers(..), WhichTimer(..))
import TimerOperations
    exposing
        ( DistanceType(..)
        , OffsetDetails
        , OffsetType(..)
        , TimerField(..)
        , TimerOperation(..)
        , TimerOperationChangeType(..)
        , TimerOperationEditDetails
        , TimerOperationValidationError(..)
        , TimersToApplyTo(..)
        , emptyEditDetailsFor
        , emptyEditDetailsFromTimers
        , isActualDistanceFieldInvalid
        , isAddOffsetFieldInvalid
        , isExpectedDistanceFieldInvalid
        , isScaleFactorFieldInvalid
        , isSubtractOffsetFieldInvalid
        , tryApplyOperationToTimerData
        , updateEditDetails
        , validateEditDetails
        )


emptyEditDetails : TimerOperationEditDetails
emptyEditDetails =
    emptyEditDetailsFor TwoTimers


editDetailsForDistanceBasedScaleFactorTest : IntegerEntry -> IntegerEntry -> TimerOperationEditDetails
editDetailsForDistanceBasedScaleFactorTest expectedDistanceEntry actualDistanceEntry =
    { emptyEditDetails
        | operation = ApplyDistanceBasedTimerScaleFactor
        , expectedDistance = expectedDistanceEntry
        , actualDistance = actualDistanceEntry
    }


allValidationErrors : List ( String, TimerOperationValidationError )
allValidationErrors =
    [ ( "noError", NoValidationError )
    , ( "operationNotSelected", TimerOperationNotSelected )
    , ( "invalidAddOffset", InvalidOffset AddOffset )
    , ( "invalidSubtractOffset", InvalidOffset SubtractOffset )
    , ( "noTimersToAddOffsetTo", NoTimersToApplyOffsetTo AddOffset )
    , ( "noTimersToSubtractOffsetFrom", NoTimersToApplyOffsetTo SubtractOffset )
    , ( "subtractOffsetTooLarge", SubtractOffsetTooLarge 1 2 )
    , ( "invalidScaleFactor", InvalidScaleFactor )
    , ( "scaleFactorOne", ScaleFactorOne )
    , ( "invalidExpectedDistance", InvalidDistance ExpectedDistance )
    , ( "invalidActualDistance", InvalidDistance ActualDistance )
    , ( "actualDistanceEqualsExpectedDistance", ActualDistanceEqualsExpectedDistance )
    ]


testTimerFile : TimerFile
testTimerFile =
    TimerFile "timer1.txt" "Name1"


runFieldValidationTest : (TimerOperationEditDetails -> Bool) -> List String -> Expectation
runFieldValidationTest validationFunction expectedFields =
    allValidationErrors
        |> List.filter (\( _, error ) -> validationFunction { emptyEditDetails | validationError = error })
        |> List.map Tuple.first
        |> Expect.equal expectedFields


getTimes : Timers -> ( List Int, List Int )
getTimes timers =
    case timers of
        None ->
            ( [], [] )

        Single _ times ->
            ( times, [] )

        Double doubleTimerData ->
            ( doubleTimerData.times1, doubleTimerData.times2 )


getAddOffsetEditDetails : Int -> Bool -> Bool -> TimerOperationEditDetails
getAddOffsetEditDetails offset applyToTimer1 applyToTimer2 =
    { emptyEditDetails
        | operation = AddTimerTimeOffset
        , addOffsetDetails = OffsetDetails (integerEntryFromInt offset) applyToTimer1 applyToTimer2
    }


getSubtractOffsetEditDetails : Int -> Bool -> Bool -> TimerOperationEditDetails
getSubtractOffsetEditDetails offset applyToTimer1 applyToTimer2 =
    { emptyEditDetails
        | operation = SubtractTimerTimeOffset
        , subtractOffsetDetails = OffsetDetails (integerEntryFromInt offset) applyToTimer1 applyToTimer2
    }


getApplyScaleFactorEditDetails : Float -> TimerOperationEditDetails
getApplyScaleFactorEditDetails scaleFactor =
    { emptyEditDetails
        | operation = ApplyTimerScaleFactor
        , manualScaleFactor = floatEntryFromFloat scaleFactor
    }


getApplyDistanceBasedScaleFactorEditDetails : Int -> Int -> TimerOperationEditDetails
getApplyDistanceBasedScaleFactorEditDetails expectedDistance actualDistance =
    { emptyEditDetails
        | operation = ApplyDistanceBasedTimerScaleFactor
        , expectedDistance = integerEntryFromInt expectedDistance
        , actualDistance = integerEntryFromInt actualDistance
    }


suite : Test
suite =
    describe "TimerOperationsTests tests"
        [ describe "emptyEditDetailsFromTimers tests"
            [ test "emptyEditDetailsFromTimers for single timer data creates empty object with OneTimer" <|
                \() ->
                    emptyEditDetailsFromTimers singleTimer
                        |> .timersToApplyTo
                        |> Expect.equal OneTimer
            , test "emptyEditDetailsFromTimers for double timer data creates empty object with TwoTimers" <|
                \() ->
                    emptyEditDetailsFromTimers doubleTimers
                        |> .timersToApplyTo
                        |> Expect.equal TwoTimers
            ]
        , describe "validateEditDetails tests"
            [ describe "validateEditDetails: no operation selected"
                [ test "validateEditDetails when no operation selected is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers emptyEditDetails
                            |> Expect.equal TimerOperationNotSelected
                ]
            , describe "validateEditDetails: adding an offset"
                [ test "validateEditDetails with an invalid offset to add is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails emptyEntry True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a zero offset to add is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 0) True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a negative offset to add is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt -60) True True }
                            |> Expect.equal (InvalidOffset AddOffset)
                , test "validateEditDetails with a offset to add to no timers is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal (NoTimersToApplyOffsetTo AddOffset)
                , test "validateEditDetails with a offset to add to no timers but with only one timer is valid" <|
                    \() ->
                        validateEditDetails singleTimer { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to no timers but with no timers is valid" <|
                    \() ->
                        validateEditDetails None { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to timer 1 only is valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) True False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to timer 2 only is valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to add to both timers is valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = AddTimerTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) True True }
                            |> Expect.equal NoValidationError
                ]
            , describe "validateEditDetails: subtracting an offset"
                [ test "validateEditDetails with an invalid offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails emptyEntry True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a zero offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 0) True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a negative offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt -60) True True }
                            |> Expect.equal (InvalidOffset SubtractOffset)
                , test "validateEditDetails with a offset to subtract from no timers is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal (NoTimersToApplyOffsetTo SubtractOffset)
                , test "validateEditDetails with a offset to subtract from no timers but with a single timer is valid" <|
                    \() ->
                        validateEditDetails singleTimer { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to subtract from timer 1 only is valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) True False }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to subtract from timer 2 only is valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a offset to subtract from both timers is valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) True True }
                            |> Expect.equal NoValidationError
                , test "validateEditDetails with a too-large offset to subtract from both timers is invalid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = SubtractTimerTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 200) True True }
                            |> Expect.equal (SubtractOffsetTooLarge 191 200)
                ]
            , describe "validateEditDetails: applying a scale factor"
                [ test "validateEditDetails with an invalid scale factor is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = ApplyTimerScaleFactor }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a negative scale factor is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = ApplyTimerScaleFactor, manualScaleFactor = floatEntryFromFloat -1.0 }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a scale factor of zero is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = ApplyTimerScaleFactor, manualScaleFactor = floatEntryFromFloat 0.0 }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a scale factor of one is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = ApplyTimerScaleFactor, manualScaleFactor = floatEntryFromFloat 1.0 }
                            |> Expect.equal ScaleFactorOne
                , test "validateEditDetails with a scale factor to apply is valid" <|
                    \() ->
                        validateEditDetails doubleTimers { emptyEditDetails | operation = ApplyTimerScaleFactor, manualScaleFactor = floatEntryFromFloat 1.042 }
                            |> Expect.equal NoValidationError
                ]
            , describe "validateEditDetails: applying a distance-based scale factor"
                [ test "validateEditDetails with an invalid expected distance is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers (editDetailsForDistanceBasedScaleFactorTest emptyEntry (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with a zero expected distance is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 0) (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with a negative expected distance is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt -5000) (integerEntryFromInt 4600))
                            |> Expect.equal (InvalidDistance ExpectedDistance)
                , test "validateEditDetails with an invalid actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) emptyEntry)
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with a zero actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 0))
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with a negative actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt -4600))
                            |> Expect.equal (InvalidDistance ActualDistance)
                , test "validateEditDetails with equal expected and actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 5000))
                            |> Expect.equal ActualDistanceEqualsExpectedDistance
                , test "validateEditDetails with different expected and actual distance is not valid" <|
                    \() ->
                        validateEditDetails doubleTimers (editDetailsForDistanceBasedScaleFactorTest (integerEntryFromInt 5000) (integerEntryFromInt 4600))
                            |> Expect.equal NoValidationError
                ]
            ]
        , describe "updateEditDetails tests"
            [ test "Can update an operation type" <|
                \() ->
                    updateEditDetails (ChangeOperation ApplyTimerScaleFactor) emptyEditDetails
                        |> Expect.equal { emptyEditDetails | operation = ApplyTimerScaleFactor }
            , test "Can update an offset to add" <|
                \() ->
                    updateEditDetails (TimerFieldEdited AddOffsetField "01:44") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | addOffsetDetails = OffsetDetails (IntegerEntry "01:44" (Just 104)) False False }
            , test "Can update an offset to subtract" <|
                \() ->
                    updateEditDetails (TimerFieldEdited SubtractOffsetField "03:26") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | subtractOffsetDetails = OffsetDetails (IntegerEntry "03:26" (Just 206)) False False }
            , test "Can update a scale factor" <|
                \() ->
                    updateEditDetails (TimerFieldEdited ScaleFactorField "0.974") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | manualScaleFactor = floatEntryFromFloat 0.974 }
            , test "Can update an expected distance" <|
                \() ->
                    updateEditDetails (TimerFieldEdited ExpectedDistanceManualField "5000") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | expectedDistance = integerEntryFromInt 5000 }
            , test "Can update an actual distance" <|
                \() ->
                    updateEditDetails (TimerFieldEdited ActualDistanceField "4875") emptyEditDetails
                        |> Expect.equal { emptyEditDetails | actualDistance = integerEntryFromInt 4875 }
            , test "Can check the apply-to-timer-1 flag for the add offset" <|
                \() ->
                    updateEditDetails (TimerCheckboxChanged AddOffset TimerOne True) emptyEditDetails
                        |> Expect.equal { emptyEditDetails | addOffsetDetails = OffsetDetails emptyEntry True False }
            , test "Can clear the apply-to-timer-1 flag for the add offset" <|
                \() ->
                    { emptyEditDetails | addOffsetDetails = OffsetDetails emptyEntry True False }
                        |> updateEditDetails (TimerCheckboxChanged AddOffset TimerOne False)
                        |> Expect.equal emptyEditDetails
            , test "Can check the apply-to-timer-2 flag for the add offset" <|
                \() ->
                    updateEditDetails (TimerCheckboxChanged AddOffset TimerTwo True) emptyEditDetails
                        |> Expect.equal { emptyEditDetails | addOffsetDetails = OffsetDetails emptyEntry False True }
            , test "Can clear the apply-to-timer-2 flag for the add offset" <|
                \() ->
                    { emptyEditDetails | addOffsetDetails = OffsetDetails emptyEntry False True }
                        |> updateEditDetails (TimerCheckboxChanged AddOffset TimerTwo False)
                        |> Expect.equal emptyEditDetails
            , test "Can check the apply-to-timer-1 flag for the subtract offset" <|
                \() ->
                    updateEditDetails (TimerCheckboxChanged SubtractOffset TimerOne True) emptyEditDetails
                        |> Expect.equal { emptyEditDetails | subtractOffsetDetails = OffsetDetails emptyEntry True False }
            , test "Can clear the apply-to-timer-1 flag for the subtract offset" <|
                \() ->
                    { emptyEditDetails | subtractOffsetDetails = OffsetDetails emptyEntry True False }
                        |> updateEditDetails (TimerCheckboxChanged SubtractOffset TimerOne False)
                        |> Expect.equal emptyEditDetails
            , test "Can check the apply-to-timer-2 flag for the subtract offset" <|
                \() ->
                    updateEditDetails (TimerCheckboxChanged SubtractOffset TimerTwo True) emptyEditDetails
                        |> Expect.equal { emptyEditDetails | subtractOffsetDetails = OffsetDetails emptyEntry False True }
            , test "Can clear the apply-to-timer-2 flag for the subtract offset" <|
                \() ->
                    { emptyEditDetails | subtractOffsetDetails = OffsetDetails emptyEntry False True }
                        |> updateEditDetails (TimerCheckboxChanged SubtractOffset TimerTwo False)
                        |> Expect.equal emptyEditDetails
            ]
        , describe "Field validation function tests"
            [ test "isAddOffsetFieldInvalid reports correct errors" <|
                \() ->
                    runFieldValidationTest isAddOffsetFieldInvalid [ "invalidAddOffset", "noTimersToAddOffsetTo" ]
            , test "isSubtractOffsetFieldInvalid reports correct errors" <|
                \() ->
                    runFieldValidationTest isSubtractOffsetFieldInvalid [ "invalidSubtractOffset", "noTimersToSubtractOffsetFrom", "subtractOffsetTooLarge" ]
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
        , describe "tryApplyOperationToTimerData tests"
            [ test "Returns the validation error if a field is invalid" <|
                \() ->
                    let
                        editDetails : TimerOperationEditDetails
                        editDetails =
                            { emptyEditDetails
                                | operation = AddTimerTimeOffset
                                , addOffsetDetails = OffsetDetails (integerEntryFromString "Not a valid number") True True
                            }
                    in
                    tryApplyOperationToTimerData editDetails doubleTimers
                        |> Expect.equal (Err (InvalidOffset AddOffset))
            , test "Adds offset to both timers" <|
                \() ->
                    tryApplyOperationToTimerData (getAddOffsetEditDetails 30 True True) doubleTimers
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 221, 494, 633, 776, 912, 1059 ], [ 221, 493, 776, 821, 912 ] ))
            , test "Adds offset to timer 1 only" <|
                \() ->
                    tryApplyOperationToTimerData (getAddOffsetEditDetails 30 True False) doubleTimers
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 221, 494, 633, 776, 912, 1059 ], [ 191, 463, 746, 791, 882 ] ))
            , test "Adds offset to timer 2 only" <|
                \() ->
                    tryApplyOperationToTimerData (getAddOffsetEditDetails 30 False True) doubleTimers
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 191, 464, 603, 746, 882, 1029 ], [ 221, 493, 776, 821, 912 ] ))
            , test "Adds offset to single timer" <|
                \() ->
                    tryApplyOperationToTimerData (getAddOffsetEditDetails 30 True True) (Single testTimerFile [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single testTimerFile [ 221, 494, 633, 776, 912, 1059 ]))
            , test "Adds offset to single timer even if flag disabled" <|
                \() ->
                    tryApplyOperationToTimerData (getAddOffsetEditDetails 30 False True) (Single testTimerFile [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single testTimerFile [ 221, 494, 633, 776, 912, 1059 ]))
            , test "Adding offset to no timers does nothing" <|
                \() ->
                    tryApplyOperationToTimerData (getAddOffsetEditDetails 30 True True) None
                        |> Expect.equal (Ok None)
            , test "Subtracts offset from both timers" <|
                \() ->
                    tryApplyOperationToTimerData (getSubtractOffsetEditDetails 30 True True) doubleTimers
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 161, 434, 573, 716, 852, 999 ], [ 161, 433, 716, 761, 852 ] ))
            , test "Subtracts offset from timer 1 only" <|
                \() ->
                    tryApplyOperationToTimerData (getSubtractOffsetEditDetails 30 True False) doubleTimers
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 161, 434, 573, 716, 852, 999 ], [ 191, 463, 746, 791, 882 ] ))
            , test "Subtracts offset from timer 2 only" <|
                \() ->
                    tryApplyOperationToTimerData (getSubtractOffsetEditDetails 30 False True) doubleTimers
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 191, 464, 603, 746, 882, 1029 ], [ 161, 433, 716, 761, 852 ] ))
            , test "Subtracts offset from single timer" <|
                \() ->
                    tryApplyOperationToTimerData (getSubtractOffsetEditDetails 30 True True) (Single testTimerFile [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single testTimerFile [ 161, 434, 573, 716, 852, 999 ]))
            , test "Subtracts offset from single timer even if flag disabled" <|
                \() ->
                    tryApplyOperationToTimerData (getSubtractOffsetEditDetails 30 False True) (Single testTimerFile [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single testTimerFile [ 161, 434, 573, 716, 852, 999 ]))
            , test "Applies a scale factor to two timers" <|
                \() ->
                    tryApplyOperationToTimerData (getApplyScaleFactorEditDetails 1.37) doubleTimers
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 262, 636, 826, 1022, 1208, 1410 ], [ 262, 634, 1022, 1084, 1208 ] ))
            , test "Applies a scale factor to a single timer" <|
                \() ->
                    tryApplyOperationToTimerData (getApplyScaleFactorEditDetails 1.37) (Single testTimerFile [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single testTimerFile [ 262, 636, 826, 1022, 1208, 1410 ]))
            , test "Applies a scale factor to no timers" <|
                \() ->
                    tryApplyOperationToTimerData (getApplyScaleFactorEditDetails 1.37) None
                        |> Expect.equal (Ok None)

            -- Scale factor for the next two tests is (5000/4600)^1.06, ~ 1.0924
            , test "Applies a distance-based scale factor to two timers" <|
                \() ->
                    tryApplyOperationToTimerData (getApplyDistanceBasedScaleFactorEditDetails 5000 4600) doubleTimers
                        |> Result.map getTimes
                        |> Expect.equal (Ok ( [ 209, 507, 659, 815, 964, 1124 ], [ 209, 506, 815, 864, 964 ] ))
            , test "Applies a distance-based scale factor to a single timer" <|
                \() ->
                    tryApplyOperationToTimerData (getApplyDistanceBasedScaleFactorEditDetails 5000 4600) (Single testTimerFile [ 191, 464, 603, 746, 882, 1029 ])
                        |> Expect.equal (Ok (Single testTimerFile [ 209, 507, 659, 815, 964, 1124 ]))
            , test "Applies a distance-based scale factor to no timers" <|
                \() ->
                    tryApplyOperationToTimerData (getApplyDistanceBasedScaleFactorEditDetails 5000 4600) None
                        |> Expect.equal (Ok None)
            ]
        ]
