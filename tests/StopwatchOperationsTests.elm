module StopwatchOperationsTests exposing (suite)

import DataEntry exposing (IntegerEntry, emptyEntry, floatEntryFromFloat, integerEntryFromInt)
import Expect
import StopwatchOperations
    exposing
        ( DistanceType(..)
        , OffsetDetails
        , ScaleFactorDetails
        , StopwatchField(..)
        , StopwatchOperation(..)
        , StopwatchOperationEditDetails
        , StopwatchOperationValidationError(..)
        , emptyEditDetails
        , validateEditDetails
        )
import Test exposing (Test, describe, test)


editDetailsForDistanceBasedScaleFactorTest : IntegerEntry -> IntegerEntry -> StopwatchOperationEditDetails
editDetailsForDistanceBasedScaleFactorTest expectedDistanceEntry actualDistanceEntry =
    { emptyEditDetails | operation = ApplyDistanceBasedStopwatchScaleFactor, scaleFactorDetails = ScaleFactorDetails emptyEntry expectedDistanceEntry actualDistanceEntry }


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
                            |> Expect.equal (InvalidOffset AddOffsetField)
                , test "validateEditDetails with a zero offset to add is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 0) True True }
                            |> Expect.equal (InvalidOffset AddOffsetField)
                , test "validateEditDetails with a negative offset to add is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt -60) True True }
                            |> Expect.equal (InvalidOffset AddOffsetField)
                , test "validateEditDetails with a offset to add to no stopwatches is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = AddStopwatchTimeOffset, addOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal (NoStopwatchesToApplyOffsetTo AddOffsetField)
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
                            |> Expect.equal (InvalidOffset SubtractOffsetField)
                , test "validateEditDetails with a zero offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 0) True True }
                            |> Expect.equal (InvalidOffset SubtractOffsetField)
                , test "validateEditDetails with a negative offset to subtract is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt -60) True True }
                            |> Expect.equal (InvalidOffset SubtractOffsetField)
                , test "validateEditDetails with a offset to subtract from no stopwatches is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = SubtractStopwatchTimeOffset, subtractOffsetDetails = OffsetDetails (integerEntryFromInt 60) False False }
                            |> Expect.equal (NoStopwatchesToApplyOffsetTo SubtractOffsetField)
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
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, scaleFactorDetails = ScaleFactorDetails emptyEntry emptyEntry emptyEntry }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a negative scale factor is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, scaleFactorDetails = ScaleFactorDetails (floatEntryFromFloat -1.0) emptyEntry emptyEntry }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a scale factor of zero is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, scaleFactorDetails = ScaleFactorDetails (floatEntryFromFloat 0.0) emptyEntry emptyEntry }
                            |> Expect.equal InvalidScaleFactor
                , test "validateEditDetails with a scale factor of one is not valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, scaleFactorDetails = ScaleFactorDetails (floatEntryFromFloat 1.0) emptyEntry emptyEntry }
                            |> Expect.equal ScaleFactorOne
                , test "validateEditDetails with a scale factor to apply is valid" <|
                    \() ->
                        validateEditDetails 1000 { emptyEditDetails | operation = ApplyStopwatchScaleFactor, scaleFactorDetails = ScaleFactorDetails (floatEntryFromFloat 1.042) emptyEntry emptyEntry }
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
        ]
