module StopwatchOperations exposing
    ( DistanceType(..)
    , OffsetDetails
    , ScaleFactorDetails
    , StopwatchField(..)
    , StopwatchOperation(..)
    , StopwatchOperationEditDetails
    , StopwatchOperationValidationError(..)
    , StopwatchesToModify(..)
    , emptyEditDetails
    , validateEditDetails
    )

import DataEntry exposing (FloatEntry, IntegerEntry, emptyEntry, isPositive)


type StopwatchesToModify
    = StopwatchOneOnly
    | StopwatchTwoOnly
    | BothStopwatches


type StopwatchOperation
    = NoOperationSelected
    | AddStopwatchTimeOffset
    | SubtractStopwatchTimeOffset
    | ApplyStopwatchScaleFactor
    | ApplyDistanceBasedStopwatchScaleFactor


type ScaleFactorType
    = ScaleFactor
    | DistanceBased


type alias OffsetDetails =
    { offset : IntegerEntry
    , applyToStopwatch1 : Bool
    , applyToStopwatch2 : Bool
    }


type alias ScaleFactorDetails =
    { manualScaleFactor : FloatEntry
    , expectedDistance : IntegerEntry
    , actualDistance : IntegerEntry
    }


type alias StopwatchOperationEditDetails =
    { operation : StopwatchOperation
    , addOffsetDetails : OffsetDetails
    , subtractOffsetDetails : OffsetDetails
    , scaleFactorDetails : ScaleFactorDetails
    }


type DistanceType
    = ActualDistance
    | ExpectedDistance


type StopwatchField
    = AddOffsetField
    | SubtractOffsetField
    | ScaleFactorField
    | ActualDistanceField
    | ExpectedDistanceManualField


type StopwatchOperationValidationError
    = NoValidationError
    | StopwatchOperationNotSelected
    | InvalidOffset StopwatchField
    | NoStopwatchesToApplyOffsetTo StopwatchField
    | SubtractOffsetTooLarge Int Int
    | InvalidScaleFactor
    | ScaleFactorOne
    | InvalidDistance DistanceType
    | ActualDistanceEqualsExpectedDistance


type StopwatchOperationChangeType
    = ChangeOperation StopwatchOperation
    | ChangeScaleFactorType ScaleFactorType
    | StopwatchFieldEdited StopwatchField String


emptyOffset : OffsetDetails
emptyOffset =
    OffsetDetails emptyEntry False False


emptyEditDetails : StopwatchOperationEditDetails
emptyEditDetails =
    { operation = NoOperationSelected
    , addOffsetDetails = emptyOffset
    , subtractOffsetDetails = emptyOffset
    , scaleFactorDetails = ScaleFactorDetails emptyEntry emptyEntry emptyEntry
    }


timeOffsetValidation : StopwatchField -> OffsetDetails -> Maybe StopwatchOperationValidationError
timeOffsetValidation field offsetDetails =
    if isPositive offsetDetails.offset then
        if not offsetDetails.applyToStopwatch1 && not offsetDetails.applyToStopwatch2 then
            Just (NoStopwatchesToApplyOffsetTo field)

        else
            Nothing

    else
        Just (InvalidOffset field)


offsetToRemoveLessThanFastestTime : Int -> OffsetDetails -> Maybe StopwatchOperationValidationError
offsetToRemoveLessThanFastestTime fastestTime offsetDetails =
    case offsetDetails.offset.parsedValue of
        Just someOffset ->
            if someOffset > fastestTime then
                Just (SubtractOffsetTooLarge fastestTime someOffset)

            else
                Nothing

        Nothing ->
            Nothing


scaleFactorValidation : FloatEntry -> Maybe StopwatchOperationValidationError
scaleFactorValidation scaleFactorEntry =
    if scaleFactorEntry.parsedValue == Just 1.0 then
        Just ScaleFactorOne

    else if isPositive scaleFactorEntry then
        Nothing

    else
        Just InvalidScaleFactor


distanceValidation : DistanceType -> IntegerEntry -> Maybe StopwatchOperationValidationError
distanceValidation distanceType distanceEntry =
    if isPositive distanceEntry then
        Nothing

    else
        Just (InvalidDistance distanceType)


equalDistanceValidation : IntegerEntry -> IntegerEntry -> Maybe StopwatchOperationValidationError
equalDistanceValidation expectedEntry actualEntry =
    if expectedEntry.parsedValue /= Nothing && expectedEntry.parsedValue == actualEntry.parsedValue then
        Just ActualDistanceEqualsExpectedDistance

    else
        Nothing


validateEditDetails : Int -> StopwatchOperationEditDetails -> StopwatchOperationValidationError
validateEditDetails fastestTime editDetails =
    let
        allErrors : List (Maybe StopwatchOperationValidationError)
        allErrors =
            case editDetails.operation of
                NoOperationSelected ->
                    [ Just StopwatchOperationNotSelected ]

                AddStopwatchTimeOffset ->
                    [ timeOffsetValidation AddOffsetField editDetails.addOffsetDetails ]

                SubtractStopwatchTimeOffset ->
                    [ timeOffsetValidation SubtractOffsetField editDetails.subtractOffsetDetails
                    , offsetToRemoveLessThanFastestTime fastestTime editDetails.subtractOffsetDetails
                    ]

                ApplyStopwatchScaleFactor ->
                    [ scaleFactorValidation editDetails.scaleFactorDetails.manualScaleFactor ]

                ApplyDistanceBasedStopwatchScaleFactor ->
                    [ distanceValidation ExpectedDistance editDetails.scaleFactorDetails.expectedDistance
                    , distanceValidation ActualDistance editDetails.scaleFactorDetails.actualDistance
                    , equalDistanceValidation editDetails.scaleFactorDetails.expectedDistance editDetails.scaleFactorDetails.actualDistance
                    ]
    in
    List.filterMap identity allErrors
        |> List.head
        |> Maybe.withDefault NoValidationError
