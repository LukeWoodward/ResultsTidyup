module StopwatchOperations exposing
    ( DistanceType(..)
    , OffsetDetails
    , StopwatchField(..)
    , StopwatchOperation(..)
    , StopwatchOperationChangeType(..)
    , StopwatchOperationEditDetails
    , StopwatchOperationValidationError(..)
    , StopwatchesToModify(..)
    , emptyEditDetails
    , updateEditDetails
    , validateEditDetails
    )

import DataEntry
    exposing
        ( FloatEntry
        , IntegerEntry
        , emptyEntry
        , floatEntryFromString
        , integerEntryFromString
        , integerEntryFromTime
        , isPositive
        )


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


type alias OffsetDetails =
    { offset : IntegerEntry
    , applyToStopwatch1 : Bool
    , applyToStopwatch2 : Bool
    }


type alias StopwatchOperationEditDetails =
    { operation : StopwatchOperation
    , addOffsetDetails : OffsetDetails
    , subtractOffsetDetails : OffsetDetails
    , manualScaleFactor : FloatEntry
    , expectedDistance : IntegerEntry
    , actualDistance : IntegerEntry
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
    | StopwatchFieldEdited StopwatchField String


emptyOffset : OffsetDetails
emptyOffset =
    OffsetDetails emptyEntry False False


emptyEditDetails : StopwatchOperationEditDetails
emptyEditDetails =
    { operation = NoOperationSelected
    , addOffsetDetails = emptyOffset
    , subtractOffsetDetails = emptyOffset
    , manualScaleFactor = emptyEntry
    , expectedDistance = emptyEntry
    , actualDistance = emptyEntry
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
                    [ scaleFactorValidation editDetails.manualScaleFactor ]

                ApplyDistanceBasedStopwatchScaleFactor ->
                    [ distanceValidation ExpectedDistance editDetails.expectedDistance
                    , distanceValidation ActualDistance editDetails.actualDistance
                    , equalDistanceValidation editDetails.expectedDistance editDetails.actualDistance
                    ]
    in
    List.filterMap identity allErrors
        |> List.head
        |> Maybe.withDefault NoValidationError


updateEditDetails : StopwatchOperationChangeType -> StopwatchOperationEditDetails -> StopwatchOperationEditDetails
updateEditDetails change editDetails =
    case change of
        ChangeOperation newOperation ->
            { editDetails | operation = newOperation }

        StopwatchFieldEdited AddOffsetField newValue ->
            let
                oldOffset : OffsetDetails
                oldOffset =
                    editDetails.addOffsetDetails
            in
            { editDetails | addOffsetDetails = { oldOffset | offset = integerEntryFromTime newValue } }

        StopwatchFieldEdited SubtractOffsetField newValue ->
            let
                oldOffset : OffsetDetails
                oldOffset =
                    editDetails.subtractOffsetDetails
            in
            { editDetails | subtractOffsetDetails = { oldOffset | offset = integerEntryFromTime newValue } }

        StopwatchFieldEdited ScaleFactorField newValue ->
            { editDetails | manualScaleFactor = floatEntryFromString newValue }

        StopwatchFieldEdited ExpectedDistanceManualField newValue ->
            { editDetails | expectedDistance = integerEntryFromString newValue }

        StopwatchFieldEdited ActualDistanceField newValue ->
            { editDetails | actualDistance = integerEntryFromString newValue }
