module StopwatchOperations exposing
    ( DistanceType(..)
    , OffsetDetails
    , OffsetType(..)
    , StopwatchField(..)
    , StopwatchOperation(..)
    , StopwatchOperationChangeType(..)
    , StopwatchOperationEditDetails
    , StopwatchOperationValidationError(..)
    , StopwatchesToModify(..)
    , emptyEditDetails
    , isActualDistanceFieldInvalid
    , isAddOffsetFieldInvalid
    , isExpectedDistanceFieldInvalid
    , isScaleFactorFieldInvalid
    , isSubtractOffsetFieldInvalid
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


type DistanceType
    = ActualDistance
    | ExpectedDistance


type StopwatchField
    = AddOffsetField
    | SubtractOffsetField
    | ScaleFactorField
    | ActualDistanceField
    | ExpectedDistanceManualField


type OffsetType
    = AddOffset
    | SubtractOffset


type StopwatchOperationValidationError
    = NoValidationError
    | StopwatchOperationNotSelected
    | InvalidOffset OffsetType
    | NoStopwatchesToApplyOffsetTo OffsetType
    | SubtractOffsetTooLarge Int Int
    | InvalidScaleFactor
    | ScaleFactorOne
    | InvalidDistance DistanceType
    | ActualDistanceEqualsExpectedDistance


type alias StopwatchOperationEditDetails =
    { operation : StopwatchOperation
    , addOffsetDetails : OffsetDetails
    , subtractOffsetDetails : OffsetDetails
    , manualScaleFactor : FloatEntry
    , expectedDistance : IntegerEntry
    , actualDistance : IntegerEntry
    , validationError : StopwatchOperationValidationError
    }


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
    , validationError = NoValidationError
    }


timeOffsetValidation : StopwatchField -> OffsetType -> OffsetDetails -> Maybe StopwatchOperationValidationError
timeOffsetValidation field offsetType offsetDetails =
    if isPositive offsetDetails.offset then
        if not offsetDetails.applyToStopwatch1 && not offsetDetails.applyToStopwatch2 then
            Just (NoStopwatchesToApplyOffsetTo offsetType)

        else
            Nothing

    else
        Just (InvalidOffset offsetType)


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
                    [ timeOffsetValidation AddOffsetField AddOffset editDetails.addOffsetDetails ]

                SubtractStopwatchTimeOffset ->
                    [ timeOffsetValidation SubtractOffsetField SubtractOffset editDetails.subtractOffsetDetails
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


isStopwatchFieldInvalid : StopwatchField -> StopwatchOperationEditDetails -> Bool
isStopwatchFieldInvalid field editDetails =
    case editDetails.validationError of
        NoValidationError ->
            False

        StopwatchOperationNotSelected ->
            False

        InvalidOffset AddOffset ->
            field == AddOffsetField

        InvalidOffset SubtractOffset ->
            field == SubtractOffsetField

        NoStopwatchesToApplyOffsetTo AddOffset ->
            field == AddOffsetField

        NoStopwatchesToApplyOffsetTo SubtractOffset ->
            field == SubtractOffsetField

        SubtractOffsetTooLarge _ _ ->
            field == SubtractOffsetField

        InvalidScaleFactor ->
            field == ScaleFactorField

        ScaleFactorOne ->
            field == ScaleFactorField

        InvalidDistance ExpectedDistance ->
            field == ExpectedDistanceManualField

        InvalidDistance ActualDistance ->
            field == ActualDistanceField

        ActualDistanceEqualsExpectedDistance ->
            field == ExpectedDistanceManualField || field == ActualDistanceField


isAddOffsetFieldInvalid : StopwatchOperationEditDetails -> Bool
isAddOffsetFieldInvalid editDetails =
    isStopwatchFieldInvalid AddOffsetField editDetails


isSubtractOffsetFieldInvalid : StopwatchOperationEditDetails -> Bool
isSubtractOffsetFieldInvalid editDetails =
    isStopwatchFieldInvalid SubtractOffsetField editDetails


isScaleFactorFieldInvalid : StopwatchOperationEditDetails -> Bool
isScaleFactorFieldInvalid editDetails =
    isStopwatchFieldInvalid ScaleFactorField editDetails


isExpectedDistanceFieldInvalid : StopwatchOperationEditDetails -> Bool
isExpectedDistanceFieldInvalid editDetails =
    isStopwatchFieldInvalid ExpectedDistanceManualField editDetails


isActualDistanceFieldInvalid : StopwatchOperationEditDetails -> Bool
isActualDistanceFieldInvalid editDetails =
    isStopwatchFieldInvalid ActualDistanceField editDetails
