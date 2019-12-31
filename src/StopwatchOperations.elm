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
    , tryApplyOperationToStopwatchData
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
import Stopwatch exposing (DoubleStopwatchData, Stopwatches(..), createMergedTable)


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


minimumWithZero : List Int -> Int
minimumWithZero ints =
    List.minimum ints
        |> Maybe.withDefault 0


getFastestTime : Stopwatches -> Int
getFastestTime stopwatches =
    case stopwatches of
        None ->
            0

        Single _ times ->
            minimumWithZero times

        Double doubleStopwatchData ->
            min (minimumWithZero doubleStopwatchData.times1) (minimumWithZero doubleStopwatchData.times2)


applyStopwatchOffset : Int -> Bool -> Bool -> Stopwatches -> Stopwatches
applyStopwatchOffset offset applyToStopwatch1 applyToStopwatch2 stopwatches =
    case stopwatches of
        None ->
            stopwatches

        Single filename times ->
            Single filename (List.map ((+) offset) times)

        Double doubleStopwatchData ->
            let
                newTimes1 : List Int
                newTimes1 =
                    if applyToStopwatch1 then
                        List.map ((+) offset) doubleStopwatchData.times1

                    else
                        doubleStopwatchData.times1

                newTimes2 : List Int
                newTimes2 =
                    if applyToStopwatch2 then
                        List.map ((+) offset) doubleStopwatchData.times2

                    else
                        doubleStopwatchData.times2
            in
            recreateDoubleStopwatchData newTimes1 newTimes2 doubleStopwatchData


recreateDoubleStopwatchData : List Int -> List Int -> DoubleStopwatchData -> Stopwatches
recreateDoubleStopwatchData times1 times2 doubleStopwatchData =
    createMergedTable times1 times2 doubleStopwatchData.filename1 doubleStopwatchData.filename2


applyScaleFactor : Float -> Stopwatches -> Stopwatches
applyScaleFactor scaleFactor stopwatches =
    let
        applyFactor : Int -> Int
        applyFactor time =
            round (toFloat time * scaleFactor)
    in
    case stopwatches of
        None ->
            stopwatches

        Single filename times ->
            Single filename (List.map applyFactor times)

        Double doubleStopwatchData ->
            recreateDoubleStopwatchData (List.map applyFactor doubleStopwatchData.times1) (List.map applyFactor doubleStopwatchData.times2) doubleStopwatchData


tryApplyOperationToStopwatchData : StopwatchOperationEditDetails -> Stopwatches -> Result StopwatchOperationValidationError Stopwatches
tryApplyOperationToStopwatchData editDetails stopwatches =
    let
        validationError : StopwatchOperationValidationError
        validationError =
            validateEditDetails (getFastestTime stopwatches) editDetails
    in
    if validationError == NoValidationError then
        case editDetails.operation of
            NoOperationSelected ->
                Ok stopwatches

            AddStopwatchTimeOffset ->
                case editDetails.addOffsetDetails.offset.parsedValue of
                    Just offset ->
                        applyStopwatchOffset offset editDetails.addOffsetDetails.applyToStopwatch1 editDetails.addOffsetDetails.applyToStopwatch2 stopwatches
                            |> Ok

                    Nothing ->
                        Ok stopwatches

            SubtractStopwatchTimeOffset ->
                case editDetails.subtractOffsetDetails.offset.parsedValue of
                    Just offset ->
                        applyStopwatchOffset -offset editDetails.subtractOffsetDetails.applyToStopwatch1 editDetails.subtractOffsetDetails.applyToStopwatch2 stopwatches
                            |> Ok

                    Nothing ->
                        Ok stopwatches

            ApplyStopwatchScaleFactor ->
                case editDetails.manualScaleFactor.parsedValue of
                    Just scaleFactor ->
                        applyScaleFactor scaleFactor stopwatches
                            |> Ok

                    Nothing ->
                        Ok stopwatches

            ApplyDistanceBasedStopwatchScaleFactor ->
                case ( editDetails.expectedDistance.parsedValue, editDetails.actualDistance.parsedValue ) of
                    ( Just expectedDistance, Just actualDistance ) ->
                        applyScaleFactor (toFloat expectedDistance / toFloat actualDistance) stopwatches
                            |> Ok

                    _ ->
                        Ok stopwatches

    else
        Err validationError
