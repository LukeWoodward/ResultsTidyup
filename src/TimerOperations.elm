module TimerOperations exposing
    ( DistanceType(..)
    , OffsetDetails
    , OffsetType(..)
    , TimerField(..)
    , TimerOperation(..)
    , TimerOperationChangeType(..)
    , TimerOperationEditDetails
    , TimerOperationValidationError(..)
    , TimersToApplyTo(..)
    , TimersToModify(..)
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
import Timer exposing (DoubleTimerData, Timers(..), WhichTimer(..), createMergedTable)


type TimersToModify
    = TimerOneOnly
    | TimerTwoOnly
    | BothTimers


type TimerOperation
    = NoOperationSelected
    | AddTimerTimeOffset
    | SubtractTimerTimeOffset
    | ApplyTimerScaleFactor
    | ApplyDistanceBasedTimerScaleFactor


type alias OffsetDetails =
    { offset : IntegerEntry
    , applyToTimer1 : Bool
    , applyToTimer2 : Bool
    }


type DistanceType
    = ActualDistance
    | ExpectedDistance


type TimerField
    = AddOffsetField
    | SubtractOffsetField
    | ScaleFactorField
    | ActualDistanceField
    | ExpectedDistanceManualField


type OffsetType
    = AddOffset
    | SubtractOffset


type TimersToApplyTo
    = OneTimer
    | TwoTimers


type TimerOperationValidationError
    = NoValidationError
    | TimerOperationNotSelected
    | InvalidOffset OffsetType
    | NoTimersToApplyOffsetTo OffsetType
    | SubtractOffsetTooLarge Int Int
    | InvalidScaleFactor
    | ScaleFactorOne
    | InvalidDistance DistanceType
    | ActualDistanceEqualsExpectedDistance


type alias TimerOperationEditDetails =
    { operation : TimerOperation
    , timersToApplyTo : TimersToApplyTo
    , addOffsetDetails : OffsetDetails
    , subtractOffsetDetails : OffsetDetails
    , manualScaleFactor : FloatEntry
    , expectedDistance : IntegerEntry
    , actualDistance : IntegerEntry
    , validationError : TimerOperationValidationError
    }


type TimerOperationChangeType
    = ChangeOperation TimerOperation
    | TimerFieldEdited TimerField String
    | TimerCheckboxChanged OffsetType WhichTimer Bool


emptyOffset : OffsetDetails
emptyOffset =
    OffsetDetails emptyEntry False False


emptyEditDetailsFor : TimersToApplyTo -> TimerOperationEditDetails
emptyEditDetailsFor timersToApplyTo =
    { operation = NoOperationSelected
    , timersToApplyTo = timersToApplyTo
    , addOffsetDetails = emptyOffset
    , subtractOffsetDetails = emptyOffset
    , manualScaleFactor = emptyEntry
    , expectedDistance = emptyEntry
    , actualDistance = emptyEntry
    , validationError = NoValidationError
    }


emptyEditDetailsFromTimers : Timers -> TimerOperationEditDetails
emptyEditDetailsFromTimers timers =
    let
        timersToApplyTo : TimersToApplyTo
        timersToApplyTo =
            case timers of
                None ->
                    OneTimer

                Single _ _ ->
                    OneTimer

                Double _ ->
                    TwoTimers
    in
    emptyEditDetailsFor timersToApplyTo


isDoubleTimerData : Timers -> Bool
isDoubleTimerData timers =
    case timers of
        None ->
            False

        Single _ _ ->
            False

        Double _ ->
            True


timeOffsetValidation : OffsetType -> Timers -> OffsetDetails -> Maybe TimerOperationValidationError
timeOffsetValidation offsetType timers offsetDetails =
    if isPositive offsetDetails.offset then
        if isDoubleTimerData timers && not offsetDetails.applyToTimer1 && not offsetDetails.applyToTimer2 then
            Just (NoTimersToApplyOffsetTo offsetType)

        else
            Nothing

    else
        Just (InvalidOffset offsetType)


offsetToRemoveLessThanFastestTime : Timers -> OffsetDetails -> Maybe TimerOperationValidationError
offsetToRemoveLessThanFastestTime timers offsetDetails =
    case offsetDetails.offset.parsedValue of
        Just someOffset ->
            let
                fastestTime : Int
                fastestTime =
                    getFastestTime timers
            in
            if someOffset > fastestTime then
                Just (SubtractOffsetTooLarge fastestTime someOffset)

            else
                Nothing

        Nothing ->
            Nothing


scaleFactorValidation : FloatEntry -> Maybe TimerOperationValidationError
scaleFactorValidation scaleFactorEntry =
    if scaleFactorEntry.parsedValue == Just 1.0 then
        Just ScaleFactorOne

    else if isPositive scaleFactorEntry then
        Nothing

    else
        Just InvalidScaleFactor


distanceValidation : DistanceType -> IntegerEntry -> Maybe TimerOperationValidationError
distanceValidation distanceType distanceEntry =
    if isPositive distanceEntry then
        Nothing

    else
        Just (InvalidDistance distanceType)


equalDistanceValidation : IntegerEntry -> IntegerEntry -> Maybe TimerOperationValidationError
equalDistanceValidation expectedEntry actualEntry =
    if expectedEntry.parsedValue /= Nothing && expectedEntry.parsedValue == actualEntry.parsedValue then
        Just ActualDistanceEqualsExpectedDistance

    else
        Nothing


validateEditDetails : Timers -> TimerOperationEditDetails -> TimerOperationValidationError
validateEditDetails timers editDetails =
    let
        allErrors : List (Maybe TimerOperationValidationError)
        allErrors =
            case editDetails.operation of
                NoOperationSelected ->
                    [ Just TimerOperationNotSelected ]

                AddTimerTimeOffset ->
                    [ timeOffsetValidation AddOffset timers editDetails.addOffsetDetails ]

                SubtractTimerTimeOffset ->
                    [ timeOffsetValidation SubtractOffset timers editDetails.subtractOffsetDetails
                    , offsetToRemoveLessThanFastestTime timers editDetails.subtractOffsetDetails
                    ]

                ApplyTimerScaleFactor ->
                    [ scaleFactorValidation editDetails.manualScaleFactor ]

                ApplyDistanceBasedTimerScaleFactor ->
                    [ distanceValidation ExpectedDistance editDetails.expectedDistance
                    , distanceValidation ActualDistance editDetails.actualDistance
                    , equalDistanceValidation editDetails.expectedDistance editDetails.actualDistance
                    ]
    in
    List.filterMap identity allErrors
        |> List.head
        |> Maybe.withDefault NoValidationError


updateEditDetails : TimerOperationChangeType -> TimerOperationEditDetails -> TimerOperationEditDetails
updateEditDetails change editDetails =
    let
        oldAddOffsetDetails : OffsetDetails
        oldAddOffsetDetails =
            editDetails.addOffsetDetails

        oldSubtractOffsetDetails : OffsetDetails
        oldSubtractOffsetDetails =
            editDetails.subtractOffsetDetails

        updatedDetails : TimerOperationEditDetails
        updatedDetails =
            case change of
                ChangeOperation newOperation ->
                    { editDetails | operation = newOperation }

                TimerFieldEdited AddOffsetField newValue ->
                    { editDetails | addOffsetDetails = { oldAddOffsetDetails | offset = integerEntryFromTime newValue } }

                TimerFieldEdited SubtractOffsetField newValue ->
                    { editDetails | subtractOffsetDetails = { oldSubtractOffsetDetails | offset = integerEntryFromTime newValue } }

                TimerFieldEdited ScaleFactorField newValue ->
                    { editDetails | manualScaleFactor = floatEntryFromString newValue }

                TimerFieldEdited ExpectedDistanceManualField newValue ->
                    { editDetails | expectedDistance = integerEntryFromString newValue }

                TimerFieldEdited ActualDistanceField newValue ->
                    { editDetails | actualDistance = integerEntryFromString newValue }

                TimerCheckboxChanged AddOffset timer newValue ->
                    case timer of
                        TimerOne ->
                            { editDetails | addOffsetDetails = { oldAddOffsetDetails | applyToTimer1 = newValue } }

                        TimerTwo ->
                            { editDetails | addOffsetDetails = { oldAddOffsetDetails | applyToTimer2 = newValue } }

                TimerCheckboxChanged SubtractOffset timer newValue ->
                    case timer of
                        TimerOne ->
                            { editDetails | subtractOffsetDetails = { oldSubtractOffsetDetails | applyToTimer1 = newValue } }

                        TimerTwo ->
                            { editDetails | subtractOffsetDetails = { oldSubtractOffsetDetails | applyToTimer2 = newValue } }
    in
    { updatedDetails | validationError = NoValidationError }


isTimerFieldInvalid : TimerField -> TimerOperationEditDetails -> Bool
isTimerFieldInvalid field editDetails =
    case editDetails.validationError of
        NoValidationError ->
            False

        TimerOperationNotSelected ->
            False

        InvalidOffset AddOffset ->
            field == AddOffsetField

        InvalidOffset SubtractOffset ->
            field == SubtractOffsetField

        NoTimersToApplyOffsetTo AddOffset ->
            field == AddOffsetField

        NoTimersToApplyOffsetTo SubtractOffset ->
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


isAddOffsetFieldInvalid : TimerOperationEditDetails -> Bool
isAddOffsetFieldInvalid editDetails =
    isTimerFieldInvalid AddOffsetField editDetails


isSubtractOffsetFieldInvalid : TimerOperationEditDetails -> Bool
isSubtractOffsetFieldInvalid editDetails =
    isTimerFieldInvalid SubtractOffsetField editDetails


isScaleFactorFieldInvalid : TimerOperationEditDetails -> Bool
isScaleFactorFieldInvalid editDetails =
    isTimerFieldInvalid ScaleFactorField editDetails


isExpectedDistanceFieldInvalid : TimerOperationEditDetails -> Bool
isExpectedDistanceFieldInvalid editDetails =
    isTimerFieldInvalid ExpectedDistanceManualField editDetails


isActualDistanceFieldInvalid : TimerOperationEditDetails -> Bool
isActualDistanceFieldInvalid editDetails =
    isTimerFieldInvalid ActualDistanceField editDetails


minimumWithZero : List Int -> Int
minimumWithZero ints =
    List.minimum ints
        |> Maybe.withDefault 0


getFastestTime : Timers -> Int
getFastestTime timers =
    case timers of
        None ->
            0

        Single _ times ->
            minimumWithZero times

        Double doubleTimerData ->
            min (minimumWithZero doubleTimerData.times1) (minimumWithZero doubleTimerData.times2)


applyTimerOffset : Int -> Bool -> Bool -> Timers -> Timers
applyTimerOffset offset applyToTimer1 applyToTimer2 timers =
    case timers of
        None ->
            timers

        Single filename times ->
            Single filename (List.map ((+) offset) times)

        Double doubleTimerData ->
            let
                newTimes1 : List Int
                newTimes1 =
                    if applyToTimer1 then
                        List.map ((+) offset) doubleTimerData.times1

                    else
                        doubleTimerData.times1

                newTimes2 : List Int
                newTimes2 =
                    if applyToTimer2 then
                        List.map ((+) offset) doubleTimerData.times2

                    else
                        doubleTimerData.times2
            in
            Double (recreateDoubleTimerData newTimes1 newTimes2 doubleTimerData)


recreateDoubleTimerData : List Int -> List Int -> DoubleTimerData -> DoubleTimerData
recreateDoubleTimerData times1 times2 doubleTimerData =
    createMergedTable times1 times2 doubleTimerData.filename1 doubleTimerData.filename2


applyScaleFactor : Float -> Timers -> Timers
applyScaleFactor scaleFactor timers =
    let
        applyFactor : Int -> Int
        applyFactor time =
            round (toFloat time * scaleFactor)
    in
    case timers of
        None ->
            timers

        Single filename times ->
            Single filename (List.map applyFactor times)

        Double doubleTimerData ->
            Double (recreateDoubleTimerData (List.map applyFactor doubleTimerData.times1) (List.map applyFactor doubleTimerData.times2) doubleTimerData)


tryApplyOperationToTimerData : TimerOperationEditDetails -> Timers -> Result TimerOperationValidationError Timers
tryApplyOperationToTimerData editDetails timers =
    let
        validationError : TimerOperationValidationError
        validationError =
            validateEditDetails timers editDetails
    in
    if validationError == NoValidationError then
        case editDetails.operation of
            NoOperationSelected ->
                Ok timers

            AddTimerTimeOffset ->
                case editDetails.addOffsetDetails.offset.parsedValue of
                    Just offset ->
                        applyTimerOffset offset editDetails.addOffsetDetails.applyToTimer1 editDetails.addOffsetDetails.applyToTimer2 timers
                            |> Ok

                    Nothing ->
                        Ok timers

            SubtractTimerTimeOffset ->
                case editDetails.subtractOffsetDetails.offset.parsedValue of
                    Just offset ->
                        applyTimerOffset -offset editDetails.subtractOffsetDetails.applyToTimer1 editDetails.subtractOffsetDetails.applyToTimer2 timers
                            |> Ok

                    Nothing ->
                        Ok timers

            ApplyTimerScaleFactor ->
                case editDetails.manualScaleFactor.parsedValue of
                    Just scaleFactor ->
                        applyScaleFactor scaleFactor timers
                            |> Ok

                    Nothing ->
                        Ok timers

            ApplyDistanceBasedTimerScaleFactor ->
                case ( editDetails.expectedDistance.parsedValue, editDetails.actualDistance.parsedValue ) of
                    ( Just expectedDistance, Just actualDistance ) ->
                        applyScaleFactor ((toFloat expectedDistance / toFloat actualDistance) ^ 1.06) timers
                            |> Ok

                    _ ->
                        Ok timers

    else
        Err validationError
