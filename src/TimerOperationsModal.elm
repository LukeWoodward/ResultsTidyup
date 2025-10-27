module TimerOperationsModal exposing (timerOperationsButtons, timerOperationsDialogSize, timerOperationsDialogTitle, timerOperationsModalBody)

import DataEntry exposing (Entry, IntegerEntry)
import Html exposing (Html, div, hr, input, label, span, text)
import Html.Attributes exposing (checked, class, disabled, for, id, type_, value)
import Html.Events exposing (onCheck, onInput)
import Msg exposing (Msg(..))
import TimeHandling exposing (formatTime)
import Timer exposing (WhichTimer(..))
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
        , isActualDistanceFieldInvalid
        , isAddOffsetFieldInvalid
        , isExpectedDistanceFieldInvalid
        , isScaleFactorFieldInvalid
        , isSubtractOffsetFieldInvalid
        )
import ViewCommon exposing (ModalSize(..), normalButton, outlineButton, radioButton)


timerOperationsDialogTitle : String
timerOperationsDialogTitle =
    "Timer operations"


timerRadioButton : String -> String -> String -> TimerOperation -> TimerOperationEditDetails -> Html Msg
timerRadioButton containerClass elementId labelText operation editDetails =
    radioButton containerClass elementId (editDetails.operation == operation) labelText (TimerOperationEdit (ChangeOperation operation))


checkBoxRowApplyToLabel : Html Msg
checkBoxRowApplyToLabel =
    div [ class "col-4 col-md-2 offset-md-1 col-form-label" ] [ text "Apply to:" ]


applyToTimerCheckbox : String -> OffsetType -> WhichTimer -> TimerOperationEditDetails -> Html Msg
applyToTimerCheckbox elementId offsetType timer editDetails =
    let
        offset : OffsetDetails
        offset =
            case offsetType of
                AddOffset ->
                    editDetails.addOffsetDetails

                SubtractOffset ->
                    editDetails.subtractOffsetDetails

        currentValue : Bool
        currentValue =
            case timer of
                TimerOne ->
                    offset.applyToTimer1

                TimerTwo ->
                    offset.applyToTimer2

        labelText : String
        labelText =
            case timer of
                TimerOne ->
                    "Timer 1"

                TimerTwo ->
                    "Timer 2"

        matchingOperation : TimerOperation
        matchingOperation =
            case offsetType of
                AddOffset ->
                    AddTimerTimeOffset

                SubtractOffset ->
                    SubtractTimerTimeOffset
    in
    div [ class "col-4 col-md-2 col-form-label form-check" ]
        [ input
            [ id elementId
            , type_ "checkbox"
            , class "form-check-input"
            , checked currentValue
            , disabled (editDetails.operation /= matchingOperation)
            , onCheck (TimerOperationEdit << TimerCheckboxChanged offsetType timer)
            ]
            []
        , label [ for elementId, class "form-check-label" ]
            [ text labelText ]
        ]


inputTextFieldOptions : String -> (TimerOperationEditDetails -> Entry a) -> TimerField -> TimerOperation -> (TimerOperationEditDetails -> Bool) -> TimerOperationEditDetails -> List (Html.Attribute Msg)
inputTextFieldOptions inputType rangeEntryGetter field option validator editDetails =
    let
        validationAttributes : List (Html.Attribute Msg)
        validationAttributes =
            if validator editDetails then
                [ class "is-invalid" ]

            else
                []
    in
    type_ inputType
        :: class "form-control"
        :: value (rangeEntryGetter editDetails).enteredValue
        :: onInput (TimerOperationEdit << TimerFieldEdited field)
        :: disabled (editDetails.operation /= option)
        :: validationAttributes


inputTextField : (TimerOperationEditDetails -> Entry a) -> TimerField -> TimerOperation -> (TimerOperationEditDetails -> Bool) -> TimerOperationEditDetails -> Html Msg
inputTextField rangeEntryGetter field option validator editDetails =
    div [ class "col-6 col-md-2" ]
        [ input (inputTextFieldOptions "text" rangeEntryGetter field option validator editDetails) [] ]


bareInputNumberField : (TimerOperationEditDetails -> Entry a) -> TimerField -> TimerOperation -> (TimerOperationEditDetails -> Bool) -> TimerOperationEditDetails -> Html Msg
bareInputNumberField rangeEntryGetter field option validator editDetails =
    input (inputTextFieldOptions "number" rangeEntryGetter field option validator editDetails) []


inputNumberField : (TimerOperationEditDetails -> Entry a) -> TimerField -> TimerOperation -> (TimerOperationEditDetails -> Bool) -> TimerOperationEditDetails -> Html Msg
inputNumberField rangeEntryGetter field option validator editDetails =
    div [ class "col-6 col-md-2" ]
        [ bareInputNumberField rangeEntryGetter field option validator editDetails ]


distanceField : (TimerOperationEditDetails -> IntegerEntry) -> TimerField -> (TimerOperationEditDetails -> Bool) -> TimerOperationEditDetails -> Html Msg
distanceField rangeEntryGetter field validator editDetails =
    div [ class "input-group" ]
        [ bareInputNumberField rangeEntryGetter field ApplyDistanceBasedTimerScaleFactor validator editDetails
        , span [ class "input-group-text" ] [ text "m" ]
        ]


distanceTypeToString : DistanceType -> String
distanceTypeToString distanceType =
    case distanceType of
        ExpectedDistance ->
            "expected distance"

        ActualDistance ->
            "actual distance"


validationErrorToString : TimerOperationValidationError -> String
validationErrorToString validationError =
    case validationError of
        NoValidationError ->
            ""

        TimerOperationNotSelected ->
            "Please select a timer operation"

        InvalidOffset _ ->
            "The offset entered is not valid.  Please enter a valid offset in minutes and seconds, e.g 02:17"

        NoTimersToApplyOffsetTo _ ->
            "No timers have been selected to apply the offset to.  Please select one or both timers"

        SubtractOffsetTooLarge fastestTime offset ->
            "It is not possible to subtract an offset of " ++ formatTime offset ++ " from the times because the fastest time is " ++ formatTime fastestTime

        InvalidScaleFactor ->
            "The scale factor entered is not valid.  Please enter a valid scale factor, e.g. 1.23"

        ScaleFactorOne ->
            "The scale factor to apply cannot be 1.  Please choose another number"

        InvalidDistance distanceType ->
            "The " ++ distanceTypeToString distanceType ++ " is not valid.  Please enter a valid distance as a whole number of metres"

        ActualDistanceEqualsExpectedDistance ->
            "The actual distance and expected distance are equal.  Please enter two different distances"


validationErrorRow : TimerOperationValidationError -> Html Msg
validationErrorRow validationError =
    div [ class "validation-error" ] [ text (validationErrorToString validationError) ]


applyToTimerCheckboxesRow : OffsetType -> String -> TimerOperationEditDetails -> Html Msg
applyToTimerCheckboxesRow offsetType offsetTypeAsString editDetails =
    case editDetails.timersToApplyTo of
        OneTimer ->
            text ""

        TwoTimers ->
            div [ class "row mb-3" ]
                [ checkBoxRowApplyToLabel
                , applyToTimerCheckbox ("apply" ++ offsetTypeAsString ++ "OffsetToTimer1Checkbox") offsetType TimerOne editDetails
                , applyToTimerCheckbox ("apply" ++ offsetTypeAsString ++ "OffsetToTimer2Checkbox") offsetType TimerTwo editDetails
                ]


timerOperationsModalBody : TimerOperationEditDetails -> Html Msg
timerOperationsModalBody editDetails =
    div []
        [ div [ class "row mb-3" ]
            [ timerRadioButton "col-6 col-md-4" "addOffsetRadioButton" "Add offset to all times:" AddTimerTimeOffset editDetails
            , inputTextField (.addOffsetDetails >> .offset) AddOffsetField AddTimerTimeOffset isAddOffsetFieldInvalid editDetails
            ]
        , applyToTimerCheckboxesRow AddOffset "Add" editDetails
        , hr [] []
        , div [ class "row mb-3" ]
            [ timerRadioButton "col-6 col-md-4" "subtractOffsetRadioButton" "Subtract offset from all times:" SubtractTimerTimeOffset editDetails
            , inputTextField (.subtractOffsetDetails >> .offset) SubtractOffsetField SubtractTimerTimeOffset isSubtractOffsetFieldInvalid editDetails
            ]
        , applyToTimerCheckboxesRow SubtractOffset "Subtract" editDetails
        , hr [] []
        , div [ class "row mb-3" ]
            [ timerRadioButton "col-6 col-md-4" "applyScaleFactorRadioButton" "Apply scale factor to all times:" ApplyTimerScaleFactor editDetails
            , inputNumberField .manualScaleFactor ScaleFactorField ApplyTimerScaleFactor isScaleFactorFieldInvalid editDetails
            ]
        , hr [] []
        , div [ class "row mb-3" ]
            [ timerRadioButton "col-12 col-md-6" "applyDistanceBasedScaleFactorRadioButton" "Apply distance-based scale factor to all times:" ApplyDistanceBasedTimerScaleFactor editDetails
            ]
        , div [ class "row mb-4" ]
            [ div [ class "col-5 col-md-3 offset-1 col-form-label" ] [ text "Expected distance" ]
            , div [ class "col-6 col-md-2" ] [ distanceField .expectedDistance ExpectedDistanceManualField isExpectedDistanceFieldInvalid editDetails ]
            , div [ class "col-5 col-md-3 offset-1 col-form-label" ] [ text "Actual distance" ]
            , div [ class "col-6 col-md-2" ] [ distanceField .actualDistance ActualDistanceField isActualDistanceFieldInvalid editDetails ]
            ]
        , validationErrorRow editDetails.validationError
        ]


processTimerOperationsButtonText : TimerOperationEditDetails -> Maybe String
processTimerOperationsButtonText editDetails =
    case editDetails.operation of
        NoOperationSelected ->
            Nothing

        AddTimerTimeOffset ->
            Just "Add offset"

        SubtractTimerTimeOffset ->
            Just "Subtract offset"

        ApplyTimerScaleFactor ->
            Just "Apply scale factor"

        ApplyDistanceBasedTimerScaleFactor ->
            Just "Apply scale factor"


timerOperationsButtons : TimerOperationEditDetails -> List (Html Msg)
timerOperationsButtons editDetails =
    let
        processButtonText : Maybe String
        processButtonText =
            processTimerOperationsButtonText editDetails

        processButtons : List (Html Msg)
        processButtons =
            case processButtonText of
                Just buttonText ->
                    [ normalButton (ApplyTimerOperation editDetails) [] buttonText ]

                Nothing ->
                    []
    in
    processButtons ++ [ outlineButton CloseModal [] "Close" ]


timerOperationsDialogSize : ModalSize
timerOperationsDialogSize =
    Large
