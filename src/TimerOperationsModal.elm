module TimerOperationsModal exposing (timerOperationsButtons, timerOperationsDialogSizer, timerOperationsDialogTitle, timerOperationsModalBody)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import DataEntry exposing (Entry, IntegerEntry)
import Html exposing (Html, div, hr, label, text)
import Html.Attributes exposing (class, for)
import Html.Events exposing (onClick)
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
        , validateEditDetails
        )
import ViewCommon exposing (normalButton, outlineButton)


timerOperationsDialogTitle : String
timerOperationsDialogTitle =
    "Timer operations"


radioButton : String -> TimerOperation -> String -> TimerOperationEditDetails -> Grid.Column Msg
radioButton elementId operation labelText editDetails =
    sizedRadioButton Col.xs4 elementId operation labelText editDetails


sizedRadioButton : Col.Option Msg -> String -> TimerOperation -> String -> TimerOperationEditDetails -> Grid.Column Msg
sizedRadioButton size elementId operation labelText editDetails =
    Grid.col [ size ]
        [ Radio.radio
            [ Radio.id elementId
            , Radio.checked (editDetails.operation == operation)
            , Radio.onClick (TimerOperationEdit (ChangeOperation operation))
            ]
            labelText
        ]


checkBoxRowApplyToLabel : Grid.Column Msg
checkBoxRowApplyToLabel =
    Grid.col [ Col.xs2, Col.offsetXs1 ] [ text "Apply to:" ]


applyToTimerCheckbox : String -> OffsetType -> WhichTimer -> TimerOperationEditDetails -> Grid.Column Msg
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
    Grid.col [ Col.xs3 ]
        [ Checkbox.checkbox
            [ Checkbox.id elementId
            , Checkbox.checked currentValue
            , Checkbox.onCheck (TimerOperationEdit << TimerCheckboxChanged offsetType timer)
            , Checkbox.disabled (editDetails.operation /= matchingOperation)
            ]
            labelText
        ]


inputTextFieldOptions : (TimerOperationEditDetails -> Entry a) -> TimerField -> TimerOperation -> (TimerOperationEditDetails -> Bool) -> TimerOperationEditDetails -> List (Input.Option Msg)
inputTextFieldOptions rangeEntryGetter field option validator editDetails =
    let
        dangerAttributes : List (Input.Option Msg)
        dangerAttributes =
            if validator editDetails then
                [ Input.danger ]

            else
                []
    in
    [ Input.value (rangeEntryGetter editDetails).enteredValue
    , Input.onInput (TimerOperationEdit << TimerFieldEdited field)
    , Input.disabled (editDetails.operation /= option)
    ]
        ++ dangerAttributes


inputTextField : (TimerOperationEditDetails -> Entry a) -> TimerField -> TimerOperation -> (TimerOperationEditDetails -> Bool) -> TimerOperationEditDetails -> Grid.Column Msg
inputTextField rangeEntryGetter field option validator editDetails =
    Grid.col
        [ Col.xs2 ]
        [ Input.text (inputTextFieldOptions rangeEntryGetter field option validator editDetails) ]


distanceField : (TimerOperationEditDetails -> IntegerEntry) -> TimerField -> (TimerOperationEditDetails -> Bool) -> TimerOperationEditDetails -> Grid.Column Msg
distanceField rangeEntryGetter field validator editDetails =
    let
        dangerAttributes : List (Input.Option Msg)
        dangerAttributes =
            if validator editDetails then
                [ Input.danger ]

            else
                []
    in
    Grid.col [ Col.xs2 ]
        [ InputGroup.config
            (InputGroup.text (inputTextFieldOptions rangeEntryGetter field ApplyDistanceBasedTimerScaleFactor validator editDetails))
            |> InputGroup.successors
                [ InputGroup.span [] [ text "m" ] ]
            |> InputGroup.view
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
            Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
                [ checkBoxRowApplyToLabel
                , applyToTimerCheckbox ("apply" ++ offsetTypeAsString ++ "OffsetToTimer1Checkbox") offsetType TimerOne editDetails
                , applyToTimerCheckbox ("apply" ++ offsetTypeAsString ++ "OffsetToTimer2Checkbox") offsetType TimerTwo editDetails
                ]


timerOperationsModalBody : TimerOperationEditDetails -> Html Msg
timerOperationsModalBody editDetails =
    div []
        [ Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "addOffsetRadioButton" AddTimerTimeOffset "Add offset to all times" editDetails
            , inputTextField (.addOffsetDetails >> .offset) AddOffsetField AddTimerTimeOffset isAddOffsetFieldInvalid editDetails
            ]
        , applyToTimerCheckboxesRow AddOffset "Add" editDetails
        , hr [] []
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "subtractOffsetRadioButton" SubtractTimerTimeOffset "Subtract offset from all times" editDetails
            , inputTextField (.subtractOffsetDetails >> .offset) SubtractOffsetField SubtractTimerTimeOffset isSubtractOffsetFieldInvalid editDetails
            ]
        , applyToTimerCheckboxesRow SubtractOffset "Subtract" editDetails
        , hr [] []
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "applyScaleFactorRadioButton" ApplyTimerScaleFactor "Apply scale factor to all times:" editDetails
            , inputTextField .manualScaleFactor ScaleFactorField ApplyTimerScaleFactor isScaleFactorFieldInvalid editDetails
            ]
        , hr [] []
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ sizedRadioButton Col.xs6 "applyDistanceBasedScaleFactorRadioButton" ApplyDistanceBasedTimerScaleFactor "Apply distance-based scale factor to all times:" editDetails
            ]
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ Grid.col [ Col.xs3, Col.offsetXs1 ] [ text "Expected distance" ]
            , distanceField .expectedDistance ExpectedDistanceManualField isExpectedDistanceFieldInvalid editDetails
            , Grid.col [ Col.xs3 ] [ text "Actual distance" ]
            , distanceField .actualDistance ActualDistanceField isActualDistanceFieldInvalid editDetails
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


timerOperationsDialogSizer : Modal.Config Msg -> Modal.Config Msg
timerOperationsDialogSizer =
    Modal.large
