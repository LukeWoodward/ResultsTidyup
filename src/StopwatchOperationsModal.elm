module StopwatchOperationsModal exposing (stopwatchOperationsButtons, stopwatchOperationsDialogSizer, stopwatchOperationsDialogTitle, stopwatchOperationsModalBody)

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
import Stopwatch exposing (WhichStopwatch(..))
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
        , StopwatchesToApplyTo(..)
        , isActualDistanceFieldInvalid
        , isAddOffsetFieldInvalid
        , isExpectedDistanceFieldInvalid
        , isScaleFactorFieldInvalid
        , isSubtractOffsetFieldInvalid
        , validateEditDetails
        )
import TimeHandling exposing (formatTime)
import ViewCommon exposing (normalButton, outlineButton)


stopwatchOperationsDialogTitle : String
stopwatchOperationsDialogTitle =
    "Stopwatch operations"


radioButton : String -> StopwatchOperation -> String -> StopwatchOperationEditDetails -> Grid.Column Msg
radioButton elementId operation labelText editDetails =
    sizedRadioButton Col.xs4 elementId operation labelText editDetails


sizedRadioButton : Col.Option Msg -> String -> StopwatchOperation -> String -> StopwatchOperationEditDetails -> Grid.Column Msg
sizedRadioButton size elementId operation labelText editDetails =
    Grid.col [ size ]
        [ Radio.radio
            [ Radio.id elementId
            , Radio.checked (editDetails.operation == operation)
            , Radio.onClick (StopwatchOperationEdit (ChangeOperation operation))
            ]
            labelText
        ]


checkBoxRowApplyToLabel : Grid.Column Msg
checkBoxRowApplyToLabel =
    Grid.col [ Col.xs2, Col.offsetXs1 ] [ text "Apply to:" ]


applyToStopwatchCheckbox : String -> OffsetType -> WhichStopwatch -> StopwatchOperationEditDetails -> Grid.Column Msg
applyToStopwatchCheckbox elementId offsetType stopwatch editDetails =
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
            case stopwatch of
                StopwatchOne ->
                    offset.applyToStopwatch1

                StopwatchTwo ->
                    offset.applyToStopwatch2

        labelText : String
        labelText =
            case stopwatch of
                StopwatchOne ->
                    "Stopwatch 1"

                StopwatchTwo ->
                    "Stopwatch 2"

        matchingOperation : StopwatchOperation
        matchingOperation =
            case offsetType of
                AddOffset ->
                    AddStopwatchTimeOffset

                SubtractOffset ->
                    SubtractStopwatchTimeOffset
    in
    Grid.col [ Col.xs3 ]
        [ Checkbox.checkbox
            [ Checkbox.id elementId
            , Checkbox.checked currentValue
            , Checkbox.onCheck (StopwatchOperationEdit << StopwatchCheckboxChanged offsetType stopwatch)
            , Checkbox.disabled (editDetails.operation /= matchingOperation)
            ]
            labelText
        ]


inputTextFieldOptions : (StopwatchOperationEditDetails -> Entry a) -> StopwatchField -> StopwatchOperation -> (StopwatchOperationEditDetails -> Bool) -> StopwatchOperationEditDetails -> List (Input.Option Msg)
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
    , Input.onInput (StopwatchOperationEdit << StopwatchFieldEdited field)
    , Input.disabled (editDetails.operation /= option)
    ]
        ++ dangerAttributes


inputTextField : (StopwatchOperationEditDetails -> Entry a) -> StopwatchField -> StopwatchOperation -> (StopwatchOperationEditDetails -> Bool) -> StopwatchOperationEditDetails -> Grid.Column Msg
inputTextField rangeEntryGetter field option validator editDetails =
    Grid.col
        [ Col.xs2 ]
        [ Input.text (inputTextFieldOptions rangeEntryGetter field option validator editDetails) ]


distanceField : (StopwatchOperationEditDetails -> IntegerEntry) -> StopwatchField -> (StopwatchOperationEditDetails -> Bool) -> StopwatchOperationEditDetails -> Grid.Column Msg
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
            (InputGroup.text (inputTextFieldOptions rangeEntryGetter field ApplyDistanceBasedStopwatchScaleFactor validator editDetails))
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


validationErrorToString : StopwatchOperationValidationError -> String
validationErrorToString validationError =
    case validationError of
        NoValidationError ->
            ""

        StopwatchOperationNotSelected ->
            "Please select a stopwatch operation"

        InvalidOffset _ ->
            "The offset entered is not valid.  Please enter a valid offset in minutes and seconds, e.g 02:17"

        NoStopwatchesToApplyOffsetTo _ ->
            "No stopwatches have been selected to apply the offset to.  Please select one or both stopwatches"

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


validationErrorRow : StopwatchOperationValidationError -> Html Msg
validationErrorRow validationError =
    div [ class "validation-error" ] [ text (validationErrorToString validationError) ]


applyToStopwatchCheckboxesRow : OffsetType -> String -> StopwatchOperationEditDetails -> Html Msg
applyToStopwatchCheckboxesRow offsetType offsetTypeAsString editDetails =
    case editDetails.stopwatchesToApplyTo of
        OneStopwatch ->
            text ""

        TwoStopwatches ->
            Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
                [ checkBoxRowApplyToLabel
                , applyToStopwatchCheckbox ("apply" ++ offsetTypeAsString ++ "OffsetToStopwatch1Checkbox") offsetType StopwatchOne editDetails
                , applyToStopwatchCheckbox ("apply" ++ offsetTypeAsString ++ "OffsetToStopwatch2Checkbox") offsetType StopwatchTwo editDetails
                ]


stopwatchOperationsModalBody : StopwatchOperationEditDetails -> Html Msg
stopwatchOperationsModalBody editDetails =
    div []
        [ Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "addOffsetRadioButton" AddStopwatchTimeOffset "Add offset to all times" editDetails
            , inputTextField (.addOffsetDetails >> .offset) AddOffsetField AddStopwatchTimeOffset isAddOffsetFieldInvalid editDetails
            ]
        , applyToStopwatchCheckboxesRow AddOffset "Add" editDetails
        , hr [] []
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "subtractOffsetRadioButton" SubtractStopwatchTimeOffset "Subtract offset from all times" editDetails
            , inputTextField (.subtractOffsetDetails >> .offset) SubtractOffsetField SubtractStopwatchTimeOffset isSubtractOffsetFieldInvalid editDetails
            ]
        , applyToStopwatchCheckboxesRow SubtractOffset "Subtract" editDetails
        , hr [] []
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "applyScaleFactorRadioButton" ApplyStopwatchScaleFactor "Apply scale factor to all times:" editDetails
            , inputTextField .manualScaleFactor ScaleFactorField ApplyStopwatchScaleFactor isScaleFactorFieldInvalid editDetails
            ]
        , hr [] []
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ sizedRadioButton Col.xs6 "applyDistanceBasedScaleFactorRadioButton" ApplyDistanceBasedStopwatchScaleFactor "Apply distance-based scale factor to all times:" editDetails
            ]
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ Grid.col [ Col.xs3, Col.offsetXs1 ] [ text "Expected distance" ]
            , distanceField .expectedDistance ExpectedDistanceManualField isExpectedDistanceFieldInvalid editDetails
            , Grid.col [ Col.xs3 ] [ text "Actual distance" ]
            , distanceField .actualDistance ActualDistanceField isActualDistanceFieldInvalid editDetails
            ]
        , validationErrorRow editDetails.validationError
        ]


processStopwatchOperationsButtonText : StopwatchOperationEditDetails -> Maybe String
processStopwatchOperationsButtonText editDetails =
    case editDetails.operation of
        NoOperationSelected ->
            Nothing

        AddStopwatchTimeOffset ->
            Just "Add offset"

        SubtractStopwatchTimeOffset ->
            Just "Subtract offset"

        ApplyStopwatchScaleFactor ->
            Just "Apply scale factor"

        ApplyDistanceBasedStopwatchScaleFactor ->
            Just "Apply scale factor"


stopwatchOperationsButtons : StopwatchOperationEditDetails -> List (Html Msg)
stopwatchOperationsButtons editDetails =
    let
        processButtonText : Maybe String
        processButtonText =
            processStopwatchOperationsButtonText editDetails

        processButtons : List (Html Msg)
        processButtons =
            case processButtonText of
                Just buttonText ->
                    [ normalButton (ApplyStopwatchOperation editDetails) [] buttonText ]

                Nothing ->
                    []
    in
    processButtons ++ [ outlineButton CloseModal [] "Close" ]


stopwatchOperationsDialogSizer : Modal.Config Msg -> Modal.Config Msg
stopwatchOperationsDialogSizer =
    Modal.large
