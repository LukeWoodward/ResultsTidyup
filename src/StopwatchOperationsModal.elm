module StopwatchOperationsModal exposing (stopwatchOperationsButtons, stopwatchOperationsDialogTitle, stopwatchOperationsModalBody)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import DataEntry exposing (Entry)
import Html exposing (Html, div, label, text)
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
        , isActualDistanceFieldInvalid
        , isAddOffsetFieldInvalid
        , isExpectedDistanceFieldInvalid
        , isScaleFactorFieldInvalid
        , isSubtractOffsetFieldInvalid
        , validateEditDetails
        )
import TimeHandling exposing (formatTime)


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


checkboxRowIndent : Grid.Column Msg
checkboxRowIndent =
    Grid.col [ Col.xs1 ] []


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


inputTextField : (StopwatchOperationEditDetails -> Entry a) -> StopwatchField -> StopwatchOperation -> (StopwatchOperationEditDetails -> Bool) -> StopwatchOperationEditDetails -> Grid.Column Msg
inputTextField rangeEntryGetter field option validator editDetails =
    let
        dangerAttributes : List (Input.Option Msg)
        dangerAttributes =
            if validator editDetails then
                [ Input.danger ]

            else
                []
    in
    Grid.col [ Col.xs2 ]
        [ Input.text
            ([ Input.value (rangeEntryGetter editDetails).enteredValue
             , Input.onInput (StopwatchOperationEdit << StopwatchFieldEdited field)
             , Input.disabled (editDetails.operation /= option)
             ]
                ++ dangerAttributes
            )
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


stopwatchOperationsModalBody : StopwatchOperationEditDetails -> Html Msg
stopwatchOperationsModalBody editDetails =
    div []
        [ Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "addOffsetRadioButton" AddStopwatchTimeOffset "Add offset to all times" editDetails
            , inputTextField (.addOffsetDetails >> .offset) AddOffsetField AddStopwatchTimeOffset isAddOffsetFieldInvalid editDetails
            ]
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ checkboxRowIndent
            , applyToStopwatchCheckbox "applyAddOffsetToStopwatch1Checkbox" AddOffset StopwatchOne editDetails
            , applyToStopwatchCheckbox "applyAddOffsetToStopwatch2Checkbox" AddOffset StopwatchTwo editDetails
            ]
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "subtractOffsetRadioButton" SubtractStopwatchTimeOffset "Subtract offset from all times" editDetails
            , inputTextField (.subtractOffsetDetails >> .offset) SubtractOffsetField SubtractStopwatchTimeOffset isSubtractOffsetFieldInvalid editDetails
            ]
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ checkboxRowIndent
            , applyToStopwatchCheckbox "applySubtractOffsetToStopwatch1Checkbox" SubtractOffset StopwatchOne editDetails
            , applyToStopwatchCheckbox "applySubtractOffsetToStopwatch2Checkbox" SubtractOffset StopwatchTwo editDetails
            ]
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "applyScaleFactorRadioButton" ApplyStopwatchScaleFactor "Apply scale factor to all times" editDetails
            , inputTextField .manualScaleFactor ScaleFactorField ApplyStopwatchScaleFactor isScaleFactorFieldInvalid editDetails
            ]
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ sizedRadioButton Col.xs6 "applyDistanceBasedScaleFactorRadioButton" ApplyDistanceBasedStopwatchScaleFactor "Apply distance-based scale factor to all times" editDetails
            ]
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ checkboxRowIndent
            , Grid.col [ Col.xs3 ] [ text "Expected distance" ]
            , inputTextField .expectedDistance ExpectedDistanceManualField ApplyDistanceBasedStopwatchScaleFactor isExpectedDistanceFieldInvalid editDetails
            , Grid.col [ Col.xs3 ] [ text "Actual distance" ]
            , inputTextField .actualDistance ActualDistanceField ApplyDistanceBasedStopwatchScaleFactor isActualDistanceFieldInvalid editDetails
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
                    [ Button.button
                        [ Button.primary
                        , Button.onClick (ApplyStopwatchOperation editDetails)
                        ]
                        [ text buttonText ]
                    ]

                Nothing ->
                    []
    in
    processButtons
        ++ [ Button.button
                [ Button.outlinePrimary
                , Button.attrs [ onClick CloseModal ]
                ]
                [ text "Close" ]
           ]
