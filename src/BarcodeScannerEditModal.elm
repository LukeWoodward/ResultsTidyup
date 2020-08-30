module BarcodeScannerEditModal exposing
    ( athleteInputId
    , athleteRadioButtonId
    , barcodeScannerDialogSizer
    , barcodeScannerDialogTitle
    , barcodeScannerEditButtons
    , editBarcodeScannerRowModalBody
    )

import BarcodeScanner exposing (LineContents(..))
import BarcodeScannerEditing
    exposing
        ( BarcodeScannerEditDetails(..)
        , BarcodeScannerFieldBeingEdited(..)
        , BarcodeScannerRowEditDetails
        , BarcodeScannerRowEditLocation
        , BarcodeScannerValidationError(..)
        , isValidAthlete
        , isValidFinishPosition
        )
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Html exposing (Html, b, div, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import ViewCommon exposing (outlineButton)


athleteInputId : String
athleteInputId =
    "barcodeScannerEditAthlete"


athleteRadioButtonId : String
athleteRadioButtonId =
    "athleteRadio"


finishPositionRadioButtonId : String
finishPositionRadioButtonId =
    "finishPositionRadio"


validationErrorToString : BarcodeScannerValidationError -> String
validationErrorToString validationError =
    case validationError of
        NeitherSelected ->
            "Please select either an athlete number or finish token"

        InvalidAthleteNumber ->
            "Please enter a valid athlete number"

        InvalidFinishPosition ->
            "Please enter a valid finish position"

        InvalidAthleteNumberAndFinishPosition ->
            "Please enter a valid athlete number and finish position"


editBarcodeScannerRowModalBody : BarcodeScannerRowEditDetails -> Html Msg
editBarcodeScannerRowModalBody rowEditDetails =
    let
        athleteRadio : Html Msg
        athleteRadio =
            case rowEditDetails.fieldBeingEdited of
                Both ->
                    text "Athlete"

                _ ->
                    Radio.radio
                        [ Radio.id athleteRadioButtonId
                        , Radio.checked (rowEditDetails.fieldBeingEdited == AthleteOnly)
                        , Radio.onClick (BarcodeScannerEdit (ChangeWhatsBeingEdited AthleteOnly))
                        ]
                        "Athlete"

        finishPositionRadio : Html Msg
        finishPositionRadio =
            case rowEditDetails.fieldBeingEdited of
                Both ->
                    text "Finish position"

                _ ->
                    Radio.radio
                        [ Radio.id finishPositionRadioButtonId
                        , Radio.checked (rowEditDetails.fieldBeingEdited == FinishPositionOnly)
                        , Radio.onClick (BarcodeScannerEdit (ChangeWhatsBeingEdited FinishPositionOnly))
                        ]
                        "Finish position"

        isAthleteEditable : Bool
        isAthleteEditable =
            rowEditDetails.fieldBeingEdited == AthleteOnly || rowEditDetails.fieldBeingEdited == Both

        athleteDanger : List (Input.Option Msg)
        athleteDanger =
            if isAthleteEditable && not (isValidAthlete rowEditDetails) then
                [ Input.danger ]

            else
                []

        isFinishPositionEditable : Bool
        isFinishPositionEditable =
            rowEditDetails.fieldBeingEdited == FinishPositionOnly || rowEditDetails.fieldBeingEdited == Both

        finishPositionDanger : List (Input.Option Msg)
        finishPositionDanger =
            if isFinishPositionEditable && not (isValidFinishPosition rowEditDetails) then
                [ Input.danger ]

            else
                []

        misScannedText : Maybe String
        misScannedText =
            case rowEditDetails.currentContents of
                Ordinary _ _ ->
                    Nothing

                MisScan text ->
                    Just text

        generateMisScannedTextRow : String -> Html Msg
        generateMisScannedTextRow misScanText =
            Grid.row [ Row.attrs [ class "form-group" ] ]
                [ Grid.col
                    [ Col.xs12 ]
                    [ text "Mis-scanned text: "
                    , b [] [ text misScanText ]
                    ]
                ]

        athleteRow : Html Msg
        athleteRow =
            Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
                [ Grid.col [ Col.xs4 ] [ athleteRadio ]
                , Grid.col [ Col.xs8 ]
                    [ Input.text
                        ([ Input.id athleteInputId
                         , Input.onInput (BarcodeScannerEdit << AthleteChanged)
                         , Input.value rowEditDetails.athleteEntered.enteredValue
                         , Input.disabled (not isAthleteEditable)
                         ]
                            ++ athleteDanger
                        )
                    ]
                ]

        finishPositionRow : Html Msg
        finishPositionRow =
            Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
                [ Grid.col [ Col.xs4 ] [ finishPositionRadio ]
                , Grid.col [ Col.xs8 ]
                    [ Input.text
                        ([ Input.onInput (BarcodeScannerEdit << FinishPositionChanged)
                         , Input.value rowEditDetails.finishPositionEntered.enteredValue
                         , Input.disabled (not isFinishPositionEditable)
                         ]
                            ++ finishPositionDanger
                        )
                    ]
                ]

        generateValidationRow : BarcodeScannerValidationError -> Html Msg
        generateValidationRow validationError =
            div [ class "validation-error" ] [ text (validationErrorToString validationError) ]
    in
    List.filterMap identity
        [ Maybe.map generateMisScannedTextRow misScannedText
        , Just athleteRow
        , Just finishPositionRow
        , Maybe.map generateValidationRow rowEditDetails.validationError
        ]
        |> Grid.containerFluid []


barcodeScannerDialogTitle : BarcodeScannerRowEditDetails -> String
barcodeScannerDialogTitle rowEditDetails =
    let
        prefix : String
        prefix =
            if rowEditDetails.isDeleted then
                "Reinstate "

            else
                "Edit "

        suffix : String
        suffix =
            case rowEditDetails.currentContents of
                MisScan _ ->
                    "mis-scanned barcode scanner row"

                Ordinary _ _ ->
                    "barcode scanner row"
    in
    prefix ++ suffix


barcodeScannerEditButtons : BarcodeScannerRowEditDetails -> List (Html Msg)
barcodeScannerEditButtons barcodeScannerRowEditDetails =
    let
        updateButtonMsg : Msg
        updateButtonMsg =
            if barcodeScannerRowEditDetails.validationError == Nothing then
                UpdateRowFromBarcodeScannerEditModal barcodeScannerRowEditDetails

            else
                NoOp

        deleteButtonAttrs : List (Html.Attribute Msg)
        deleteButtonAttrs =
            [ onClick (DeleteRowFromBarcodeScannerEditModal barcodeScannerRowEditDetails.location)
            , class "mr-5"
            ]

        deleteButton : Html Msg
        deleteButton =
            if barcodeScannerRowEditDetails.isDeleted then
                text ""

            else
                Button.button
                    [ Button.outlinePrimary
                    , Button.danger
                    , Button.attrs deleteButtonAttrs
                    ]
                    [ text "Delete row" ]

        updateButtonText : String
        updateButtonText =
            if barcodeScannerRowEditDetails.isDeleted then
                "Reinstate row"

            else
                "Update row"
    in
    [ deleteButton
    , outlineButton updateButtonMsg [] updateButtonText
    , outlineButton CloseModal [] "Close"
    ]


barcodeScannerDialogSizer : Modal.Config Msg -> Modal.Config Msg
barcodeScannerDialogSizer =
    identity
