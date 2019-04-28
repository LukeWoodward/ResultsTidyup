module BarcodeScannerEditModal exposing (barcodeScannerEditModal)

import BarcodeScanner exposing (LineContents(..))
import BarcodeScannerEditing
    exposing
        ( BarcodeScannerEditDetails(..)
        , BarcodeScannerFieldBeingEdited(..)
        , BarcodeScannerRowEditDetails
        , BarcodeScannerRowEditLocation
        , BarcodeScannerValidationError(..)
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
import Model exposing (DialogDetails(..), Model)
import Msg exposing (Msg(..))
import NumericEntry exposing (isValidEntry)


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
                        [ Radio.id "athleteRadio"
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
                        [ Radio.id "finishPositionRadio"
                        , Radio.checked (rowEditDetails.fieldBeingEdited == FinishPositionOnly)
                        , Radio.onClick (BarcodeScannerEdit (ChangeWhatsBeingEdited FinishPositionOnly))
                        ]
                        "Finish position"

        isAthleteEditable : Bool
        isAthleteEditable =
            rowEditDetails.fieldBeingEdited == AthleteOnly || rowEditDetails.fieldBeingEdited == Both

        athleteDanger : List (Input.Option Msg)
        athleteDanger =
            if isAthleteEditable && not (isValidEntry rowEditDetails.athleteEntered) then
                [ Input.danger ]

            else
                []

        isFinishPositionEditable : Bool
        isFinishPositionEditable =
            rowEditDetails.fieldBeingEdited == FinishPositionOnly || rowEditDetails.fieldBeingEdited == Both

        finishPositionDanger : List (Input.Option Msg)
        finishPositionDanger =
            if isFinishPositionEditable && not (isValidEntry rowEditDetails.finishPositionEntered) then
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
                        ([ Input.onInput (BarcodeScannerEdit << AthleteChanged)
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


dialogVisibility : DialogDetails -> Modal.Visibility
dialogVisibility dialogDetails =
    if dialogDetails == NoDialog then
        Modal.hidden

    else
        Modal.shown


dialogTitle : DialogDetails -> String
dialogTitle dialogDetails =
    case dialogDetails of
        BarcodeScannerRowEditDialog rowEditDetails ->
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

        NoDialog ->
            ""


dialogBody : DialogDetails -> Html Msg
dialogBody dialogDetails =
    case dialogDetails of
        NoDialog ->
            div [] []

        BarcodeScannerRowEditDialog rowEditDetails ->
            editBarcodeScannerRowModalBody rowEditDetails


barcodeScannerEditButtons : BarcodeScannerRowEditDetails -> List (Html Msg)
barcodeScannerEditButtons barcodeScannerRowEditDetails =
    let
        updateButtonAttrs : List (Html.Attribute Msg)
        updateButtonAttrs =
            if barcodeScannerRowEditDetails.validationError == Nothing then
                let
                    athleteStringValue : String
                    athleteStringValue =
                        case barcodeScannerRowEditDetails.athleteEntered.parsedValue of
                            Just athleteNumber ->
                                "A" ++ String.fromInt athleteNumber

                            Nothing ->
                                ""
                in
                [ onClick
                    (UpdateRowFromBarcodeScannerEditModal
                        barcodeScannerRowEditDetails.location
                        athleteStringValue
                        barcodeScannerRowEditDetails.finishPositionEntered.parsedValue
                    )
                ]

            else
                []

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

        updateButtonDisabled : Bool
        updateButtonDisabled =
            barcodeScannerRowEditDetails.validationError /= Nothing
    in
    [ deleteButton
    , Button.button
        [ Button.outlinePrimary
        , Button.disabled updateButtonDisabled
        , Button.attrs updateButtonAttrs
        ]
        [ text updateButtonText ]
    , Button.button
        [ Button.outlinePrimary
        , Button.attrs [ onClick CloseModal ]
        ]
        [ text "Close" ]
    ]


barcodeScannerEditModal : Model -> Html Msg
barcodeScannerEditModal model =
    let
        buttons : List (Html Msg)
        buttons =
            case model.dialogDetails of
                NoDialog ->
                    []

                BarcodeScannerRowEditDialog barcodeScannerRowEditDetails ->
                    barcodeScannerEditButtons barcodeScannerRowEditDetails
    in
    Modal.config CloseModal
        |> Modal.h5 [] [ text (dialogTitle model.dialogDetails) ]
        |> Modal.body [] [ dialogBody model.dialogDetails ]
        |> Modal.footer [] buttons
        |> Modal.view (dialogVisibility model.dialogDetails)
