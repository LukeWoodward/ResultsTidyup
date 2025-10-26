module BarcodeScannerEditModal exposing
    ( athleteInputId
    , barcodeScannerDialogSize
    , barcodeScannerDialogTitle
    , barcodeScannerEditButtons
    , editBarcodeScannerRowModalBody
    )

import BarcodeScanner exposing (LineContents(..))
import BarcodeScannerEditing
    exposing
        ( BarcodeScannerEditDetails(..)
        , BarcodeScannerRowEditDetails
        , BarcodeScannerValidationError(..)
        , isValidAthlete
        , isValidFinishPosition
        )
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Msg exposing (Msg(..))
import ViewCommon exposing (ModalSize(..), outlineButton)


athleteInputId : String
athleteInputId =
    "barcodeScannerEditAthlete"


validationErrorToString : BarcodeScannerValidationError -> String
validationErrorToString validationError =
    case validationError of
        InvalidAthleteNumber ->
            "Please enter a valid athlete number"

        InvalidFinishPosition ->
            "Please enter a valid finish position"

        InvalidAthleteNumberAndFinishPosition ->
            "Please enter a valid athlete number and finish position"


editBarcodeScannerRowModalBody : BarcodeScannerRowEditDetails -> Html Msg
editBarcodeScannerRowModalBody rowEditDetails =
    let
        athleteDanger : List (Html.Attribute Msg)
        athleteDanger =
            if isValidAthlete rowEditDetails then
                []

            else
                [ class "is-invalid" ]

        finishPositionDanger : List (Html.Attribute Msg)
        finishPositionDanger =
            if isValidFinishPosition rowEditDetails then
                []

            else
                [ class "is-invalid" ]

        athleteRow : Html Msg
        athleteRow =
            div [ class "row mb-3" ]
                [ div [ class "col-4 col-form-label" ] [ text "Athlete" ]
                , div [ class "col-8" ]
                    [ input
                        (type_ "text"
                            :: class "form-control"
                            :: id athleteInputId
                            :: onInput (BarcodeScannerEdit << AthleteChanged)
                            :: value rowEditDetails.athleteEntered.enteredValue
                            :: athleteDanger
                        )
                        []
                    ]
                ]

        finishPositionRow : Html Msg
        finishPositionRow =
            div [ class "row mb-3" ]
                [ div [ class "col-4 col-form-label" ] [ text "Finish position" ]
                , div [ class "col-8" ]
                    [ input
                        (type_ "text"
                            :: class "form-control"
                            :: onInput (BarcodeScannerEdit << FinishPositionChanged)
                            :: value rowEditDetails.finishPositionEntered.enteredValue
                            :: finishPositionDanger
                        )
                        []
                    ]
                ]

        generateValidationRow : BarcodeScannerValidationError -> Html Msg
        generateValidationRow validationError =
            div [ class "validation-error" ] [ text (validationErrorToString validationError) ]
    in
    List.filterMap identity
        [ Just athleteRow
        , Just finishPositionRow
        , Maybe.map generateValidationRow rowEditDetails.validationError
        ]
        |> div [ class "container-fluid" ]


barcodeScannerDialogTitle : BarcodeScannerRowEditDetails -> String
barcodeScannerDialogTitle rowEditDetails =
    let
        prefix : String
        prefix =
            if rowEditDetails.isDeleted then
                "Reinstate"

            else
                "Edit"
    in
    prefix ++ " barcode scanner row"


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
                button
                    (type_ "button" :: class "btn btn-danger" :: deleteButtonAttrs)
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


barcodeScannerDialogSize : ModalSize
barcodeScannerDialogSize =
    Standard
