module Modals exposing (showModalDialog)

import BarcodeScannerEditModal exposing (barcodeScannerDialogSizer, barcodeScannerDialogTitle, barcodeScannerEditButtons, editBarcodeScannerRowModalBody)
import Bootstrap.Modal as Modal
import ConfirmClearEverythingModal exposing (confirmClearEverythingDialogSizer, confirmClearEverythingModalBody, confirmClearEverythingModalButtons, confirmClearEverythingModalTitle)
import Html exposing (Html, div, text)
import Model exposing (DialogDetails(..), Model)
import Msg exposing (Msg(..))
import PasteFileModal exposing (pasteFileButtons, pasteFileDialogSizer, pasteFileDialogTitle, pasteFileModalBody)
import TimerOperationsModal exposing (timerOperationsButtons, timerOperationsDialogSizer, timerOperationsDialogTitle, timerOperationsModalBody)
import TokenOperationsModal exposing (tokenOperationsButtons, tokenOperationsDialogSizer, tokenOperationsDialogTitle, tokenOperationsModalBody)


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
            barcodeScannerDialogTitle rowEditDetails

        TokenOperationsDialog _ ->
            tokenOperationsDialogTitle

        TimerOperationsDialog _ ->
            timerOperationsDialogTitle

        PasteFileDialog _ ->
            pasteFileDialogTitle

        ConfirmClearEverythingDialog ->
            confirmClearEverythingModalTitle

        NoDialog ->
            ""


dialogBody : DialogDetails -> Html Msg
dialogBody dialogDetails =
    case dialogDetails of
        BarcodeScannerRowEditDialog rowEditDetails ->
            editBarcodeScannerRowModalBody rowEditDetails

        TokenOperationsDialog tokenOperationEditDetails ->
            tokenOperationsModalBody tokenOperationEditDetails

        TimerOperationsDialog timerOperationEditDetails ->
            timerOperationsModalBody timerOperationEditDetails

        PasteFileDialog pastedFileDetails ->
            pasteFileModalBody pastedFileDetails

        ConfirmClearEverythingDialog ->
            confirmClearEverythingModalBody

        NoDialog ->
            div [] []


showModalDialog : Model -> Html Msg
showModalDialog model =
    let
        buttons : List (Html Msg)
        buttons =
            case model.dialogDetails of
                BarcodeScannerRowEditDialog barcodeScannerRowEditDetails ->
                    barcodeScannerEditButtons barcodeScannerRowEditDetails

                TokenOperationsDialog tokenOperationEditDetails ->
                    tokenOperationsButtons tokenOperationEditDetails

                TimerOperationsDialog timerOperationEditDetails ->
                    timerOperationsButtons timerOperationEditDetails

                PasteFileDialog pastedFileDetails ->
                    pasteFileButtons pastedFileDetails

                ConfirmClearEverythingDialog ->
                    confirmClearEverythingModalButtons

                NoDialog ->
                    []

        sizer : Modal.Config Msg -> Modal.Config Msg
        sizer =
            case model.dialogDetails of
                BarcodeScannerRowEditDialog _ ->
                    barcodeScannerDialogSizer

                TokenOperationsDialog _ ->
                    tokenOperationsDialogSizer

                TimerOperationsDialog _ ->
                    timerOperationsDialogSizer

                PasteFileDialog _ ->
                    pasteFileDialogSizer

                ConfirmClearEverythingDialog ->
                    confirmClearEverythingDialogSizer

                NoDialog ->
                    identity
    in
    Modal.config CloseModal
        |> Modal.h5 [] [ text (dialogTitle model.dialogDetails) ]
        |> Modal.body [] [ dialogBody model.dialogDetails ]
        |> Modal.footer [] buttons
        |> sizer
        |> Modal.view (dialogVisibility model.dialogDetails)
