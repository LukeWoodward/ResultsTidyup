module Modals exposing (modalBackdrop, showModalDialog)

import BarcodeScannerEditModal exposing (barcodeScannerDialogSize, barcodeScannerDialogTitle, barcodeScannerEditButtons, editBarcodeScannerRowModalBody)
import ConfirmClearEverythingModal exposing (confirmClearEverythingDialogSize, confirmClearEverythingModalBody, confirmClearEverythingModalButtons, confirmClearEverythingModalTitle)
import Html exposing (Html, div, h5, text)
import Html.Attributes exposing (class, classList, style, tabindex)
import Html.Events exposing (onClick)
import Model exposing (DialogDetails(..), Model)
import Msg exposing (Msg(..))
import PasteFileModal exposing (pasteFileButtons, pasteFileDialogSize, pasteFileDialogTitle, pasteFileModalBody)
import TimerOperationsModal exposing (timerOperationsButtons, timerOperationsDialogSize, timerOperationsDialogTitle, timerOperationsModalBody)
import TokenOperationsModal exposing (tokenOperationsButtons, tokenOperationsDialogSize, tokenOperationsDialogTitle, tokenOperationsModalBody)
import ViewCommon exposing (ModalSize(..), role)


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

        size : ModalSize
        size =
            case model.dialogDetails of
                BarcodeScannerRowEditDialog _ ->
                    barcodeScannerDialogSize

                TokenOperationsDialog _ ->
                    tokenOperationsDialogSize

                TimerOperationsDialog _ ->
                    timerOperationsDialogSize

                PasteFileDialog _ ->
                    pasteFileDialogSize

                ConfirmClearEverythingDialog ->
                    confirmClearEverythingDialogSize

                NoDialog ->
                    Standard

        visible : Bool
        visible =
            model.dialogDetails /= NoDialog

        extraStyle : Html.Attribute Msg
        extraStyle =
            if visible then
                style "pointer-events" "none"

            else
                style "height" "0px"

        modalClassAttribute : Html.Attribute Msg
        modalClassAttribute =
            classList [ ( "modal-lg", size == Large ) ]
    in
    div
        [ tabindex -1
        , style "display" "block"
        , extraStyle
        , class "modal"
        , role "dialog"
        , classList [ ( "show", visible ) ]
        ]
        [ div
            [ class "modal-dialog"
            , style "pointer-events" "auto"
            , modalClassAttribute
            ]
            [ div
                [ class "modal-content" ]
                [ div [ class "modal-header" ] [ h5 [] [ text (dialogTitle model.dialogDetails) ] ]
                , div [ class "modal-body" ] [ dialogBody model.dialogDetails ]
                , div [ class "modal-footer" ] buttons
                ]
            ]
        ]


modalBackdrop : DialogDetails -> Html Msg
modalBackdrop dialogDetails =
    let
        visible : Bool
        visible =
            dialogDetails /= NoDialog
    in
    div
        [ classList [ ( "modal-backdrop", visible ), ( "show", visible ) ]
        , onClick CloseModal
        ]
        []
