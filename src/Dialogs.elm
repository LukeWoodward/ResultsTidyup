module Dialogs exposing (showModalDialog)

import BarcodeScannerEditModal exposing (barcodeScannerDialogTitle, barcodeScannerEditButtons, editBarcodeScannerRowModalBody)
import Bootstrap.Modal as Modal
import Html exposing (Html, div, text)
import Model exposing (DialogDetails(..), Model)
import Msg exposing (Msg(..))
import TokenOperationsModal exposing (tokenOperationsButtons, tokenOperationsDialogTitle, tokenOperationsModalBody)


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

        TokenOperationsDialog tokenOperationEditDetails ->
            tokenOperationsDialogTitle

        NoDialog ->
            ""


dialogBody : DialogDetails -> Html Msg
dialogBody dialogDetails =
    case dialogDetails of
        BarcodeScannerRowEditDialog rowEditDetails ->
            editBarcodeScannerRowModalBody rowEditDetails

        TokenOperationsDialog tokenOperationEditDetails ->
            tokenOperationsModalBody tokenOperationEditDetails

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

                NoDialog ->
                    []

        sizer : Modal.Config Msg -> Modal.Config Msg
        sizer =
            case model.dialogDetails of
                TokenOperationsDialog _ ->
                    Modal.large

                _ ->
                    identity
    in
    Modal.config CloseModal
        |> Modal.h5 [] [ text (dialogTitle model.dialogDetails) ]
        |> Modal.body [] [ dialogBody model.dialogDetails ]
        |> Modal.footer [] buttons
        |> sizer
        |> Modal.view (dialogVisibility model.dialogDetails)
