module ConfirmClearEverythingModal exposing (confirmClearEverythingDialogSizer, confirmClearEverythingModalBody, confirmClearEverythingModalButtons, confirmClearEverythingModalTitle)

import Bootstrap.Modal as Modal
import Commands exposing (CurrentDateAndTimeOperation(..))
import Html exposing (Html, div, text)
import Msg exposing (Msg(..))
import PastedFile exposing (PastedFileInterpretation(..))
import ViewCommon exposing (dangerButton, outlineButton)


confirmClearEverythingModalTitle : String
confirmClearEverythingModalTitle =
    "Clear everything?"


confirmClearEverythingModalBody : Html Msg
confirmClearEverythingModalBody =
    div []
        [ text "Are you sure you wish to clear all data uploaded?" ]


confirmClearEverythingModalButtons : List (Html Msg)
confirmClearEverythingModalButtons =
    [ dangerButton ClearAllData [] "Clear all data"
    , outlineButton CloseModal [] "Cancel"
    ]


confirmClearEverythingDialogSizer : Modal.Config Msg -> Modal.Config Msg
confirmClearEverythingDialogSizer =
    identity
