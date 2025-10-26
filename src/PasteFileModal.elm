module PasteFileModal exposing
    ( pasteFileButtons
    , pasteFileDialogSize
    , pasteFileDialogTextAreaId
    , pasteFileDialogTitle
    , pasteFileModalBody
    )

import Commands exposing (CurrentDateAndTimeOperation(..))
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (disabled, id, placeholder)
import Html.Events exposing (onInput)
import Msg exposing (Msg(..))
import PastedFile exposing (PastedFileDetails, PastedFileInterpretation(..))
import ViewCommon exposing (ModalSize(..), normalButton, outlineButton)


pasteFileDialogTextAreaId : String
pasteFileDialogTextAreaId =
    "pasteFileDialogTextArea"


pasteFileDialogTitle : String
pasteFileDialogTitle =
    "Paste file"


pluralSuffix : Int -> String
pluralSuffix count =
    if count == 1 then
        ""

    else
        "s"


formatInterpretation : PastedFileInterpretation -> String
formatInterpretation interpretation =
    case interpretation of
        NoFilePasted ->
            "No file pasted"

        TimerFilePasted timeCount ->
            "Timer file with " ++ String.fromInt timeCount ++ " time" ++ pluralSuffix timeCount

        BarcodeScannerFilePasted scannedBarcodeCount ->
            "Barcode scanner file with " ++ String.fromInt scannedBarcodeCount ++ " scanned barcode" ++ pluralSuffix scannedBarcodeCount

        UnrecognisedFilePasted ->
            "File not recognised"


pasteFileModalBody : PastedFileDetails -> Html Msg
pasteFileModalBody pastedFileDetails =
    div []
        [ textarea
            [ id pasteFileDialogTextAreaId
            , placeholder "Paste contents of timer or scanner file here..."
            , onInput PastedFileChanged
            ]
            [ text pastedFileDetails.pastedText ]
        , div [] [ text (formatInterpretation pastedFileDetails.interpretation) ]
        ]


pasteFileButtons : PastedFileDetails -> List (Html Msg)
pasteFileButtons pastedFileDetails =
    let
        uploadButtonAttributes : List (Html.Attribute Msg)
        uploadButtonAttributes =
            case pastedFileDetails.interpretation of
                TimerFilePasted _ ->
                    []

                BarcodeScannerFilePasted _ ->
                    []

                NoFilePasted ->
                    [ disabled True ]

                UnrecognisedFilePasted ->
                    [ disabled True ]
    in
    [ normalButton (RequestCurrentDateAndTime (UploadPastedFile pastedFileDetails.pastedText)) uploadButtonAttributes "Upload"
    , outlineButton CloseModal [] "Close"
    ]


pasteFileDialogSize : ModalSize
pasteFileDialogSize =
    Standard
