module PastedFile exposing (PastedFileDetails, PastedFileInterpretation(..), empty, interpretPastedFile)

import BarcodeScanner exposing (readBarcodeScannerData)
import Dict
import FileHandling exposing (AddedFile)
import Timer exposing (Timer(..), readTimerData)


type PastedFileInterpretation
    = NoFilePasted
    | TimerFilePasted Int
    | BarcodeScannerFilePasted Int
    | UnrecognisedFilePasted


type alias PastedFileDetails =
    { pastedText : String
    , interpretation : PastedFileInterpretation
    }


empty : PastedFileDetails
empty =
    PastedFileDetails "" NoFilePasted


interpretPastedFile : String -> PastedFileInterpretation
interpretPastedFile contents =
    let
        trimmedContents : String
        trimmedContents =
            String.trim contents
    in
    if trimmedContents == "" then
        NoFilePasted

    else if String.startsWith "STARTOFEVENT" trimmedContents || String.startsWith "I, CP" trimmedContents then
        case readTimerData trimmedContents of
            Ok (TimerData times) ->
                TimerFilePasted (List.length times)

            Err _ ->
                UnrecognisedFilePasted

    else
        case readBarcodeScannerData (AddedFile "dummyFilename" "dummyName" trimmedContents) of
            Ok barcodeScannerData ->
                let
                    count : Int
                    count =
                        Dict.size barcodeScannerData.scannedBarcodes
                in
                if count > 0 then
                    BarcodeScannerFilePasted count

                else
                    UnrecognisedFilePasted

            Err _ ->
                UnrecognisedFilePasted
