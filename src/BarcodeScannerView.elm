module BarcodeScannerView exposing (barcodeScannersView)

import BarcodeScanner exposing (BarcodeScannerFile, BarcodeScannerFileLine, DeletionReason(..), LineContents(..), ModificationStatus(..))
import Html exposing (Attribute, Html, button, div, h4, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, title)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import ViewCommon exposing (tableHeaders)


maybeIntToString : Maybe Int -> String
maybeIntToString maybeInt =
    case maybeInt of
        Just someInt ->
            String.fromInt someInt

        Nothing ->
            ""


deletionReasonToString : DeletionReason -> String
deletionReasonToString reason =
    case reason of
        BeforeEventStart ->
            "Before event start"

        DuplicateScan athlete position ->
            "Athlete " ++ athlete ++ " has been scanned in position " ++ String.fromInt position ++ " elsewhere"

        AthleteScannedWithFinishTokenElsewhere athlete ->
            "Athlete " ++ athlete ++ " has been scanned with a finish token elsewhere"

        FinishTokenScannedWithAthleteElsewhere position ->
            "Position " ++ String.fromInt position ++ " has been scanned with an athlete barcode elsewhere"


barcodeScannerContents : LineContents -> List (Html Msg)
barcodeScannerContents contents =
    case contents of
        Ordinary athlete position ->
            [ td [] [ text athlete ]
            , td [] [ text (maybeIntToString position) ]
            ]

        MisScan misScannedText ->
            [ td [ colspan 2, class "misscanned", title "This item was not scanned properly." ] [ text misScannedText ] ]


modificationStatusCell : ModificationStatus -> Html Msg
modificationStatusCell status =
    case status of
        Unmodified ->
            td [] [ text "Unmodified" ]

        Deleted reason ->
            td [] [ text "Deleted" ]


barcodeScannerViewRow : BarcodeScannerFileLine -> Html Msg
barcodeScannerViewRow line =
    let
        rowAttributes : List (Attribute Msg)
        rowAttributes =
            case line.modificationStatus of
                Unmodified ->
                    []

                Deleted deletionReason ->
                    [ class "deleted-barcode-scanner-row"
                    , title (deletionReasonToString deletionReason)
                    ]
    in
    tr
        rowAttributes
        ([ td [] [ text (String.fromInt line.lineNumber) ] ]
            ++ barcodeScannerContents line.contents
            ++ [ td [] [ text line.scanTime ]
               , modificationStatusCell line.modificationStatus
               ]
        )


barcodeScannerView : Int -> BarcodeScannerFile -> Html Msg
barcodeScannerView index file =
    div []
        [ div
            [ class "barcode-scanner-buttons" ]
            [ button [ class "btn btn-primary btn-xs", onClick (GetCurrentDateForDownloadFile (DownloadBarcodeScannerFile index)) ] [ text "Download" ]
            , button [ class "btn btn-primary btn-xs", onClick (DeleteBarcodeScannerFile index) ] [ text "Delete" ]
            ]
        , h4 []
            [ text "File: "
            , text file.name
            ]
        , table
            [ class "table table-bordered table-condensed table-hover barcode-scanner-data" ]
            [ tableHeaders [ "Line #", "Athlete", "Position", "Date/Time", "Status" ]
            , tbody
                []
                (List.map barcodeScannerViewRow file.lines)
            ]
        ]


barcodeScannersView : List BarcodeScannerFile -> Html Msg
barcodeScannersView files =
    if List.isEmpty files then
        div [ class "alert alert-info no-barcode-scanner-files" ] [ text "No barcode scanner files have been loaded" ]

    else
        div [] (List.indexedMap barcodeScannerView files)
