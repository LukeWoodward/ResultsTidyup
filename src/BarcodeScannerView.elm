module BarcodeScannerView exposing (barcodeScannersView)

import BarcodeScanner exposing (BarcodeScannerFile, BarcodeScannerFileLine, DeletionReason(..), LineContents(..), ModificationStatus(..), WrongWayAroundStatus(..))
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Html exposing (Attribute, Html, button, div, h4, text)
import Html.Attributes exposing (class, colspan, rowspan, title)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import ViewCommon exposing (smallButton, tableHeaders)


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


barcodeScannerContents : LineContents -> List (Table.Cell Msg)
barcodeScannerContents contents =
    case contents of
        Ordinary athlete position ->
            [ Table.td [] [ text athlete ]
            , Table.td [] [ text (maybeIntToString position) ]
            ]

        MisScan misScannedText ->
            [ Table.td [ Table.cellAttr (colspan 2), Table.cellAttr (class "misscanned"), Table.cellAttr (title "This item was not scanned properly.") ] [ text misScannedText ] ]


modificationStatusCell : ModificationStatus -> Table.Cell Msg
modificationStatusCell status =
    case status of
        Unmodified ->
            Table.td [] [ text "Unmodified" ]

        Deleted reason ->
            Table.td [] [ text "Deleted" ]


wrongWayAroundStatusCell : WrongWayAroundStatus -> List (Table.Cell Msg)
wrongWayAroundStatusCell wrongWayAroundStatus =
    case wrongWayAroundStatus of
        NotWrongWayAround ->
            [ Table.td [] [] ]

        FirstWrongWayAround first last ->
            let
                rowCount : Int
                rowCount =
                    last + 1 - first
            in
            [ Table.td
                [ Table.cellAttr (rowspan rowCount)
                , Table.cellAttr (class "swap-barcodes-around-button-cell")
                ]
                [ Button.button [ Button.primary ] [ text "Swap barcodes around" ] ]
            ]

        SubsequentWrongWayAround ->
            []


barcodeScannerViewRow : BarcodeScannerFileLine -> Table.Row Msg
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
    Table.tr
        (List.map Table.rowAttr rowAttributes)
        ([ Table.td [] [ text (String.fromInt line.lineNumber) ] ]
            ++ barcodeScannerContents line.contents
            ++ [ Table.td [] [ text line.scanTime ]
               , modificationStatusCell line.modificationStatus
               ]
            ++ wrongWayAroundStatusCell line.wrongWayAroundStatus
        )


barcodeScannerView : Int -> BarcodeScannerFile -> Html Msg
barcodeScannerView index file =
    div []
        [ div
            [ class "barcode-scanner-buttons" ]
            [ smallButton (GetCurrentDateForDownloadFile (DownloadBarcodeScannerFile index)) [] "Download"
            , smallButton (DeleteBarcodeScannerFile index) [] "Delete"
            ]
        , h4 []
            [ text "File: "
            , text file.name
            ]
        , Table.table
            { options = [ Table.bordered, Table.small, Table.hover, Table.attr (class "barcode-scanner-table") ]
            , thead = tableHeaders [ "Line #", "Athlete", "Position", "Date/Time", "Status", "Action" ]
            , tbody = Table.tbody [] (List.map barcodeScannerViewRow file.lines)
            }
        ]


barcodeScannersView : List BarcodeScannerFile -> Html Msg
barcodeScannersView files =
    if List.isEmpty files then
        Alert.simpleInfo [ class "no-barcode-scanner-files" ] [ text "No barcode scanner files have been loaded" ]

    else
        div [] (List.indexedMap barcodeScannerView files)
