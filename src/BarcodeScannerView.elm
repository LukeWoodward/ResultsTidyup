module BarcodeScannerView exposing (barcodeScannerView)

import BarcodeScanner exposing (BarcodeScannerFile, BarcodeScannerFileLine, LineContents(..), ModificationStatus(..))
import Html exposing (Html, div, h4, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, title)
import Msg exposing (Msg(..))
import ViewCommon exposing (tableHeaders)


barcodeScannerContents : LineContents -> List (Html Msg)
barcodeScannerContents contents =
    case contents of
        Ordinary athlete position ->
            [ td [] [ text athlete ]
            , td [] [ text position ]
            ]

        MisScan misScannedText ->
            [ td [ colspan 2, class "misscanned", title "This item was not scanned properly." ] [ text misScannedText ] ]


barcodeScannerViewRow : BarcodeScannerFileLine -> Html Msg
barcodeScannerViewRow line =
    tr []
        ([ td [] [ text (String.fromInt line.lineNumber) ] ]
            ++ barcodeScannerContents line.contents
            ++ [ td [] [ text line.date ]
               , td [] [ text "Unmodified" ]
               ]
        )


barcodeScannerView : BarcodeScannerFile -> Html Msg
barcodeScannerView file =
    div []
        [ h4 []
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
