module BarcodeScannerView exposing (barcodeScannerView)

import BarcodeScanner exposing (BarcodeScannerFile, BarcodeScannerFileLine, LineContents(..), ModificationStatus(..))
import Html exposing (Attribute, Html, div, h4, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, title)
import Msg exposing (Msg(..))
import ViewCommon exposing (tableHeaders)


maybeIntToString : Maybe Int -> String
maybeIntToString maybeInt =
    case maybeInt of
        Just someInt ->
            String.fromInt someInt

        Nothing ->
            ""


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
                    , title deletionReason
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
