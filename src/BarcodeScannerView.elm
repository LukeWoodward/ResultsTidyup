module BarcodeScannerView exposing (barcodeScannersView)

import BarcodeScanner
    exposing
        ( BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        )
import BarcodeScannerEditing exposing (BarcodeScannerRowEditLocation)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import Commands
import Html exposing (Attribute, Html, button, div, h4, small, span, text)
import Html.Attributes exposing (class, colspan, rowspan, title)
import Html.Events exposing (onClick, onDoubleClick)
import Model exposing (Model)
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

        EndOfWrongWayAroundSection ->
            "This line was at the end of a section of barcodes scanned the wrong way around"

        DeletedByUser ->
            "You deleted this line"


barcodeScannerContents : LineContents -> List (Table.Cell Msg)
barcodeScannerContents contents =
    case contents of
        Ordinary athlete position ->
            [ Table.td [] [ text athlete ]
            , Table.td [] [ text (maybeIntToString position) ]
            ]

        MisScan misScannedText ->
            [ Table.td [ Table.cellAttr (colspan 2), Table.cellAttr (class "misscanned"), Table.cellAttr (title "This item was not scanned properly.") ] [ text misScannedText ] ]


barcodeScannerViewRow : String -> BarcodeScannerFileLine -> Table.Row Msg
barcodeScannerViewRow fileName line =
    let
        rowAttributes : List (Attribute Msg)
        rowAttributes =
            case line.deletionStatus of
                NotDeleted ->
                    let
                        className : String
                        className =
                            case line.contents of
                                MisScan _ ->
                                    "barcode-scanner-row-error"

                                Ordinary _ Nothing ->
                                    "barcode-scanner-row-warning"

                                Ordinary "" _ ->
                                    "barcode-scanner-row-warning"

                                Ordinary _ _ ->
                                    "barcode-scanner-row-ok"
                    in
                    [ class className
                    , onDoubleClick (ShowBarcodeScannerEditModal (BarcodeScannerRowEditLocation fileName line.lineNumber) line.contents False)
                    ]

                Deleted deletionReason ->
                    [ class "deleted-barcode-scanner-row"
                    , title (deletionReasonToString deletionReason)
                    , onDoubleClick (ShowBarcodeScannerEditModal (BarcodeScannerRowEditLocation fileName line.lineNumber) line.contents True)
                    ]
    in
    Table.tr
        (List.map Table.rowAttr rowAttributes)
        ([ Table.td [] [ text (String.fromInt line.lineNumber) ] ]
            ++ barcodeScannerContents line.contents
            ++ [ Table.td [] [ text line.scanDateTime ] ]
        )


barcodeScannerView : BarcodeScannerFile -> Html Msg
barcodeScannerView file =
    div []
        [ div
            [ class "barcode-scanner-buttons" ]
            [ smallButton (GetCurrentDateForDownloadFile (Commands.DownloadBarcodeScannerFile file.name)) [] "Download"
            , smallButton (DeleteBarcodeScannerFile file.name) [] "Delete"
            ]
        , Table.table
            { options = [ Table.bordered, Table.small, Table.hover, Table.attr (class "barcode-scanner-table") ]
            , thead = tableHeaders [ "Line #", "Athlete", "Position", "Date/Time" ]
            , tbody = Table.tbody [] (List.map (barcodeScannerViewRow file.name) file.lines)
            }
        ]


isNotDeleted : BarcodeScannerFileLine -> Bool
isNotDeleted line =
    line.deletionStatus == NotDeleted


isMisScan : BarcodeScannerFileLine -> Bool
isMisScan line =
    case line.contents of
        Ordinary _ _ ->
            False

        MisScan _ ->
            isNotDeleted line


hasMissingItem : BarcodeScannerFileLine -> Bool
hasMissingItem line =
    case line.contents of
        Ordinary "" _ ->
            isNotDeleted line

        Ordinary _ Nothing ->
            isNotDeleted line

        _ ->
            False


getBarcodeScannerTabClass : BarcodeScannerFile -> String
getBarcodeScannerTabClass file =
    if List.any isMisScan file.lines then
        "barcode-scanner-file-error"

    else if List.any hasMissingItem file.lines then
        "barcode-scanner-file-warning"

    else
        "barcode-scanner-file-ok"


barcodeScannerTabView : Int -> BarcodeScannerFile -> Tab.Item Msg
barcodeScannerTabView index file =
    Tab.item
        { id = "barcodeScannerTab" ++ String.fromInt index
        , link = Tab.link [ class (getBarcodeScannerTabClass file) ] [ small [] [ text file.name ] ]
        , pane = Tab.pane [] [ barcodeScannerView file ]
        }


barcodeScannersView : Model -> Html Msg
barcodeScannersView model =
    if List.isEmpty model.barcodeScannerData.files then
        Alert.simpleInfo [ class "no-barcode-scanner-files" ] [ text "No barcode scanner files have been loaded" ]

    else
        let
            additionalHeaderContent : List (Html Msg)
            additionalHeaderContent =
                if List.length model.barcodeScannerData.files == 1 then
                    []

                else
                    [ div
                        [ class "barcode-scanner-buttons" ]
                        [ smallButton
                            (GetCurrentDateForDownloadFile Commands.DownloadAllBarcodeScannerData)
                            [ title "Downloads a file containing all barcode data from all scanners" ]
                            "Download all scanned barcodes"
                        ]
                    ]
        in
        div []
            [ h4 []
                (text "Barcode scanner files" :: additionalHeaderContent)
            , Tab.config ChangeBarcodeScannerTab
                |> Tab.items (List.indexedMap barcodeScannerTabView model.barcodeScannerData.files)
                |> Tab.view model.barcodeScannerTab
            ]
