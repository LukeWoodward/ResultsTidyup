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
import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import Commands
import Html exposing (Attribute, Html, div, h3, span, text)
import Html.Attributes exposing (class, colspan, title)
import Html.Events exposing (onDoubleClick)
import Model exposing (Model)
import Msg exposing (Msg(..))
import ProblemsView exposing (scannerProblemsView)
import ViewCommon exposing (athleteLink, normalButton, smallButton, tableHeader, tableHeaderWithClass)


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
        DuplicateScan athlete position ->
            "Athlete " ++ athlete ++ " has been scanned in position " ++ String.fromInt position ++ " elsewhere"

        AthleteScannedWithFinishTokenElsewhere athlete ->
            "Athlete " ++ athlete ++ " has been scanned with a finish token elsewhere"

        DeletedByUser ->
            "You deleted this line"


barcodeScannerContents : LineContents -> DeletionStatus -> List (Table.Cell Msg)
barcodeScannerContents contents deletionStatus =
    case contents of
        Ordinary athlete position ->
            let
                athleteContents : Html Msg
                athleteContents =
                    case deletionStatus of
                        NotDeleted ->
                            athleteLink athlete

                        Deleted _ ->
                            text athlete
            in
            [ Table.td [] [ athleteContents ]
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
        (Table.td [] [ text (String.fromInt line.lineNumber) ]
            :: barcodeScannerContents line.contents line.deletionStatus
            ++ [ Table.td [ Table.cellAttr (class "scanner-date-cell") ] [ text line.scanDateTime ] ]
        )


barcodeScannerView : BarcodeScannerFile -> Html Msg
barcodeScannerView file =
    let
        thead : Table.THead a
        thead =
            Table.simpleThead
                [ tableHeader "Line #"
                , tableHeader "Athlete"
                , tableHeader "Pos"
                , tableHeaderWithClass "Date/Time" "scanner-date-cell"
                ]
    in
    div []
        [ div
            [ class "barcode-scanner-buttons" ]
            [ smallButton (RequestCurrentDateAndTime (Commands.DownloadBarcodeScannerFile file.filename)) [] "Download"
            , smallButton (RemoveBarcodeScannerFile file.filename) [] "Remove"
            ]
        , Table.table
            { options = [ Table.bordered, Table.small, Table.hover, Table.attr (class "barcode-scanner-table") ]
            , thead = thead
            , tbody = Table.tbody [] (List.map (barcodeScannerViewRow file.filename) file.lines)
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
        , link = Tab.link [ class (getBarcodeScannerTabClass file) ] [ span [ title file.filename ] [ text file.name ] ]
        , pane = Tab.pane [] [ barcodeScannerView file ]
        }


barcodeScannersView : Model -> Html Msg
barcodeScannersView model =
    let
        showTokenOperationsButton : Html Msg
        showTokenOperationsButton =
            normalButton
                ShowTokenOperationsModal
                []
                "Token operations..."

        buttons : List (Html Msg)
        buttons =
            if List.length model.barcodeScannerData.files == 1 then
                [ showTokenOperationsButton ]

            else
                [ showTokenOperationsButton
                , normalButton
                    (RequestCurrentDateAndTime Commands.DownloadAllBarcodeScannerData)
                    [ title "Downloads a file containing all barcode data from all scanners" ]
                    "Download all scanned barcodes"
                ]
    in
    div []
        [ h3 [] [ text "Scanners", div [ class "barcode-scanner-buttons" ] buttons ]
        , div [ class "clearfix" ] []
        , scannerProblemsView model.problems
        , Tab.config ChangeBarcodeScannerTab
            |> Tab.items (List.indexedMap barcodeScannerTabView model.barcodeScannerData.files)
            |> Tab.view model.barcodeScannerTab
        ]
