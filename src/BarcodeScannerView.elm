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
import Commands
import Html exposing (Attribute, Html, button, div, h3, li, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick, onDoubleClick)
import Icons exposing (download, remove)
import Model exposing (Model)
import Msg exposing (Msg(..))
import ProblemsView exposing (scannerProblemsView)
import ViewCommon exposing (athleteLink, dangerIconButton, normalButton, normalIconButton)


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


barcodeScannerContents : LineContents -> DeletionStatus -> List (Html Msg)
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
            [ td [] [ athleteContents ]
            , td [] [ text (maybeIntToString position) ]
            ]


barcodeScannerViewRow : String -> BarcodeScannerFileLine -> Html Msg
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
    tr
        rowAttributes
        (td [] [ text (String.fromInt line.lineNumber) ]
            :: barcodeScannerContents line.contents line.deletionStatus
            ++ [ td [ class "scanner-date-cell" ] [ text line.scanDateTime ] ]
        )


barcodeScannerView : Int -> BarcodeScannerFile -> Html Msg
barcodeScannerView index file =
    let
        header : Html a
        header =
            thead []
                [ tr []
                    [ th [] [ text "Line #" ]
                    , th [] [ text "Athlete" ]
                    , th [] [ text "Pos" ]
                    , th [ class "scanner-date-cell" ] [ text "Date/Time" ]
                    ]
                ]
    in
    div []
        [ div
            [ class "barcode-scanner-buttons" ]
            [ normalIconButton (RequestCurrentDateAndTime (Commands.DownloadBarcodeScannerFile file.filename)) download "Download"
            , dangerIconButton (RemoveBarcodeScannerFile index) remove "Remove"
            ]
        , table [ class "table table-bordered table-hover barcode-scanner-table" ]
            [ header
            , tbody [] (List.map (barcodeScannerViewRow file.filename) file.lines)
            ]
        ]


isNotDeleted : BarcodeScannerFileLine -> Bool
isNotDeleted line =
    line.deletionStatus == NotDeleted


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
    if List.any hasMissingItem file.lines then
        "barcode-scanner-file-warning"

    else
        "barcode-scanner-file-ok"


tabBar : Maybe Int -> List BarcodeScannerFile -> Html Msg
tabBar selectedTab files =
    let
        tabItem : Int -> BarcodeScannerFile -> Html Msg
        tabItem index file =
            li
                [ class "nav-item" ]
                [ button
                    [ class "nav-link btn btn-link"
                    , class (getBarcodeScannerTabClass file)
                    , classList [ ( "active", selectedTab == Just index ) ]
                    , onClick (ChangeBarcodeScannerTab index)
                    ]
                    [ span [ title file.filename ] [ text file.name ] ]
                ]
    in
    ul [ class "nav nav-tabs" ] (List.indexedMap tabItem files)


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

        selectedBarcodeScannerFile : Maybe BarcodeScannerFile
        selectedBarcodeScannerFile =
            case model.barcodeScannerTab of
                Just index ->
                    List.drop index model.barcodeScannerData.files
                        |> List.head

                Nothing ->
                    Nothing

        selectedBarcodeScannerView : Html Msg
        selectedBarcodeScannerView =
            Maybe.map2 barcodeScannerView model.barcodeScannerTab selectedBarcodeScannerFile
                |> Maybe.withDefault (text "")
    in
    div []
        [ h3 [] [ text "Scanners", div [ class "barcode-scanner-buttons" ] buttons ]
        , scannerProblemsView model.problems
        , tabBar model.barcodeScannerTab model.barcodeScannerData.files
        , selectedBarcodeScannerView
        ]
