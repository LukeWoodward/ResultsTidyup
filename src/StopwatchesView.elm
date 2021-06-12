module StopwatchesView exposing (stopwatchesView)

import BarcodeScanner exposing (AthleteAndTimePair, BarcodeScannerData, maxFinishToken)
import Bootstrap.Button as Button
import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import Commands
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, h3, input, label, small, text)
import Html.Attributes exposing (checked, class, for, id, title, type_)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import Problems exposing (Problems)
import ProblemsView exposing (stopwatchProblemsView)
import Stopwatch exposing (MergeEntry(..), MergedTableRow, StopwatchMatchSummary, Stopwatches(..), WhichStopwatch(..))
import TimeHandling exposing (formatTime)
import ViewCommon exposing (athleteLink, intCell, normalButton, plainCell, smallButton)


tableOptions : List (Table.TableOption a)
tableOptions =
    [ Table.small, Table.bordered, Table.attr (class "stopwatch-times") ]


type alias TableHeaderButton =
    { change : Msg
    , buttonText : String
    }


type alias TableHeaderWithButtons =
    { headerText : String
    , headerTooltip : String
    , buttonData : List TableHeaderButton
    }


athleteItem : AthleteAndTimePair -> Html a
athleteItem athleteAndTimePair =
    div [] [ athleteLink athleteAndTimePair.athlete ]


barcodeScannerCell : BarcodeScannerData -> Int -> Maybe Int -> Maybe Int -> Table.Cell a
barcodeScannerCell barcodeScannerData position numberCheckerId highlightedNumberCheckerId =
    case Dict.get position barcodeScannerData.scannedBarcodes of
        Just athleteAndTimePairs ->
            Table.td
                (numberCheckerUnderlineAttributes (Just "scanned-athlete") numberCheckerId highlightedNumberCheckerId)
                (List.map athleteItem athleteAndTimePairs)

        Nothing ->
            emptyBarcodeScannerCell numberCheckerId highlightedNumberCheckerId


cell : String -> Maybe Int -> Maybe Int -> Table.Cell a
cell contents numberCheckerId highlightedNumberCheckerId =
    Table.td (numberCheckerUnderlineAttributes Nothing numberCheckerId highlightedNumberCheckerId) [ text contents ]


timeCell : String -> Int -> Maybe Int -> Maybe Int -> Table.Cell a
timeCell className time numberCheckerId highlightedNumberCheckerId =
    Table.td (numberCheckerUnderlineAttributes (Just className) numberCheckerId highlightedNumberCheckerId) [ text (formatTime time) ]


emptyBarcodeScannerCell : Maybe Int -> Maybe Int -> Table.Cell a
emptyBarcodeScannerCell maybeNumberCheckerId highlightedNumberCheckerId =
    Table.td (numberCheckerUnderlineAttributes (Just "no-scanned-athlete") maybeNumberCheckerId highlightedNumberCheckerId) [ text "−" ]


emptyNumberCell : Table.Cell a
emptyNumberCell =
    Table.td [ Table.cellAttr (class "empty-cell") ] [ text "–" ]


checkboxCell : Int -> Int -> Bool -> Maybe Int -> Maybe Int -> Table.Cell Msg
checkboxCell time index included numberCheckerId highlightedNumberCheckerId =
    let
        idText : String
        idText =
            "toggle_checkbox_" ++ String.fromInt index

        labelClassName : String
        labelClassName =
            if included then
                "stopwatch-time-label"

            else
                "stopwatch-time-label excluded"
    in
    Table.td
        (numberCheckerUnderlineAttributes (Just "mismatch") numberCheckerId highlightedNumberCheckerId)
        [ input
            [ type_ "checkbox"
            , checked included
            , id idText
            , onClick (ToggleTableRow index)
            ]
            []
        , label
            [ for idText
            , class labelClassName
            ]
            [ text (formatTime time) ]
        ]


tableHeaderWithButtons : TableHeaderWithButtons -> Table.Cell Msg
tableHeaderWithButtons { headerText, headerTooltip, buttonData } =
    let
        textElement : Html Msg
        textElement =
            text headerText

        elements : List (Html Msg)
        elements =
            (List.map (\{ change, buttonText } -> smallButton change [] buttonText) buttonData ++ [ textElement ])
                |> List.intersperse (br [] [])
    in
    Table.th
        [ Table.cellAttr (class "stopwatch-header")
        , Table.cellAttr (title headerTooltip)
        ]
        elements


tableHeadersWithButtons : List TableHeaderWithButtons -> Table.THead Msg
tableHeadersWithButtons headerTexts =
    Table.simpleThead (List.map tableHeaderWithButtons headerTexts)


downloadStopwatchButton : WhichStopwatch -> TableHeaderButton
downloadStopwatchButton which =
    { change = RequestCurrentDateAndTime (Commands.DownloadSingleStopwatch which)
    , buttonText = "Download"
    }


removeStopwatchButton : WhichStopwatch -> TableHeaderButton
removeStopwatchButton which =
    { change = RemoveStopwatch which
    , buttonText = "Remove"
    }


numberCheckerUnderlineClass : Int -> Maybe Int -> String
numberCheckerUnderlineClass numberCheckerId highlightedNumberCheckerId =
    let
        highlightClassPrefix : String
        highlightClassPrefix =
            if highlightedNumberCheckerId == Just numberCheckerId then
                "highlighted "

            else
                ""
    in
    highlightClassPrefix ++ "underlined number-checker-row-" ++ String.fromInt numberCheckerId


numberCheckerUnderlineAttributes : Maybe String -> Maybe Int -> Maybe Int -> List (Table.CellOption a)
numberCheckerUnderlineAttributes className numberCheckerId highlightedNumberCheckerId =
    case ( className, numberCheckerId ) of
        ( Just someClass, Just someNumberCheckerId ) ->
            [ Table.cellAttr (class (someClass ++ " " ++ numberCheckerUnderlineClass someNumberCheckerId highlightedNumberCheckerId)) ]

        ( Nothing, Just someNumberCheckerId ) ->
            [ Table.cellAttr (class (numberCheckerUnderlineClass someNumberCheckerId highlightedNumberCheckerId)) ]

        ( Just someClass, Nothing ) ->
            [ Table.cellAttr (class someClass) ]

        ( Nothing, Nothing ) ->
            []


rowWithNoStopwatchTime : BarcodeScannerData -> Int -> Int -> Table.Row Msg
rowWithNoStopwatchTime barcodeScannerData blankTimeColumns position =
    let
        cells : List (Table.Cell Msg)
        cells =
            [ intCell position ]
                ++ List.repeat blankTimeColumns (plainCell "")
                ++ [ barcodeScannerCell barcodeScannerData position Nothing Nothing ]
    in
    Table.tr [] cells


noStopwatchTableBody : BarcodeScannerData -> Table.TBody Msg
noStopwatchTableBody barcodeScannerData =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0
    in
    List.range 1 maxPosition
        |> List.map (rowWithNoStopwatchTime barcodeScannerData 0)
        |> Table.tbody []


singleStopwatchTableBody : List Int -> BarcodeScannerData -> Table.TBody Msg
singleStopwatchTableBody stopwatchTimes barcodeScannerData =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0

        rowsWithStopwatches : List (Table.Row Msg)
        rowsWithStopwatches =
            List.indexedMap (stopwatchRow barcodeScannerData) stopwatchTimes

        additionalRows : List (Table.Row Msg)
        additionalRows =
            List.range (List.length stopwatchTimes + 1) maxPosition
                |> List.map (rowWithNoStopwatchTime barcodeScannerData 1)
    in
    Table.tbody [] (rowsWithStopwatches ++ additionalRows)


mergedTableBody : Maybe Int -> BarcodeScannerData -> List MergedTableRow -> Table.TBody Msg
mergedTableBody highlightedNumberCheckerId barcodeScannerData mergedTable =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0

        maxPositionFromStopwatches : Int
        maxPositionFromStopwatches =
            List.filterMap .rowNumber mergedTable
                |> List.maximum
                |> Maybe.withDefault 0

        rowsWithStopwatches : List (Table.Row Msg)
        rowsWithStopwatches =
            List.map (mergedStopwatchRow highlightedNumberCheckerId barcodeScannerData) mergedTable

        additionalRows : List (Table.Row Msg)
        additionalRows =
            List.range (maxPositionFromStopwatches + 1) maxPosition
                |> List.map (rowWithNoStopwatchTime barcodeScannerData 2)
    in
    Table.tbody [] (rowsWithStopwatches ++ additionalRows)


stopwatchButtons : WhichStopwatch -> List TableHeaderButton
stopwatchButtons whichStopwatch =
    [ downloadStopwatchButton whichStopwatch, removeStopwatchButton whichStopwatch ]


stopwatchTable : Stopwatches -> BarcodeScannerData -> Maybe Int -> Html Msg
stopwatchTable stopwatches barcodeScannerData highlightedNumberCheckerId =
    let
        appendAthletesHeader : List TableHeaderWithButtons -> List TableHeaderWithButtons
        appendAthletesHeader headers =
            if BarcodeScanner.isEmpty barcodeScannerData then
                headers

            else
                headers ++ [ TableHeaderWithButtons "Athletes" "" [] ]
    in
    case stopwatches of
        None ->
            if BarcodeScanner.isEmpty barcodeScannerData then
                text ""

            else
                Table.table
                    { options = tableOptions
                    , thead =
                        tableHeadersWithButtons
                            (appendAthletesHeader [ TableHeaderWithButtons "Position" "" [] ])
                    , tbody = noStopwatchTableBody barcodeScannerData
                    }

        Single filename stopwatchTimes ->
            Table.table
                { options = tableOptions
                , thead =
                    tableHeadersWithButtons
                        (appendAthletesHeader
                            [ TableHeaderWithButtons "Position" "" []
                            , TableHeaderWithButtons "Stopwatch 1" filename (stopwatchButtons StopwatchOne)
                            ]
                        )
                , tbody = singleStopwatchTableBody stopwatchTimes barcodeScannerData
                }

        Double doubleStopwatchData ->
            Table.table
                { options = tableOptions
                , thead =
                    tableHeadersWithButtons
                        (appendAthletesHeader
                            [ TableHeaderWithButtons "Position" "" []
                            , TableHeaderWithButtons "Stopwatch 1" doubleStopwatchData.filename1 (stopwatchButtons StopwatchOne)
                            , TableHeaderWithButtons "Stopwatch 2" doubleStopwatchData.filename2 (stopwatchButtons StopwatchTwo)
                            ]
                        )
                , tbody = mergedTableBody highlightedNumberCheckerId barcodeScannerData doubleStopwatchData.mergedTableRows
                }


matchSummaryViewRow : Int -> String -> Maybe (Html Msg)
matchSummaryViewRow count label =
    if count == 0 then
        Nothing

    else if count == 1 then
        let
            singularisedLabel : String
            singularisedLabel =
                label
                    |> String.replace "times match" "time matches"
                    |> String.replace "times" "time"
        in
        Just (div [] [ text ("1 " ++ singularisedLabel) ])

    else
        Just (div [] [ text (String.fromInt count ++ " " ++ label) ])


stopwatchMatchSummaryView : Stopwatches -> Html Msg
stopwatchMatchSummaryView stopwatches =
    case stopwatches of
        None ->
            div [] []

        Single _ _ ->
            div [] []

        Double doubleStopwatches ->
            let
                summary : StopwatchMatchSummary
                summary =
                    doubleStopwatches.matchSummary

                rows : List (Maybe (Html Msg))
                rows =
                    [ matchSummaryViewRow summary.exactMatches "times match exactly"
                    , matchSummaryViewRow summary.nearMatches "times match within one second"
                    , matchSummaryViewRow summary.notNearMatches "times match not within one second"
                    , matchSummaryViewRow summary.stopwatch1Only "times on stopwatch 1 only"
                    , matchSummaryViewRow summary.stopwatch2Only "times on stopwatch 2 only"
                    ]
            in
            div
                [ id "stopwatchMatchSummaryView" ]
                (List.filterMap identity rows)


stopwatchRow : BarcodeScannerData -> Int -> Int -> Table.Row Msg
stopwatchRow barcodeScannerData index time =
    let
        firstTwoCells : List (Table.Cell Msg)
        firstTwoCells =
            [ intCell (index + 1)
            , plainCell (formatTime time)
            ]

        rowCells : List (Table.Cell Msg)
        rowCells =
            if BarcodeScanner.isEmpty barcodeScannerData then
                firstTwoCells

            else
                firstTwoCells ++ [ barcodeScannerCell barcodeScannerData (index + 1) Nothing Nothing ]
    in
    Table.tr [] rowCells


mergedStopwatchRow : Maybe Int -> BarcodeScannerData -> MergedTableRow -> Table.Row Msg
mergedStopwatchRow highlightedNumberCheckerId barcodeScannerData row =
    let
        indexCell : Table.Cell Msg
        indexCell =
            case row.rowNumber of
                Just num ->
                    cell (String.fromInt num) row.underlines.actual highlightedNumberCheckerId

                Nothing ->
                    emptyNumberCell

        thisBarcodeScannerCell : Table.Cell Msg
        thisBarcodeScannerCell =
            case row.rowNumber of
                Just num ->
                    barcodeScannerCell barcodeScannerData num row.underlines.position highlightedNumberCheckerId

                Nothing ->
                    emptyBarcodeScannerCell Nothing Nothing

        appendScannerCell : List (Table.Cell Msg) -> List (Table.Cell Msg)
        appendScannerCell cells =
            if BarcodeScanner.isEmpty barcodeScannerData then
                cells

            else
                cells ++ [ thisBarcodeScannerCell ]

        generateTwoTimeRow : String -> Int -> Int -> Table.Row Msg
        generateTwoTimeRow className time1 time2 =
            let
                firstThreeCells : List (Table.Cell Msg)
                firstThreeCells =
                    [ indexCell
                    , timeCell className time1 row.underlines.stopwatch1 highlightedNumberCheckerId
                    , timeCell className time2 row.underlines.stopwatch2 highlightedNumberCheckerId
                    ]
            in
            Table.tr [] (appendScannerCell firstThreeCells)
    in
    case row.entry of
        ExactMatch time ->
            generateTwoTimeRow "exact-match" time time

        NearMatch time1 time2 ->
            generateTwoTimeRow "near-match" time1 time2

        NotNearMatch time1 time2 ->
            generateTwoTimeRow "not-near-match" time1 time2

        OneWatchOnly StopwatchOne time1 ->
            Table.tr
                []
                (appendScannerCell
                    [ indexCell
                    , checkboxCell time1 row.index row.included row.underlines.stopwatch1 highlightedNumberCheckerId
                    , plainCell ""
                    ]
                )

        OneWatchOnly StopwatchTwo time2 ->
            Table.tr
                []
                (appendScannerCell
                    [ indexCell
                    , plainCell ""
                    , checkboxCell time2 row.index row.included row.underlines.stopwatch2 highlightedNumberCheckerId
                    ]
                )


stopwatchesView : Stopwatches -> BarcodeScannerData -> Problems -> Maybe Int -> Html Msg
stopwatchesView stopwatches barcodeScannerData problems highlightedNumberCheckerId =
    let
        stopwatchOperationsButton : Html Msg
        stopwatchOperationsButton =
            normalButton ShowStopwatchOperationsModal [] "Stopwatch operations..."

        buttons : List (Html Msg)
        buttons =
            case stopwatches of
                None ->
                    []

                Single _ _ ->
                    [ stopwatchOperationsButton ]

                Double _ ->
                    [ stopwatchOperationsButton
                    , normalButton (RequestCurrentDateAndTime Commands.DownloadMergedStopwatches) [] "Download merged times"
                    ]
    in
    div
        []
        [ h3 [] (text "Stopwatches" :: [ div [ class "stopwatch-buttons" ] buttons ])
        , stopwatchProblemsView problems
        , stopwatchTable stopwatches barcodeScannerData highlightedNumberCheckerId
        , div
            []
            [ stopwatchMatchSummaryView stopwatches ]
        ]
