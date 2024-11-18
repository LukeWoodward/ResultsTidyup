module TimersView exposing (timersView)

import BarcodeScanner exposing (AthleteAndTimePair, BarcodeScannerData, maxFinishToken)
import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Commands
import Dict
import Html exposing (Html, div, h3, input, label, text)
import Html.Attributes exposing (checked, class, for, id, title, type_)
import Html.Events exposing (onClick)
import Icons exposing (download, remove)
import Msg exposing (Msg(..))
import Problems exposing (Problems)
import ProblemsView exposing (timerProblemsView)
import TimeHandling exposing (formatTime)
import Timer exposing (MergeEntry(..), MergedTableRow, TimerMatchSummary, Timers(..), WhichTimer(..))
import ViewCommon exposing (athleteLink, iconButton, intCell, normalButton, plainCell)


tableOptions : List (Table.TableOption a)
tableOptions =
    [ Table.small, Table.bordered, Table.attr (class "timer-times") ]


type alias TableHeaderButton =
    { change : Msg
    , option : Button.Option Msg
    , icon : Html Msg
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
                "timer-time-label"

            else
                "timer-time-label excluded"
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
        elements : List (Html Msg)
        elements =
            List.map (\{ change, option, icon, buttonText } -> iconButton change option icon [] buttonText) buttonData
    in
    Table.th
        [ Table.cellAttr (class "timer-header")
        , Table.cellAttr (title headerTooltip)
        ]
        [ div [ class "timer-header-buttons" ] elements, text headerText ]


tableHeadersWithButtons : List TableHeaderWithButtons -> Table.THead Msg
tableHeadersWithButtons headerTexts =
    Table.simpleThead (List.map tableHeaderWithButtons headerTexts)


downloadTimerButton : WhichTimer -> TableHeaderButton
downloadTimerButton which =
    { change = RequestCurrentDateAndTime (Commands.DownloadSingleTimer which)
    , option = Button.primary
    , icon = download
    , buttonText = "Download"
    }


removeTimerButton : WhichTimer -> TableHeaderButton
removeTimerButton which =
    { change = RemoveTimer which
    , option = Button.danger
    , icon = remove
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


rowWithNoTimerTime : BarcodeScannerData -> Int -> Int -> Table.Row Msg
rowWithNoTimerTime barcodeScannerData blankTimeColumns position =
    let
        cells : List (Table.Cell Msg)
        cells =
            intCell position
                :: List.repeat blankTimeColumns (plainCell "")
                ++ [ barcodeScannerCell barcodeScannerData position Nothing Nothing ]
    in
    Table.tr [] cells


noTimerTableBody : BarcodeScannerData -> Table.TBody Msg
noTimerTableBody barcodeScannerData =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0
    in
    List.range 1 maxPosition
        |> List.map (rowWithNoTimerTime barcodeScannerData 0)
        |> Table.tbody []


singleTimerTableBody : List Int -> BarcodeScannerData -> Table.TBody Msg
singleTimerTableBody timerTimes barcodeScannerData =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0

        rowsWithTimers : List (Table.Row Msg)
        rowsWithTimers =
            List.indexedMap (timerRow barcodeScannerData) timerTimes

        additionalRows : List (Table.Row Msg)
        additionalRows =
            List.range (List.length timerTimes + 1) maxPosition
                |> List.map (rowWithNoTimerTime barcodeScannerData 1)
    in
    Table.tbody [] (rowsWithTimers ++ additionalRows)


mergedTableBody : Maybe Int -> BarcodeScannerData -> List MergedTableRow -> Table.TBody Msg
mergedTableBody highlightedNumberCheckerId barcodeScannerData mergedTable =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0

        maxPositionFromTimers : Int
        maxPositionFromTimers =
            List.filterMap .rowNumber mergedTable
                |> List.maximum
                |> Maybe.withDefault 0

        rowsWithTimers : List (Table.Row Msg)
        rowsWithTimers =
            List.map (mergedTimerRow highlightedNumberCheckerId barcodeScannerData) mergedTable

        additionalRows : List (Table.Row Msg)
        additionalRows =
            List.range (maxPositionFromTimers + 1) maxPosition
                |> List.map (rowWithNoTimerTime barcodeScannerData 2)
    in
    Table.tbody [] (rowsWithTimers ++ additionalRows)


timerButtons : WhichTimer -> List TableHeaderButton
timerButtons whichTimer =
    [ downloadTimerButton whichTimer, removeTimerButton whichTimer ]


timerTable : Timers -> BarcodeScannerData -> Maybe Int -> Html Msg
timerTable timers barcodeScannerData highlightedNumberCheckerId =
    let
        appendAthletesHeader : List TableHeaderWithButtons -> List TableHeaderWithButtons
        appendAthletesHeader headers =
            if BarcodeScanner.isEmpty barcodeScannerData then
                headers

            else
                headers ++ [ TableHeaderWithButtons "Athletes" "" [] ]
    in
    case timers of
        None ->
            if BarcodeScanner.isEmpty barcodeScannerData then
                text ""

            else
                Table.table
                    { options = tableOptions
                    , thead =
                        tableHeadersWithButtons
                            (appendAthletesHeader [ TableHeaderWithButtons "Pos" "" [] ])
                    , tbody = noTimerTableBody barcodeScannerData
                    }

        Single file timerTimes ->
            Table.table
                { options = tableOptions
                , thead =
                    tableHeadersWithButtons
                        (appendAthletesHeader
                            [ TableHeaderWithButtons "Pos" "" []
                            , TableHeaderWithButtons ("Timer 1: " ++ file.name) file.filename (timerButtons TimerOne)
                            ]
                        )
                , tbody = singleTimerTableBody timerTimes barcodeScannerData
                }

        Double doubleTimerData ->
            Table.table
                { options = tableOptions
                , thead =
                    tableHeadersWithButtons
                        (appendAthletesHeader
                            [ TableHeaderWithButtons "Pos" "" []
                            , TableHeaderWithButtons ("Timer 1: " ++ doubleTimerData.file1.name) doubleTimerData.file1.filename (timerButtons TimerOne)
                            , TableHeaderWithButtons ("Timer 2: " ++ doubleTimerData.file2.name) doubleTimerData.file2.filename (timerButtons TimerTwo)
                            ]
                        )
                , tbody = mergedTableBody highlightedNumberCheckerId barcodeScannerData doubleTimerData.mergedTableRows
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


timerMatchSummaryView : Timers -> Html Msg
timerMatchSummaryView timers =
    case timers of
        None ->
            div [] []

        Single _ _ ->
            div [] []

        Double doubleTimers ->
            let
                summary : TimerMatchSummary
                summary =
                    doubleTimers.matchSummary

                rows : List (Maybe (Html Msg))
                rows =
                    [ matchSummaryViewRow summary.exactMatches "times match exactly"
                    , matchSummaryViewRow summary.nearMatches "times match within one second"
                    , matchSummaryViewRow summary.notNearMatches "times match not within one second"
                    , matchSummaryViewRow summary.timer1Only "times on timer 1 only"
                    , matchSummaryViewRow summary.timer2Only "times on timer 2 only"
                    ]
            in
            div
                [ id "timerMatchSummaryView" ]
                (List.filterMap identity rows)


timerRow : BarcodeScannerData -> Int -> Int -> Table.Row Msg
timerRow barcodeScannerData index time =
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


mergedTimerRow : Maybe Int -> BarcodeScannerData -> MergedTableRow -> Table.Row Msg
mergedTimerRow highlightedNumberCheckerId barcodeScannerData row =
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
                    , timeCell className time1 row.underlines.timer1 highlightedNumberCheckerId
                    , timeCell className time2 row.underlines.timer2 highlightedNumberCheckerId
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

        OneWatchOnly TimerOne time1 ->
            Table.tr
                []
                (appendScannerCell
                    [ indexCell
                    , checkboxCell time1 row.index row.included row.underlines.timer1 highlightedNumberCheckerId
                    , plainCell ""
                    ]
                )

        OneWatchOnly TimerTwo time2 ->
            Table.tr
                []
                (appendScannerCell
                    [ indexCell
                    , plainCell ""
                    , checkboxCell time2 row.index row.included row.underlines.timer2 highlightedNumberCheckerId
                    ]
                )


timersView : Timers -> BarcodeScannerData -> Problems -> Maybe Int -> Html Msg
timersView timers barcodeScannerData problems highlightedNumberCheckerId =
    let
        flipTimersButton : Html Msg
        flipTimersButton =
            normalButton FlipTimers [] "← Swap timers →"

        timerOperationsButton : Html Msg
        timerOperationsButton =
            normalButton ShowTimerOperationsModal [] "Timer operations..."

        buttons : List (Html Msg)
        buttons =
            case timers of
                None ->
                    []

                Single _ _ ->
                    [ timerOperationsButton ]

                Double _ ->
                    [ flipTimersButton
                    , timerOperationsButton
                    , normalButton (RequestCurrentDateAndTime Commands.DownloadMergedTimers) [] "Download merged times"
                    ]

        headerText : String
        headerText =
            if timers == None then
                "Positions"

            else
                "Timers"
    in
    div
        []
        [ h3 [] [ text headerText, div [ class "timer-buttons" ] buttons ]
        , timerProblemsView problems
        , timerTable timers barcodeScannerData highlightedNumberCheckerId
        , div
            []
            [ timerMatchSummaryView timers ]
        ]
