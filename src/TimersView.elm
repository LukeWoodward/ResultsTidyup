module TimersView exposing (timersView)

import BarcodeScanner exposing (AthleteAndTimePair, BarcodeScannerData, maxFinishToken)
import Commands
import Dict
import Html exposing (Html, div, h3, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, classList, for, id, title, type_)
import Html.Events exposing (onClick)
import Icons exposing (download, remove)
import Msg exposing (Msg(..))
import Problems exposing (Problems)
import ProblemsView exposing (timerProblemsView)
import TimeHandling exposing (formatTime)
import Timer exposing (MergeEntry(..), MergedTableRow, TimerMatchSummary, Timers(..), WhichTimer(..))
import ViewCommon exposing (athleteLink, dangerIconButton, intCell, normalButton, normalIconButton, plainCell)


tableClass : String
tableClass =
    "table table-bordered timer-times"


type ButtonType
    = Primary
    | Danger


type alias TableHeaderButton =
    { change : Msg
    , option : ButtonType
    , icon : Html Msg
    , buttonTooltip : String
    }


type alias TableHeaderWithButtons =
    { headerText : String
    , headerTooltip : String
    , buttonData : List TableHeaderButton
    }


athleteItem : AthleteAndTimePair -> Html a
athleteItem athleteAndTimePair =
    div [] [ athleteLink athleteAndTimePair.athlete ]


barcodeScannerCell : BarcodeScannerData -> Int -> Maybe Int -> Maybe Int -> Html a
barcodeScannerCell barcodeScannerData position numberCheckerId highlightedNumberCheckerId =
    case Dict.get position barcodeScannerData.scannedBarcodes of
        Just athleteAndTimePairs ->
            td
                (numberCheckerUnderlineAttributes (Just "scanned-athlete") numberCheckerId highlightedNumberCheckerId)
                (List.map athleteItem athleteAndTimePairs)

        Nothing ->
            emptyBarcodeScannerCell numberCheckerId highlightedNumberCheckerId


cell : String -> Maybe Int -> Maybe Int -> Html a
cell contents numberCheckerId highlightedNumberCheckerId =
    td (numberCheckerUnderlineAttributes Nothing numberCheckerId highlightedNumberCheckerId) [ text contents ]


timeCell : String -> Int -> Maybe Int -> Maybe Int -> Html a
timeCell className time numberCheckerId highlightedNumberCheckerId =
    td (numberCheckerUnderlineAttributes (Just className) numberCheckerId highlightedNumberCheckerId) [ text (formatTime time) ]


emptyBarcodeScannerCell : Maybe Int -> Maybe Int -> Html a
emptyBarcodeScannerCell maybeNumberCheckerId highlightedNumberCheckerId =
    td (numberCheckerUnderlineAttributes (Just "no-scanned-athlete") maybeNumberCheckerId highlightedNumberCheckerId) [ text "−" ]


emptyNumberCell : Html a
emptyNumberCell =
    td [ class "empty-cell" ] [ text "–" ]


checkboxCell : Int -> Int -> Bool -> Maybe Int -> Maybe Int -> Html Msg
checkboxCell time index included numberCheckerId highlightedNumberCheckerId =
    let
        idText : String
        idText =
            "toggle_checkbox_" ++ String.fromInt index
    in
    td
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
            , class "timer-time-label"
            , classList [ ( "excluded", not included ) ]
            ]
            [ text (formatTime time) ]
        ]


tableHeaderWithButtons : TableHeaderWithButtons -> Html Msg
tableHeaderWithButtons { headerText, headerTooltip, buttonData } =
    let
        elements : List (Html Msg)
        elements =
            List.map
                (\{ change, option, icon, buttonTooltip } ->
                    case option of
                        Primary ->
                            normalIconButton change icon buttonTooltip

                        Danger ->
                            dangerIconButton change icon buttonTooltip
                )
                buttonData
    in
    th
        [ class "timer-header"
        , title headerTooltip
        ]
        [ div [ class "timer-header-buttons" ] elements, text headerText ]


tableHeadersWithButtons : List TableHeaderWithButtons -> Html Msg
tableHeadersWithButtons headerTexts =
    thead [] [ tr [] (List.map tableHeaderWithButtons headerTexts) ]


downloadTimerButton : WhichTimer -> TableHeaderButton
downloadTimerButton which =
    { change = RequestCurrentDateAndTime (Commands.DownloadSingleTimer which)
    , option = Primary
    , icon = download
    , buttonTooltip = "Download"
    }


removeTimerButton : WhichTimer -> TableHeaderButton
removeTimerButton which =
    { change = RemoveTimer which
    , option = Danger
    , icon = remove
    , buttonTooltip = "Remove"
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


numberCheckerUnderlineAttributes : Maybe String -> Maybe Int -> Maybe Int -> List (Html.Attribute a)
numberCheckerUnderlineAttributes className numberCheckerId highlightedNumberCheckerId =
    case ( className, numberCheckerId ) of
        ( Just someClass, Just someNumberCheckerId ) ->
            [ class (someClass ++ " " ++ numberCheckerUnderlineClass someNumberCheckerId highlightedNumberCheckerId) ]

        ( Nothing, Just someNumberCheckerId ) ->
            [ class (numberCheckerUnderlineClass someNumberCheckerId highlightedNumberCheckerId) ]

        ( Just someClass, Nothing ) ->
            [ class someClass ]

        ( Nothing, Nothing ) ->
            []


rowWithNoTimerTime : BarcodeScannerData -> Int -> Int -> Html Msg
rowWithNoTimerTime barcodeScannerData blankTimeColumns position =
    let
        cells : List (Html Msg)
        cells =
            intCell position
                :: List.repeat blankTimeColumns (plainCell "")
                ++ [ barcodeScannerCell barcodeScannerData position Nothing Nothing ]
    in
    tr [] cells


noTimerTableBody : BarcodeScannerData -> Html Msg
noTimerTableBody barcodeScannerData =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0
    in
    List.range 1 maxPosition
        |> List.map (rowWithNoTimerTime barcodeScannerData 0)
        |> tbody []


singleTimerTableBody : List Int -> BarcodeScannerData -> Html Msg
singleTimerTableBody timerTimes barcodeScannerData =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0

        rowsWithTimers : List (Html Msg)
        rowsWithTimers =
            List.indexedMap (timerRow barcodeScannerData) timerTimes

        additionalRows : List (Html Msg)
        additionalRows =
            List.range (List.length timerTimes + 1) maxPosition
                |> List.map (rowWithNoTimerTime barcodeScannerData 1)
    in
    tbody [] (rowsWithTimers ++ additionalRows)


mergedTableBody : Maybe Int -> BarcodeScannerData -> List MergedTableRow -> Html Msg
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

        rowsWithTimers : List (Html Msg)
        rowsWithTimers =
            List.map (mergedTimerRow highlightedNumberCheckerId barcodeScannerData) mergedTable

        additionalRows : List (Html Msg)
        additionalRows =
            List.range (maxPositionFromTimers + 1) maxPosition
                |> List.map (rowWithNoTimerTime barcodeScannerData 2)
    in
    tbody [] (rowsWithTimers ++ additionalRows)


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
                table
                    [ class tableClass ]
                    [ tableHeadersWithButtons
                        (appendAthletesHeader [ TableHeaderWithButtons "Pos" "" [] ])
                    , noTimerTableBody barcodeScannerData
                    ]

        Single file timerTimes ->
            table
                [ class tableClass ]
                [ tableHeadersWithButtons
                    (appendAthletesHeader
                        [ TableHeaderWithButtons "Pos" "" []
                        , TableHeaderWithButtons ("Timer 1: " ++ file.name) file.filename (timerButtons TimerOne)
                        ]
                    )
                , singleTimerTableBody timerTimes barcodeScannerData
                ]

        Double doubleTimerData ->
            table
                [ class tableClass ]
                [ tableHeadersWithButtons
                    (appendAthletesHeader
                        [ TableHeaderWithButtons "Pos" "" []
                        , TableHeaderWithButtons ("Timer 1: " ++ doubleTimerData.file1.name) doubleTimerData.file1.filename (timerButtons TimerOne)
                        , TableHeaderWithButtons ("Timer 2: " ++ doubleTimerData.file2.name) doubleTimerData.file2.filename (timerButtons TimerTwo)
                        ]
                    )
                , mergedTableBody highlightedNumberCheckerId barcodeScannerData doubleTimerData.mergedTableRows
                ]


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


timerRow : BarcodeScannerData -> Int -> Int -> Html Msg
timerRow barcodeScannerData index time =
    let
        firstTwoCells : List (Html Msg)
        firstTwoCells =
            [ intCell (index + 1)
            , plainCell (formatTime time)
            ]

        rowCells : List (Html Msg)
        rowCells =
            if BarcodeScanner.isEmpty barcodeScannerData then
                firstTwoCells

            else
                firstTwoCells ++ [ barcodeScannerCell barcodeScannerData (index + 1) Nothing Nothing ]
    in
    tr [] rowCells


mergedTimerRow : Maybe Int -> BarcodeScannerData -> MergedTableRow -> Html Msg
mergedTimerRow highlightedNumberCheckerId barcodeScannerData row =
    let
        indexCell : Html Msg
        indexCell =
            case row.rowNumber of
                Just num ->
                    cell (String.fromInt num) row.underlines.actual highlightedNumberCheckerId

                Nothing ->
                    emptyNumberCell

        thisBarcodeScannerCell : Html Msg
        thisBarcodeScannerCell =
            case row.rowNumber of
                Just num ->
                    barcodeScannerCell barcodeScannerData num row.underlines.position highlightedNumberCheckerId

                Nothing ->
                    emptyBarcodeScannerCell Nothing Nothing

        appendScannerCell : List (Html Msg) -> List (Html Msg)
        appendScannerCell cells =
            if BarcodeScanner.isEmpty barcodeScannerData then
                cells

            else
                cells ++ [ thisBarcodeScannerCell ]

        generateTwoTimeRow : String -> Int -> Int -> Html Msg
        generateTwoTimeRow className time1 time2 =
            let
                firstThreeCells : List (Html Msg)
                firstThreeCells =
                    [ indexCell
                    , timeCell className time1 row.underlines.timer1 highlightedNumberCheckerId
                    , timeCell className time2 row.underlines.timer2 highlightedNumberCheckerId
                    ]
            in
            tr [] (appendScannerCell firstThreeCells)
    in
    case row.entry of
        ExactMatch time ->
            generateTwoTimeRow "exact-match" time time

        NearMatch time1 time2 ->
            generateTwoTimeRow "near-match" time1 time2

        NotNearMatch time1 time2 ->
            generateTwoTimeRow "not-near-match" time1 time2

        OneWatchOnly TimerOne time1 ->
            tr
                []
                (appendScannerCell
                    [ indexCell
                    , checkboxCell time1 row.index row.included row.underlines.timer1 highlightedNumberCheckerId
                    , plainCell ""
                    ]
                )

        OneWatchOnly TimerTwo time2 ->
            tr
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
