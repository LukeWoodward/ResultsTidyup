module StopwatchesView exposing (stopwatchesView)

import BarcodeScanner exposing (AthleteAndTimePair, BarcodeScannerData, isEmpty, maxFinishToken)
import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import DataStructures exposing (WhichStopwatch(..))
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, h3, input, label, small, text)
import Html.Attributes exposing (checked, class, for, href, id, rel, target, type_)
import Html.Events exposing (onClick)
import MergedTable exposing (MergedTableRow, Stopwatches(..))
import Merger exposing (MergeEntry(..))
import Msg exposing (Msg(..))
import TimeHandling exposing (formatTime)
import ViewCommon exposing (intCell, plainCell, smallButton)


urlResultsPrefix : String
urlResultsPrefix =
    "http://www.parkrun.org.uk/results/athleteresultshistory/?athleteNumber="


tableOptions : List (Table.TableOption a)
tableOptions =
    [ Table.small, Table.bordered, Table.attr (class "stopwatch-times") ]


twoLineButton : Msg -> String -> String -> Html Msg
twoLineButton msg firstLine secondLine =
    Button.button
        [ Button.primary
        , Button.onClick msg
        ]
        [ text firstLine
        , br [] []
        , small [] [ text secondLine ]
        ]


type alias TableHeaderButton =
    { change : Msg
    , buttonText : String
    }


type alias TableHeaderWithButtons =
    { headerText : String
    , buttonData : List TableHeaderButton
    }


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


athleteItem : AthleteAndTimePair -> Html a
athleteItem athleteAndTimePair =
    let
        athlete =
            athleteAndTimePair.athlete
    in
    div
        []
        [ a
            [ rel "nofollow"
            , href (urlResultsPrefix ++ String.dropLeft 1 athlete)
            , target "_blank"
            ]
            [ text athlete ]
        ]


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
tableHeaderWithButtons { headerText, buttonData } =
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
        [ Table.cellAttr (class "stopwatch-header") ]
        elements


tableHeadersWithButtons : List TableHeaderWithButtons -> Table.THead Msg
tableHeadersWithButtons headerTexts =
    Table.simpleThead (List.map tableHeaderWithButtons headerTexts)


downloadStopwatchButton : WhichStopwatch -> TableHeaderButton
downloadStopwatchButton which =
    { change = GetCurrentDateForDownloadFile (DownloadStopwatch which)
    , buttonText = "Download "
    }


deleteStopwatchButton : WhichStopwatch -> TableHeaderButton
deleteStopwatchButton which =
    { change = DeleteStopwatch which
    , buttonText = "Delete "
    }


stopwatchInfoMessage : Stopwatches -> Html a
stopwatchInfoMessage stopwatches =
    let
        message : Maybe String
        message =
            case stopwatches of
                None ->
                    Just "No stopwatch files have been loaded"

                Single _ _ ->
                    Nothing

                Double _ ->
                    Nothing
    in
    case message of
        Just messageText ->
            Alert.simpleInfo [] [ text messageText ]

        Nothing ->
            text ""


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


stopwatchTable : Stopwatches -> BarcodeScannerData -> Maybe Int -> Html Msg
stopwatchTable stopwatches barcodeScannerData highlightedNumberCheckerId =
    case stopwatches of
        None ->
            if isEmpty barcodeScannerData then
                text ""

            else
                Table.table
                    { options = tableOptions
                    , thead =
                        tableHeadersWithButtons
                            [ TableHeaderWithButtons "Position" []
                            , TableHeaderWithButtons "Athletes" []
                            ]
                    , tbody = noStopwatchTableBody barcodeScannerData
                    }

        Single _ stopwatchTimes ->
            Table.table
                { options = tableOptions
                , thead =
                    tableHeadersWithButtons
                        [ TableHeaderWithButtons "Position" []
                        , TableHeaderWithButtons "Stopwatch 1" [ downloadStopwatchButton StopwatchOne, deleteStopwatchButton StopwatchOne ]
                        , TableHeaderWithButtons "Athletes" []
                        ]
                , tbody = singleStopwatchTableBody stopwatchTimes barcodeScannerData
                }

        Double doubleStopwatchData ->
            Table.table
                { options = tableOptions
                , thead =
                    tableHeadersWithButtons
                        [ TableHeaderWithButtons "Position" []
                        , TableHeaderWithButtons "Stopwatch 1" [ downloadStopwatchButton StopwatchOne, deleteStopwatchButton StopwatchOne ]
                        , TableHeaderWithButtons "Stopwatch 2" [ downloadStopwatchButton StopwatchTwo, deleteStopwatchButton StopwatchTwo ]
                        , TableHeaderWithButtons "Athletes" []
                        ]
                , tbody = mergedTableBody highlightedNumberCheckerId barcodeScannerData doubleStopwatchData.mergedTableRows
                }


stopwatchButtonsContent : Stopwatches -> List (Html Msg)
stopwatchButtonsContent stopwatches =
    case stopwatches of
        None ->
            []

        Single _ _ ->
            []

        Double _ ->
            [ twoLineButton FlipStopwatches "Flip" "stopwatches"
            , br [] []
            , br [] []
            , twoLineButton (GetCurrentDateForDownloadFile DownloadMergedStopwatchData) "Download" "merged times"
            , br [] []
            , br [] []
            , twoLineButton ClearAllData "Clear" "all data"
            ]


stopwatchRow : BarcodeScannerData -> Int -> Int -> Table.Row a
stopwatchRow barcodeScannerData index time =
    Table.tr []
        [ intCell (index + 1)
        , plainCell (formatTime time)
        , barcodeScannerCell barcodeScannerData (index + 1) Nothing Nothing
        ]


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
    in
    case row.entry of
        ExactMatch time ->
            Table.tr
                []
                [ indexCell
                , timeCell "exact-match" time row.underlines.stopwatch1 highlightedNumberCheckerId
                , timeCell "exact-match" time row.underlines.stopwatch2 highlightedNumberCheckerId
                , thisBarcodeScannerCell
                ]

        NearMatch time1 time2 ->
            Table.tr
                []
                [ indexCell
                , timeCell "near-match" time1 row.underlines.stopwatch1 highlightedNumberCheckerId
                , timeCell "near-match" time2 row.underlines.stopwatch2 highlightedNumberCheckerId
                , thisBarcodeScannerCell
                ]

        OneWatchOnly StopwatchOne time1 ->
            Table.tr
                []
                [ indexCell
                , checkboxCell time1 row.index row.included row.underlines.stopwatch1 highlightedNumberCheckerId
                , plainCell ""
                , thisBarcodeScannerCell
                ]

        OneWatchOnly StopwatchTwo time2 ->
            Table.tr
                []
                [ indexCell
                , plainCell ""
                , checkboxCell time2 row.index row.included row.underlines.stopwatch2 highlightedNumberCheckerId
                , thisBarcodeScannerCell
                ]


stopwatchesView : Stopwatches -> BarcodeScannerData -> Maybe Int -> Maybe Int -> Html Msg
stopwatchesView stopwatches barcodeScannerData lastHeight highlightedNumberCheckerId =
    div
        []
        [ h3 [] [ text "Stopwatches" ]
        , stopwatchInfoMessage stopwatches
        , stopwatchTable stopwatches barcodeScannerData highlightedNumberCheckerId
        , div [ class "stopwatch-buttons" ] (stopwatchButtonsContent stopwatches)
        ]
