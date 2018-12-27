module StopwatchesView exposing (stopwatchesView)

import BarcodeScanner exposing (BarcodeScannerData, isEmpty, maxFinishToken)
import DataStructures exposing (WhichStopwatch(..))
import Dict exposing (Dict)
import Html exposing (Html, a, br, button, div, h3, input, label, small, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, for, href, id, rel, target, type_)
import Html.Events exposing (onClick)
import MergedTable exposing (MergedTableRow, Stopwatches(..))
import Merger exposing (MergeEntry(..))
import Msg exposing (Msg(..))
import TimeHandling exposing (formatTime)
import ViewCommon exposing (intCell, plainCell)


parkrunUrlResultsPrefix : String
parkrunUrlResultsPrefix =
    "http://www.parkrun.org.uk/results/athleteresultshistory/?athleteNumber="


type alias TableHeaderButton =
    { change : Msg
    , buttonText : String
    }


type alias TableHeaderWithButton =
    { headerText : String
    , buttonData : Maybe TableHeaderButton
    }


barcodeScannerCell : BarcodeScannerData -> Int -> Maybe Int -> Maybe Int -> Html a
barcodeScannerCell barcodeScannerData position numberCheckerId highlightedNumberCheckerId =
    case Dict.get position barcodeScannerData.scannedBarcodes of
        Just athletes ->
            td
                (numberCheckerUnderlineAttributes (Just "scanned-athlete") numberCheckerId highlightedNumberCheckerId)
                (List.map athleteItem athletes)

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


athleteItem : String -> Html a
athleteItem athlete =
    div
        []
        [ a
            [ rel "nofollow"
            , href (parkrunUrlResultsPrefix ++ String.dropLeft 1 athlete)
            , target "_blank"
            ]
            [ text athlete ]
        ]


emptyNumberCell : Html a
emptyNumberCell =
    td [ class "empty-cell" ] [ text "–" ]


checkboxCell : Int -> Int -> Bool -> Maybe Int -> Maybe Int -> Html Msg
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
            , class labelClassName
            ]
            [ text (formatTime time) ]
        ]


tableHeaderWithButtons : TableHeaderWithButton -> Html Msg
tableHeaderWithButtons { headerText, buttonData } =
    let
        textElement : Html Msg
        textElement =
            text headerText
    in
    case buttonData of
        Just { change, buttonText } ->
            th
                [ class "stopwatch-header" ]
                [ button
                    [ type_ "button"
                    , onClick change
                    , class "btn btn-primary btn-xs"
                    ]
                    [ text buttonText ]
                , br [] []
                , textElement
                ]

        Nothing ->
            th [] [ textElement ]


tableHeadersWithButtons : List TableHeaderWithButton -> Html Msg
tableHeadersWithButtons headerTexts =
    thead
        []
        [ tr
            []
            (List.map tableHeaderWithButtons headerTexts)
        ]


deleteStopwatchButton : WhichStopwatch -> Maybe TableHeaderButton
deleteStopwatchButton which =
    Just
        { change = DeleteStopwatch which
        , buttonText = "Delete "
        }


clearBarcodeScannerButton : Maybe TableHeaderButton
clearBarcodeScannerButton =
    Just
        { change = ClearBarcodeScannerData
        , buttonText = "Clear"
        }


stopwatchInfoMessage : Stopwatches -> Html a
stopwatchInfoMessage stopwatches =
    let
        message : Maybe String
        message =
            case stopwatches of
                None ->
                    Just "No stopwatch files have been uploaded"

                Single _ _ ->
                    Just "Please upload another stopwatch file to enable comparison/merging"

                Double _ _ _ ->
                    Nothing
    in
    case message of
        Just messageText ->
            div [ class "alert alert-info" ]
                [ text messageText ]

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


rowWithNoStopwatchTime : BarcodeScannerData -> Int -> Int -> Html Msg
rowWithNoStopwatchTime barcodeScannerData blankTimeColumns position =
    let
        cells : List (Html Msg)
        cells =
            [ intCell position ]
                ++ List.repeat blankTimeColumns (plainCell "")
                ++ [ barcodeScannerCell barcodeScannerData position Nothing Nothing ]
    in
    tr [] cells


noStopwatchTableBody : BarcodeScannerData -> Html Msg
noStopwatchTableBody barcodeScannerData =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0
    in
    List.range 1 maxPosition
        |> List.map (rowWithNoStopwatchTime barcodeScannerData 0)
        |> tbody []


singleStopwatchTableBody : List Int -> BarcodeScannerData -> Html Msg
singleStopwatchTableBody stopwatchTimes barcodeScannerData =
    let
        maxPosition : Int
        maxPosition =
            maxFinishToken barcodeScannerData
                |> Maybe.withDefault 0

        rowsWithStopwatches : List (Html Msg)
        rowsWithStopwatches =
            List.indexedMap (stopwatchRow barcodeScannerData) stopwatchTimes

        additionalRows : List (Html Msg)
        additionalRows =
            List.range (List.length stopwatchTimes + 1) maxPosition
                |> List.map (rowWithNoStopwatchTime barcodeScannerData 1)
    in
    tbody [] (rowsWithStopwatches ++ additionalRows)


mergedTableBody : Maybe Int -> BarcodeScannerData -> List MergedTableRow -> Html Msg
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

        rowsWithStopwatches : List (Html Msg)
        rowsWithStopwatches =
            List.map (mergedStopwatchRow highlightedNumberCheckerId barcodeScannerData) mergedTable

        additionalRows : List (Html Msg)
        additionalRows =
            List.range (maxPositionFromStopwatches + 1) maxPosition
                |> List.map (rowWithNoStopwatchTime barcodeScannerData 2)
    in
    tbody [] (rowsWithStopwatches ++ additionalRows)


stopwatchTable : Stopwatches -> BarcodeScannerData -> Maybe Int -> Html Msg
stopwatchTable stopwatches barcodeScannerData highlightedNumberCheckerId =
    case stopwatches of
        None ->
            if isEmpty barcodeScannerData then
                text ""

            else
                table
                    [ class "table table-condensed table-bordered stopwatch-times" ]
                    [ tableHeadersWithButtons
                        [ TableHeaderWithButton "Position" Nothing
                        , TableHeaderWithButton "Athletes" clearBarcodeScannerButton
                        ]
                    , noStopwatchTableBody barcodeScannerData
                    ]

        Single _ stopwatchTimes ->
            table
                [ class "table table-condensed table-bordered stopwatch-times" ]
                [ tableHeadersWithButtons
                    [ TableHeaderWithButton "Position" Nothing
                    , TableHeaderWithButton "Stopwatch 1" (deleteStopwatchButton StopwatchOne)
                    , TableHeaderWithButton "Athletes" clearBarcodeScannerButton
                    ]
                , singleStopwatchTableBody stopwatchTimes barcodeScannerData
                ]

        Double _ _ mergedTable ->
            table
                [ class "table table-condensed table-bordered stopwatch-times" ]
                [ tableHeadersWithButtons
                    [ TableHeaderWithButton "Position" Nothing
                    , TableHeaderWithButton "Stopwatch 1" (deleteStopwatchButton StopwatchOne)
                    , TableHeaderWithButton "Stopwatch 2" (deleteStopwatchButton StopwatchTwo)
                    , TableHeaderWithButton "Athletes" clearBarcodeScannerButton
                    ]
                , mergedTableBody highlightedNumberCheckerId barcodeScannerData mergedTable
                ]


stopwatchButtonsContent : Stopwatches -> List (Html Msg)
stopwatchButtonsContent stopwatches =
    case stopwatches of
        None ->
            []

        Single _ _ ->
            []

        Double _ _ _ ->
            [ button
                [ class "btn btn-primary btn-large"
                , onClick FlipStopwatches
                ]
                [ text "Flip"
                , br [] []
                , small [] [ text "stopwatches" ]
                ]
            , br [] []
            , br [] []
            , button
                [ class "btn btn-primary btn-large"
                , onClick GetCurrentDateForDownloadFile
                ]
                [ text "Download"
                , br [] []
                , small [] [ text "merged times" ]
                ]
            ]


stopwatchRow : BarcodeScannerData -> Int -> Int -> Html a
stopwatchRow barcodeScannerData index time =
    tr []
        [ intCell (index + 1)
        , plainCell (formatTime time)
        , barcodeScannerCell barcodeScannerData (index + 1) Nothing Nothing
        ]


mergedStopwatchRow : Maybe Int -> BarcodeScannerData -> MergedTableRow -> Html Msg
mergedStopwatchRow highlightedNumberCheckerId barcodeScannerData row =
    let
        indexCell : Html Msg
        indexCell =
            case row.rowNumber of
                Just num ->
                    cell (String.fromInt num) row.underlines.position highlightedNumberCheckerId

                Nothing ->
                    emptyNumberCell

        thisBarcodeScannerCell : Html Msg
        thisBarcodeScannerCell =
            case row.rowNumber of
                Just num ->
                    barcodeScannerCell barcodeScannerData num row.underlines.position highlightedNumberCheckerId

                Nothing ->
                    emptyBarcodeScannerCell Nothing Nothing
    in
    case row.entry of
        ExactMatch time ->
            tr
                []
                [ indexCell
                , timeCell "exact-match" time row.underlines.stopwatch1 highlightedNumberCheckerId
                , timeCell "exact-match" time row.underlines.stopwatch2 highlightedNumberCheckerId
                , thisBarcodeScannerCell
                ]

        NearMatch time1 time2 ->
            tr
                []
                [ indexCell
                , timeCell "near-match" time1 row.underlines.stopwatch1 highlightedNumberCheckerId
                , timeCell "near-match" time2 row.underlines.stopwatch2 highlightedNumberCheckerId
                , thisBarcodeScannerCell
                ]

        OneWatchOnly StopwatchOne time1 ->
            tr
                []
                [ indexCell
                , checkboxCell time1 row.index row.included row.underlines.stopwatch1 highlightedNumberCheckerId
                , plainCell ""
                , thisBarcodeScannerCell
                ]

        OneWatchOnly StopwatchTwo time2 ->
            tr
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
