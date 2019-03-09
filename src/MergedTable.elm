module MergedTable exposing
    ( DoubleStopwatchData
    , MergedTableRow
    , Stopwatches(..)
    , Underlines
    , flipTable
    , generateInitialTable
    , noUnderlines
    , outputMergedTable
    , toggleRowInTable
    , underlineTable
    )

import DataStructures exposing (WhichStopwatch(..))
import Dict exposing (Dict)
import FileHandling exposing (crlf)
import Merger exposing (MergeEntry(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import TimeHandling exposing (formatTimeWithHours)


type alias DoubleStopwatchData =
    { times1 : List Int
    , times2 : List Int
    , filename1 : String
    , filename2 : String
    , mergedTableRows : List MergedTableRow
    }


type Stopwatches
    = None
    | Single String (List Int)
    | Double DoubleStopwatchData


type alias Underlines =
    { position : Maybe Int
    , stopwatch1 : Maybe Int
    , stopwatch2 : Maybe Int
    }


noUnderlines : Underlines
noUnderlines =
    Underlines Nothing Nothing Nothing


type alias MergedTableRow =
    { index : Int
    , rowNumber : Maybe Int
    , entry : MergeEntry
    , included : Bool
    , underlines : Underlines
    }


createInitialTableRow : Int -> MergeEntry -> MergedTableRow
createInitialTableRow index entry =
    MergedTableRow index (Just (index + 1)) entry True noUnderlines


generateInitialTable : List MergeEntry -> List MergedTableRow
generateInitialTable entries =
    List.indexedMap createInitialTableRow entries


isSingleTimeRow : MergedTableRow -> Bool
isSingleTimeRow row =
    case row.entry of
        ExactMatch _ ->
            False

        NearMatch _ _ ->
            False

        OneWatchOnly _ _ ->
            True


toggleRowInTableInternal : Int -> Int -> List MergedTableRow -> List MergedTableRow
toggleRowInTableInternal toggledIndex currentRowNumber rows =
    case rows of
        [] ->
            []

        firstRow :: rest ->
            let
                newRow : MergedTableRow
                newRow =
                    if firstRow.index == toggledIndex && isSingleTimeRow firstRow then
                        { firstRow | included = not firstRow.included }

                    else
                        firstRow

                rowNumberStep : Int
                rowNumberStep =
                    if newRow.included then
                        1

                    else
                        0

                newRowNumber : Maybe Int
                newRowNumber =
                    if newRow.included then
                        Just currentRowNumber

                    else
                        Nothing

                remainingRows : List MergedTableRow
                remainingRows =
                    toggleRowInTableInternal toggledIndex (currentRowNumber + rowNumberStep) rest
            in
            { newRow | rowNumber = newRowNumber } :: remainingRows


toggleRowInTable : Int -> List MergedTableRow -> List MergedTableRow
toggleRowInTable index rows =
    toggleRowInTableInternal index 1 rows


flipStopwatch : WhichStopwatch -> WhichStopwatch
flipStopwatch stopwatch =
    case stopwatch of
        StopwatchOne ->
            StopwatchTwo

        StopwatchTwo ->
            StopwatchOne


flipRow : MergedTableRow -> MergedTableRow
flipRow row =
    case row.entry of
        ExactMatch _ ->
            row

        NearMatch time1 time2 ->
            { row | entry = NearMatch time2 time1 }

        OneWatchOnly watch time ->
            { row | entry = OneWatchOnly (flipStopwatch watch) time }


flipTable : List MergedTableRow -> List MergedTableRow
flipTable =
    List.map flipRow



-- Each dictionary maps an index within the stopwatch-1, stopwatch-2 and
-- finish-tokens positions to the number-checker entry ID associated.


type alias NumberDicts =
    { stopwatch1 : Dict Int Int
    , stopwatch2 : Dict Int Int
    , finishTokens : Dict Int Int
    }


getNextEntry : Int -> Int -> Dict Int Int -> Maybe Int
getNextEntry currentPosn nextPosn numberDict =
    if nextPosn > currentPosn then
        Dict.get nextPosn numberDict

    else
        Nothing


underlineTableInternal : NumberDicts -> Int -> Int -> Int -> List MergedTableRow -> List MergedTableRow
underlineTableInternal numberDicts sw1Posn sw2Posn ftoksPosn mergedRows =
    case mergedRows of
        [] ->
            []

        firstRow :: restRows ->
            let
                nextSw1Posn : Int
                nextSw1Posn =
                    case firstRow.entry of
                        ExactMatch _ ->
                            sw1Posn + 1

                        NearMatch _ _ ->
                            sw1Posn + 1

                        OneWatchOnly StopwatchOne _ ->
                            sw1Posn + 1

                        OneWatchOnly StopwatchTwo _ ->
                            sw1Posn

                nextSw2Posn : Int
                nextSw2Posn =
                    case firstRow.entry of
                        ExactMatch _ ->
                            sw2Posn + 1

                        NearMatch _ _ ->
                            sw2Posn + 1

                        OneWatchOnly StopwatchOne _ ->
                            sw2Posn

                        OneWatchOnly StopwatchTwo _ ->
                            sw2Posn + 1

                nextFtoksPosn : Int
                nextFtoksPosn =
                    if firstRow.included then
                        ftoksPosn + 1

                    else
                        ftoksPosn

                newUnderlines : Underlines
                newUnderlines =
                    { stopwatch1 = getNextEntry sw1Posn nextSw1Posn numberDicts.stopwatch1
                    , stopwatch2 = getNextEntry sw2Posn nextSw2Posn numberDicts.stopwatch2
                    , position = getNextEntry ftoksPosn nextFtoksPosn numberDicts.finishTokens
                    }

                underlinedFirstRow : MergedTableRow
                underlinedFirstRow =
                    { firstRow | underlines = newUnderlines }

                underlinedRestRows : List MergedTableRow
                underlinedRestRows =
                    underlineTableInternal numberDicts nextSw1Posn nextSw2Posn nextFtoksPosn restRows
            in
            underlinedFirstRow :: underlinedRestRows


createMappingDict : List Int -> Dict Int Int
createMappingDict nums =
    List.indexedMap (\index num -> ( num, index + 1 )) nums
        |> Dict.fromList


underlineTable : List AnnotatedNumberCheckerEntry -> List MergedTableRow -> List MergedTableRow
underlineTable numberCheckerEntries mergedRows =
    let
        stopwatch1Numbers : Dict Int Int
        stopwatch1Numbers =
            numberCheckerEntries
                |> List.map .stopwatch1
                |> createMappingDict

        stopwatch2Numbers : Dict Int Int
        stopwatch2Numbers =
            numberCheckerEntries
                |> List.map .stopwatch2
                |> createMappingDict

        finishTokensNumbers : Dict Int Int
        finishTokensNumbers =
            numberCheckerEntries
                |> List.map .finishTokens
                |> createMappingDict
    in
    underlineTableInternal (NumberDicts stopwatch1Numbers stopwatch2Numbers finishTokensNumbers) 0 0 0 mergedRows


header : List String
header =
    [ "STARTOFEVENT,01/01/2001 00:00:00,results_tidyup", "0,01/01/2001 00:00:00" ]


footer : String
footer =
    "ENDOFEVENT,01/01/2001 01:59:59"


formatRow : Int -> Int -> Maybe String
formatRow rowNumber time =
    let
        formattedTime : String
        formattedTime =
            formatTimeWithHours time
    in
    String.fromInt rowNumber
        ++ ",01/01/2001 "
        ++ formattedTime
        ++ ","
        ++ formattedTime
        |> Just


outputMergedRow : MergedTableRow -> Maybe String
outputMergedRow row =
    case row.rowNumber of
        Just rowNum ->
            case row.entry of
                ExactMatch time ->
                    formatRow rowNum time

                NearMatch time1 time2 ->
                    formatRow rowNum (min time1 time2)

                OneWatchOnly _ time ->
                    if row.included then
                        formatRow rowNum time

                    else
                        Nothing

        Nothing ->
            Nothing


outputMergedTable : List MergedTableRow -> String
outputMergedTable mergedRows =
    header
        ++ List.filterMap outputMergedRow mergedRows
        ++ [ footer ]
        |> String.join crlf
