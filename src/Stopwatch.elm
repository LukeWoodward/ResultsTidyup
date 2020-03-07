module Stopwatch exposing
    ( DoubleStopwatchData
    , MergeEntry(..)
    , MergedTableRow
    , Stopwatch(..)
    , StopwatchMatchSummary
    , Stopwatches(..)
    , WhichStopwatch(..)
    , createMergedTable
    , flipMatchSummary
    , flipTable
    , footer
    , generateInitialTable
    , header
    , merge
    , noUnderlines
    , outputMergedTable
    , outputSingleStopwatchData
    , readStopwatchData
    , toggleRowInTable
    , underlineTable
    )

import Dict exposing (Dict)
import Error exposing (Error)
import FileHandling exposing (crlf, isPossibleBinary, splitLines)
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import Result.Extra
import TimeHandling exposing (formatTimeWithHours, parseTime)



{- Stopwatch data is basically a list of integer numbers of seconds -}


type Stopwatch
    = StopwatchData (List Int)


type WhichStopwatch
    = StopwatchOne
    | StopwatchTwo


type MergeEntry
    = ExactMatch Int
    | NearMatch Int Int
    | NotNearMatch Int Int
    | OneWatchOnly WhichStopwatch Int


type alias StopwatchMatchSummary =
    { exactMatches : Int
    , nearMatches : Int
    , notNearMatches : Int
    , stopwatch1Only : Int
    , stopwatch2Only : Int
    }


type alias DoubleStopwatchData =
    { times1 : List Int
    , times2 : List Int
    , filename1 : String
    , filename2 : String
    , mergedTableRows : List MergedTableRow
    , matchSummary : StopwatchMatchSummary
    }


type Stopwatches
    = None
    | Single String (List Int)
    | Double DoubleStopwatchData


type alias Underlines =
    { position : Maybe Int
    , stopwatch1 : Maybe Int
    , stopwatch2 : Maybe Int
    , actual : Maybe Int
    }


type alias MergedTableRow =
    { index : Int
    , rowNumber : Maybe Int
    , entry : MergeEntry
    , included : Bool
    , underlines : Underlines
    }


noUnderlines : Underlines
noUnderlines =
    Underlines Nothing Nothing Nothing Nothing


ignorableLinePrefixes : List String
ignorableLinePrefixes =
    [ "STARTOFEVENT"
    , "0,"
    , "ENDOFEVENT"
    ]


filterIgnorableLines : String -> Bool
filterIgnorableLines line =
    List.any (\prefix -> String.startsWith prefix line) ignorableLinePrefixes
        |> not


readLine : String -> Result Error Int
readLine line =
    let
        parts : List String
        parts =
            String.split "," line
    in
    case parts of
        [ _, _, time ] ->
            parseTime time

        _ ->
            Error "NOT_THREE_PARTS" ("Line " ++ line ++ " does not contain the expected three comma-separated parts")
                |> Err


failIfNoResults : List Int -> Result Error (List Int)
failIfNoResults results =
    if List.isEmpty results then
        Error "NO_RESULTS" "Stopwatch data contained no results"
            |> Err

    else
        Ok results


readStopwatchData : String -> Result Error Stopwatch
readStopwatchData text =
    if isPossibleBinary text then
        Error "BINARY_FILE" "File appears to be a binary file"
            |> Err

    else
        text
            |> splitLines
            |> List.filter (not << String.isEmpty)
            |> List.filter filterIgnorableLines
            |> List.map readLine
            |> Result.Extra.combine
            |> Result.andThen failIfNoResults
            |> Result.map StopwatchData


defaultMaxNearMatchDistance : Int
defaultMaxNearMatchDistance =
    1


createInitialTableRow : Int -> MergeEntry -> MergedTableRow
createInitialTableRow index entry =
    MergedTableRow index (Just (index + 1)) entry True noUnderlines


generateInitialTable : List MergeEntry -> List MergedTableRow
generateInitialTable entries =
    List.indexedMap createInitialTableRow entries


addItemToSummary : MergedTableRow -> StopwatchMatchSummary -> StopwatchMatchSummary
addItemToSummary tableRow summary =
    case tableRow.entry of
        ExactMatch _ ->
            { summary | exactMatches = summary.exactMatches + 1 }

        NearMatch _ _ ->
            { summary | nearMatches = summary.nearMatches + 1 }

        NotNearMatch _ _ ->
            { summary | notNearMatches = summary.notNearMatches + 1 }

        OneWatchOnly StopwatchOne _ ->
            { summary | stopwatch1Only = summary.stopwatch1Only + 1 }

        OneWatchOnly StopwatchTwo _ ->
            { summary | stopwatch2Only = summary.stopwatch2Only + 1 }


generateMatchSummary : List MergedTableRow -> StopwatchMatchSummary
generateMatchSummary mergedTableRows =
    List.foldr addItemToSummary (StopwatchMatchSummary 0 0 0 0 0) mergedTableRows


isSingleTimeEntry : MergeEntry -> Bool
isSingleTimeEntry entry =
    case entry of
        ExactMatch _ ->
            False

        NearMatch _ _ ->
            False

        NotNearMatch _ _ ->
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
                    if firstRow.index == toggledIndex && isSingleTimeEntry firstRow.entry then
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

        NotNearMatch time1 time2 ->
            { row | entry = NotNearMatch time2 time1 }

        OneWatchOnly watch time ->
            { row | entry = OneWatchOnly (flipStopwatch watch) time }


flipTable : List MergedTableRow -> List MergedTableRow
flipTable =
    List.map flipRow


flipMatchSummary : StopwatchMatchSummary -> StopwatchMatchSummary
flipMatchSummary matchSummary =
    { matchSummary
        | stopwatch1Only = matchSummary.stopwatch2Only
        , stopwatch2Only = matchSummary.stopwatch1Only
    }



-- Each dictionary maps an index within the stopwatch-1, stopwatch-2 and
-- finish-tokens positions to the number-checker entry ID associated.


type alias NumberDicts =
    { stopwatch1 : Dict Int Int
    , stopwatch2 : Dict Int Int
    , finishTokens : Dict Int Int
    , actual : Dict Int Int
    }


getNextEntry : Int -> Int -> Dict Int Int -> Maybe Int
getNextEntry currentPosn nextPosn numberDict =
    if nextPosn > currentPosn then
        Dict.get nextPosn numberDict

    else
        Nothing


underlineTableInternal : NumberDicts -> Int -> Int -> Int -> Int -> List MergedTableRow -> List MergedTableRow
underlineTableInternal numberDicts sw1Posn sw2Posn ftoksPosn actualPosn mergedRows =
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

                        NotNearMatch _ _ ->
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

                        NotNearMatch _ _ ->
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

                nextActualPosn : Int
                nextActualPosn =
                    if firstRow.included then
                        actualPosn + 1

                    else
                        actualPosn

                newUnderlines : Underlines
                newUnderlines =
                    { stopwatch1 = getNextEntry sw1Posn nextSw1Posn numberDicts.stopwatch1
                    , stopwatch2 = getNextEntry sw2Posn nextSw2Posn numberDicts.stopwatch2
                    , position = getNextEntry ftoksPosn nextFtoksPosn numberDicts.finishTokens
                    , actual = getNextEntry actualPosn nextActualPosn numberDicts.actual
                    }

                underlinedFirstRow : MergedTableRow
                underlinedFirstRow =
                    { firstRow | underlines = newUnderlines }

                underlinedRestRows : List MergedTableRow
                underlinedRestRows =
                    underlineTableInternal numberDicts nextSw1Posn nextSw2Posn nextFtoksPosn nextActualPosn restRows
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

        actualNumbers : Dict Int Int
        actualNumbers =
            numberCheckerEntries
                |> List.map .actual
                |> createMappingDict
    in
    underlineTableInternal (NumberDicts stopwatch1Numbers stopwatch2Numbers finishTokensNumbers actualNumbers) 0 0 0 0 mergedRows


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


outputSingleStopwatchData : List Int -> String
outputSingleStopwatchData times =
    let
        formatTime : Int -> Int -> Maybe String
        formatTime index time =
            formatRow (index + 1) time
    in
    header
        ++ List.filterMap identity (List.indexedMap formatTime times)
        ++ [ footer ]
        |> String.join crlf


outputMergedRow : MergedTableRow -> Maybe String
outputMergedRow row =
    case row.rowNumber of
        Just rowNum ->
            case row.entry of
                ExactMatch time ->
                    formatRow rowNum time

                NearMatch time1 time2 ->
                    formatRow rowNum (min time1 time2)

                NotNearMatch time1 time2 ->
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


createMergedTable : List Int -> List Int -> String -> String -> DoubleStopwatchData
createMergedTable times1 times2 filename1 filename2 =
    let
        mergedDetails : List MergeEntry
        mergedDetails =
            merge defaultMaxNearMatchDistance times1 times2

        mergedTable : List MergedTableRow
        mergedTable =
            generateInitialTable mergedDetails

        matchSummary : StopwatchMatchSummary
        matchSummary =
            generateMatchSummary mergedTable
    in
    { times1 = times1
    , times2 = times2
    , filename1 = filename1
    , filename2 = filename2
    , mergedTableRows = mergedTable
    , matchSummary = matchSummary
    }


isHeadInRange : List Int -> Int -> Int -> Bool
isHeadInRange list rangeMin rangeMax =
    case List.head list of
        Just someNumber ->
            rangeMin <= someNumber && someNumber <= rangeMax

        Nothing ->
            False


addNotNearMatches : List MergeEntry -> List MergeEntry
addNotNearMatches entries =
    let
        entriesWithHeadMerged : List MergeEntry
        entriesWithHeadMerged =
            case entries of
                (OneWatchOnly StopwatchOne time1) :: (OneWatchOnly StopwatchTwo time2) :: entry3 :: rest ->
                    if not (isSingleTimeEntry entry3) then
                        NotNearMatch time1 time2 :: entry3 :: rest

                    else
                        entries

                (OneWatchOnly StopwatchTwo time2) :: (OneWatchOnly StopwatchOne time1) :: entry3 :: rest ->
                    if not (isSingleTimeEntry entry3) then
                        NotNearMatch time1 time2 :: entry3 :: rest

                    else
                        entries

                _ ->
                    entries

        createNotNearMatchesInRest : List MergeEntry -> List MergeEntry
        createNotNearMatchesInRest mergeEntries =
            case mergeEntries of
                [] ->
                    []

                entry1 :: rest ->
                    if isSingleTimeEntry entry1 then
                        entry1 :: createNotNearMatchesInRest rest

                    else
                        let
                            tail : List MergeEntry
                            tail =
                                case rest of
                                    (OneWatchOnly StopwatchOne time1) :: (OneWatchOnly StopwatchTwo time2) :: [] ->
                                        NotNearMatch time1 time2 :: []

                                    (OneWatchOnly StopwatchOne time1) :: (OneWatchOnly StopwatchTwo time2) :: entry4 :: restAfterMismatch ->
                                        if isSingleTimeEntry entry4 then
                                            createNotNearMatchesInRest rest

                                        else
                                            NotNearMatch time1 time2 :: createNotNearMatchesInRest (entry4 :: restAfterMismatch)

                                    (OneWatchOnly StopwatchTwo time2) :: (OneWatchOnly StopwatchOne time1) :: [] ->
                                        NotNearMatch time1 time2 :: []

                                    (OneWatchOnly StopwatchTwo time2) :: (OneWatchOnly StopwatchOne time1) :: entry4 :: restAfterMismatch ->
                                        if isSingleTimeEntry entry4 then
                                            createNotNearMatchesInRest rest

                                        else
                                            NotNearMatch time1 time2 :: createNotNearMatchesInRest (entry4 :: restAfterMismatch)

                                    _ ->
                                        createNotNearMatchesInRest rest
                        in
                        entry1 :: tail
    in
    createNotNearMatchesInRest entriesWithHeadMerged


merge : Int -> List Int -> List Int -> List MergeEntry
merge maxNearMatchDistance times1 times2 =
    let
        createTimes : List Int -> List Int -> List MergeEntry
        createTimes sortedTimes1 sortedTimes2 =
            case ( sortedTimes1, sortedTimes2 ) of
                ( [], [] ) ->
                    []

                ( _, [] ) ->
                    List.map (OneWatchOnly StopwatchOne) sortedTimes1

                ( [], _ ) ->
                    List.map (OneWatchOnly StopwatchTwo) sortedTimes2

                ( first1 :: rest1, first2 :: rest2 ) ->
                    if first1 < first2 - maxNearMatchDistance then
                        OneWatchOnly StopwatchOne first1 :: createTimes rest1 sortedTimes2

                    else if first2 - maxNearMatchDistance <= first1 && first1 < first2 then
                        if
                            isHeadInRange rest1 (first1 + 1) first2
                                && not (isHeadInRange rest2 first2 (first2 + maxNearMatchDistance))
                        then
                            -- The times match within the interval but there's a nearer time next
                            -- on stopwatch 1 and the next time on stopwatch 2 isn't particularly near.
                            -- So it's likely that this time is on watch 1 only and the time on watch 2
                            -- will match a nearer time on watch 1.
                            OneWatchOnly StopwatchOne first1 :: createTimes rest1 sortedTimes2

                        else
                            NearMatch first1 first2 :: createTimes rest1 rest2

                    else if first1 == first2 then
                        ExactMatch first1 :: createTimes rest1 rest2

                    else if first2 < first1 && first1 <= first2 + maxNearMatchDistance then
                        if
                            isHeadInRange rest2 (first2 + 1) first1
                                && not (isHeadInRange rest1 first1 (first1 + maxNearMatchDistance))
                        then
                            OneWatchOnly StopwatchTwo first2 :: createTimes sortedTimes1 rest2

                        else
                            NearMatch first1 first2 :: createTimes rest1 rest2

                    else
                        -- first1 > first2 + maxNearMatchDistance
                        OneWatchOnly StopwatchTwo first2 :: createTimes sortedTimes1 rest2
    in
    createTimes (List.sort times1) (List.sort times2)
        |> addNotNearMatches
