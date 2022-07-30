module Timer exposing
    ( DoubleTimerData
    , MergeEntry(..)
    , MergedTableRow
    , Timer(..)
    , TimerMatchSummary
    , Timers(..)
    , WhichTimer(..)
    , createMergedTable
    , flipMatchSummary
    , flipTable
    , footer
    , generateInitialTable
    , header
    , merge
    , noUnderlines
    , outputMergedTable
    , outputSingleTimerData
    , readTimerData
    , toggleRowInTable
    , underlineTable
    )

import Bootstrap.Form exposing (row)
import Dict exposing (Dict)
import Error exposing (Error)
import FileHandling exposing (crlf, isPossibleBinary, splitLines)
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import Result.Extra
import TimeHandling exposing (formatTimeWithHours, parseTime)



{- Timer data is basically a list of integer numbers of seconds -}


type Timer
    = TimerData (List Int)


type WhichTimer
    = TimerOne
    | TimerTwo


type MergeEntry
    = ExactMatch Int
    | NearMatch Int Int
    | NotNearMatch Int Int
    | OneWatchOnly WhichTimer Int


type alias TimerMatchSummary =
    { exactMatches : Int
    , nearMatches : Int
    , notNearMatches : Int
    , timer1Only : Int
    , timer2Only : Int
    }


type alias DoubleTimerData =
    { times1 : List Int
    , times2 : List Int
    , filename1 : String
    , filename2 : String
    , mergedTableRows : List MergedTableRow
    , matchSummary : TimerMatchSummary
    }


type Timers
    = None
    | Single String (List Int)
    | Double DoubleTimerData


type alias Underlines =
    { position : Maybe Int
    , timer1 : Maybe Int
    , timer2 : Maybe Int
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
    , "I, CP"
    , "S, 001"
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

        [ "T", _, _, time ] ->
            parseTime time

        _ ->
            Error "NOT_THREE_PARTS" ("Line " ++ line ++ " does not contain the expected three comma-separated parts")
                |> Err


failIfNoResults : List Int -> Result Error (List Int)
failIfNoResults results =
    if List.isEmpty results then
        Error "NO_RESULTS" "Timer data contained no results"
            |> Err

    else
        Ok results


readTimerData : String -> Result Error Timer
readTimerData text =
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
            |> Result.map TimerData


defaultMaxNearMatchDistance : Int
defaultMaxNearMatchDistance =
    1


defaultMaxNotNearMatchDistance : Int
defaultMaxNotNearMatchDistance =
    25


createInitialTableRow : Int -> MergeEntry -> MergedTableRow
createInitialTableRow index entry =
    MergedTableRow index (Just (index + 1)) entry True noUnderlines


generateInitialTable : List MergeEntry -> List MergedTableRow
generateInitialTable entries =
    List.indexedMap createInitialTableRow entries


addItemToSummary : MergedTableRow -> TimerMatchSummary -> TimerMatchSummary
addItemToSummary tableRow summary =
    case tableRow.entry of
        ExactMatch _ ->
            { summary | exactMatches = summary.exactMatches + 1 }

        NearMatch _ _ ->
            { summary | nearMatches = summary.nearMatches + 1 }

        NotNearMatch _ _ ->
            { summary | notNearMatches = summary.notNearMatches + 1 }

        OneWatchOnly TimerOne _ ->
            { summary | timer1Only = summary.timer1Only + 1 }

        OneWatchOnly TimerTwo _ ->
            { summary | timer2Only = summary.timer2Only + 1 }


generateMatchSummary : List MergedTableRow -> TimerMatchSummary
generateMatchSummary mergedTableRows =
    List.foldr addItemToSummary (TimerMatchSummary 0 0 0 0 0) mergedTableRows


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


flipTimer : WhichTimer -> WhichTimer
flipTimer timer =
    case timer of
        TimerOne ->
            TimerTwo

        TimerTwo ->
            TimerOne


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
            { row | entry = OneWatchOnly (flipTimer watch) time }


flipTable : List MergedTableRow -> List MergedTableRow
flipTable =
    List.map flipRow


flipMatchSummary : TimerMatchSummary -> TimerMatchSummary
flipMatchSummary matchSummary =
    { matchSummary
        | timer1Only = matchSummary.timer2Only
        , timer2Only = matchSummary.timer1Only
    }



-- Each dictionary maps an index within the timer-1, timer-2 and
-- finish-tokens positions to the number-checker entry ID associated.


type alias NumberDicts =
    { timer1 : Dict Int Int
    , timer2 : Dict Int Int
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

                        OneWatchOnly TimerOne _ ->
                            sw1Posn + 1

                        OneWatchOnly TimerTwo _ ->
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

                        OneWatchOnly TimerOne _ ->
                            sw2Posn

                        OneWatchOnly TimerTwo _ ->
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
                    { timer1 = getNextEntry sw1Posn nextSw1Posn numberDicts.timer1
                    , timer2 = getNextEntry sw2Posn nextSw2Posn numberDicts.timer2
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
        timer1Numbers : Dict Int Int
        timer1Numbers =
            numberCheckerEntries
                |> List.map .timer1
                |> createMappingDict

        timer2Numbers : Dict Int Int
        timer2Numbers =
            numberCheckerEntries
                |> List.map .timer2
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
    underlineTableInternal (NumberDicts timer1Numbers timer2Numbers finishTokensNumbers actualNumbers) 0 0 0 0 mergedRows


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


outputSingleTimerData : List Int -> String
outputSingleTimerData times =
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
    row.rowNumber
        |> Maybe.andThen
            (\rowNum ->
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
            )


outputMergedTable : List MergedTableRow -> String
outputMergedTable mergedRows =
    header
        ++ List.filterMap outputMergedRow mergedRows
        ++ [ footer ]
        |> String.join crlf


createMergedTable : List Int -> List Int -> String -> String -> DoubleTimerData
createMergedTable times1 times2 filename1 filename2 =
    let
        mergedDetails : List MergeEntry
        mergedDetails =
            merge defaultMaxNearMatchDistance defaultMaxNotNearMatchDistance times1 times2

        mergedTable : List MergedTableRow
        mergedTable =
            generateInitialTable mergedDetails

        matchSummary : TimerMatchSummary
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


addNotNearMatches : Int -> List MergeEntry -> List MergeEntry
addNotNearMatches maxNotNearMatchDistance entries =
    case entries of
        [] ->
            []

        (OneWatchOnly TimerOne time1) :: (OneWatchOnly TimerTwo time2) :: rest ->
            if abs (time1 - time2) <= maxNotNearMatchDistance then
                NotNearMatch time1 time2 :: addNotNearMatches maxNotNearMatchDistance rest

            else
                OneWatchOnly TimerOne time1 :: addNotNearMatches maxNotNearMatchDistance (OneWatchOnly TimerTwo time2 :: rest)

        (OneWatchOnly TimerTwo time2) :: (OneWatchOnly TimerOne time1) :: rest ->
            if abs (time1 - time2) <= maxNotNearMatchDistance then
                NotNearMatch time1 time2 :: addNotNearMatches maxNotNearMatchDistance rest

            else
                OneWatchOnly TimerTwo time2 :: addNotNearMatches maxNotNearMatchDistance (OneWatchOnly TimerOne time1 :: rest)

        first :: rest ->
            first :: addNotNearMatches maxNotNearMatchDistance rest


merge : Int -> Int -> List Int -> List Int -> List MergeEntry
merge maxNearMatchDistance maxNotNearMatchDistance times1 times2 =
    let
        createTimes : List Int -> List Int -> List MergeEntry
        createTimes sortedTimes1 sortedTimes2 =
            case ( sortedTimes1, sortedTimes2 ) of
                ( [], [] ) ->
                    []

                ( _, [] ) ->
                    List.map (OneWatchOnly TimerOne) sortedTimes1

                ( [], _ ) ->
                    List.map (OneWatchOnly TimerTwo) sortedTimes2

                ( first1 :: rest1, first2 :: rest2 ) ->
                    if first1 < first2 - maxNearMatchDistance then
                        OneWatchOnly TimerOne first1 :: createTimes rest1 sortedTimes2

                    else if first2 - maxNearMatchDistance <= first1 && first1 < first2 then
                        if
                            isHeadInRange rest1 (first1 + 1) first2
                                && not (isHeadInRange rest2 first2 (first2 + maxNearMatchDistance))
                        then
                            -- The times match within the interval but there's a nearer time next
                            -- on timer 1 and the next time on timer 2 isn't particularly near.
                            -- So it's likely that this time is on watch 1 only and the time on watch 2
                            -- will match a nearer time on watch 1.
                            OneWatchOnly TimerOne first1 :: createTimes rest1 sortedTimes2

                        else
                            NearMatch first1 first2 :: createTimes rest1 rest2

                    else if first1 == first2 then
                        ExactMatch first1 :: createTimes rest1 rest2

                    else if first2 < first1 && first1 <= first2 + maxNearMatchDistance then
                        if
                            isHeadInRange rest2 (first2 + 1) first1
                                && not (isHeadInRange rest1 first1 (first1 + maxNearMatchDistance))
                        then
                            OneWatchOnly TimerTwo first2 :: createTimes sortedTimes1 rest2

                        else
                            NearMatch first1 first2 :: createTimes rest1 rest2

                    else
                        -- first1 > first2 + maxNearMatchDistance
                        OneWatchOnly TimerTwo first2 :: createTimes sortedTimes1 rest2
    in
    createTimes (List.sort times1) (List.sort times2)
        |> addNotNearMatches maxNotNearMatchDistance
