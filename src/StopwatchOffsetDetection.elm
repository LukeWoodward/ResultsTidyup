module StopwatchOffsetDetection exposing (findPossibleOffsets, getStopwatchTimeOffset)

import Array exposing (Array)
import Dict exposing (Dict)
import Stopwatch exposing (DoubleStopwatchData, Stopwatches(..), createMergedTable)


stopwatchTimeOffsetRange : Int
stopwatchTimeOffsetRange =
    10


{-| The minimum number of times a offset needs to appear in order for it to
be considered as a possible offset, as a proportion of the average number of
times on the two stopwatches.

This is used to help prevent spurious offsets being detected among unrelated
timer files, such as two files from two different events. In this situation
we would not expect to identify an offset between the timer files.

For timer files from the same event, the actual offset has been recorded at
least 60% of the time. For timer files from different events, offsets were
recorded no more than about 27% of the time.

-}
offsetCountThresholdRatio : Float
offsetCountThresholdRatio =
    0.5


addNextNumber : Int -> Dict Int Int -> Dict Int Int
addNextNumber number entries =
    let
        updater : Maybe Int -> Maybe Int
        updater oldValue =
            Maybe.withDefault 0 oldValue
                |> (+) 1
                |> Just
    in
    Dict.update number updater entries


{-| Find possible stopwatch time offsets. These are all of those that occur
at least the given number of times within the given list.
-}
findPossibleOffsets : Int -> List Int -> List Int
findPossibleOffsets minCountRequired numbers =
    List.foldr addNextNumber Dict.empty numbers
        |> Dict.filter (\_ count -> count >= minCountRequired)
        |> Dict.keys


scale : Int -> Int -> Int -> Int
scale index1 length1 length2 =
    round (toFloat index1 * (toFloat length2 - 1) / (toFloat length1 - 1) + 0.5)


getPossibleStopwatchTimeOffsetsInDoubleStopwatchData : DoubleStopwatchData -> List Int
getPossibleStopwatchTimeOffsetsInDoubleStopwatchData doubleStopwatchData =
    {- The implementation of this is somewhat basic: we compare corresponding
       times that are within a given number of positions away from each other.
       Adjustments are made if the stopwatches don't have the same number of
       times recorded.  All differences between pairs of times that occur
       enough times are then returned.
    -}
    let
        times1Array : Array Int
        times1Array =
            Array.fromList doubleStopwatchData.times1

        times2Array : Array Int
        times2Array =
            Array.fromList doubleStopwatchData.times2

        length1 : Int
        length1 =
            Array.length times1Array

        length2 : Int
        length2 =
            Array.length times2Array

        indexPairs : List ( Int, Int )
        indexPairs =
            if length1 > length2 then
                List.range 0 (length1 - 1)
                    |> List.map (\ix1 -> ( ix1, scale ix1 length1 length2 ))

            else if length2 > length1 then
                List.range 0 (length2 - 1)
                    |> List.map (\ix2 -> ( scale ix2 length2 length1, ix2 ))

            else
                List.range 0 (length1 - 1)
                    |> List.map (\ix -> ( ix, ix ))

        surround : ( Int, Int ) -> List ( Int, Int )
        surround ( ix1, ix2 ) =
            List.map (\offset -> ( ix1, ix2 + offset )) (List.range -stopwatchTimeOffsetRange stopwatchTimeOffsetRange)

        allIndexes : List ( Int, Int )
        allIndexes =
            List.map surround indexPairs
                |> List.concat

        diff : Maybe Int -> Maybe Int -> Maybe Int
        diff maybeTime1 maybeTime2 =
            case ( maybeTime1, maybeTime2 ) of
                ( Just time1, Just time2 ) ->
                    Just (time1 - time2)

                _ ->
                    Nothing

        getTimeDiff : ( Int, Int ) -> Maybe Int
        getTimeDiff ( ix1, ix2 ) =
            diff (Array.get ix1 times1Array) (Array.get ix2 times2Array)

        timeDifferences : List Int
        timeDifferences =
            List.filterMap getTimeDiff allIndexes

        minOffsetCountRequired : Int
        minOffsetCountRequired =
            round (toFloat (length1 + length2) * 0.5 * offsetCountThresholdRatio)
    in
    findPossibleOffsets minOffsetCountRequired timeDifferences


getBestStopwatchTimeOffsetInDoubleStopwatchData : DoubleStopwatchData -> Maybe Int
getBestStopwatchTimeOffsetInDoubleStopwatchData doubleStopwatchData =
    let
        possibleOffsets : List Int
        possibleOffsets =
            getPossibleStopwatchTimeOffsetsInDoubleStopwatchData doubleStopwatchData

        getMergedTableResult : Int -> Int
        getMergedTableResult offset =
            createMergedTable doubleStopwatchData.times1 (List.map ((+) offset) doubleStopwatchData.times2) "dummy-filename1" "fummy-filename2"
                |> .matchSummary
                |> .exactMatches

        allMergeResults : List ( Int, Int )
        allMergeResults =
            List.map (\offset -> ( offset, getMergedTableResult offset )) possibleOffsets
    in
    List.sortBy Tuple.second allMergeResults
        |> List.reverse
        |> List.head
        |> Maybe.map Tuple.first


getStopwatchTimeOffset : Stopwatches -> Maybe Int
getStopwatchTimeOffset stopwatches =
    case stopwatches of
        Double doubleStopwatchData ->
            getBestStopwatchTimeOffsetInDoubleStopwatchData doubleStopwatchData

        Single _ _ ->
            Nothing

        None ->
            Nothing
