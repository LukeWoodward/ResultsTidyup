module StopwatchOffsetDetection exposing (findMostCommonNumber, getStopwatchTimeOffset)

import Array exposing (Array)
import Dict exposing (Dict)
import Stopwatch exposing (DoubleStopwatchData, Stopwatches(..))


stopwatchTimeOffsetRange : Int
stopwatchTimeOffsetRange =
    10


minOffsetCountRequired : Int
minOffsetCountRequired =
    3


type alias MostCommonStatus =
    { entries : Dict Int Int
    , mostFrequent : Maybe Int
    , mostFrequentCount : Int
    }


addNextNumber : Int -> MostCommonStatus -> MostCommonStatus
addNextNumber number mostCommonStatus =
    let
        oldValue : Int
        oldValue =
            Dict.get number mostCommonStatus.entries
                |> Maybe.withDefault 0

        newValue : Int
        newValue =
            oldValue + 1

        newEntries : Dict Int Int
        newEntries =
            Dict.insert number newValue mostCommonStatus.entries
    in
    if newValue > mostCommonStatus.mostFrequentCount then
        { entries = newEntries, mostFrequent = Just number, mostFrequentCount = newValue }

    else
        { mostCommonStatus | entries = newEntries }


{-| Finds the most common number in a list, provided that that number occurs
at least a specified minimum number of times. Returns Nothing if the source
list is empty or doesn't contain any number repeated at least as many times
as the minimum.
-}
findMostCommonNumber : List Int -> Maybe Int
findMostCommonNumber numbers =
    let
        mostCommonStatus : MostCommonStatus
        mostCommonStatus =
            List.foldr addNextNumber (MostCommonStatus Dict.empty Nothing 0) numbers
    in
    if mostCommonStatus.mostFrequentCount >= minOffsetCountRequired then
        mostCommonStatus.mostFrequent

    else
        Nothing


scale : Int -> Int -> Int -> Int
scale index1 length1 length2 =
    round (toFloat index1 * (toFloat length2 - 1) / (toFloat length1 - 1) + 0.5)


getStopwatchTimeOffsetInDoubleStopwatchData : DoubleStopwatchData -> Int
getStopwatchTimeOffsetInDoubleStopwatchData doubleStopwatchData =
    {- The implementation of this is somewhat basic: we compare corresponding
       times that are within a given number of positions away from each other.
       Adjustments are made if the stopwatches don't have the same number of
       times recorded.  The most common difference between pairs of times
       is then taken as the estimated offset.
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
    in
    findMostCommonNumber timeDifferences
        |> Maybe.withDefault 0


getStopwatchTimeOffset : Stopwatches -> Int
getStopwatchTimeOffset stopwatches =
    case stopwatches of
        Double doubleStopwatchData ->
            getStopwatchTimeOffsetInDoubleStopwatchData doubleStopwatchData

        Single _ _ ->
            0

        None ->
            0
