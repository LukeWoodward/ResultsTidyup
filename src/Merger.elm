module Merger exposing (MergeEntry(..), merge)

import Set exposing (Set)
import DataStructures exposing (WhichStopwatch(..))


type MergeEntry
    = ExactMatch Int
    | NearMatch Int Int
    | OneWatchOnly WhichStopwatch Int


merge : Int -> List Int -> List Int -> List MergeEntry
merge maxNearMatchDistance times1 times2 =
    let
        times1AsSet : Set Int
        times1AsSet =
            Set.fromList times1

        times2AsSet : Set Int
        times2AsSet =
            Set.fromList times2

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
                        OneWatchOnly StopwatchOne first1 :: (createTimes rest1 sortedTimes2)
                    else if first2 - maxNearMatchDistance <= first1 && first1 < first2 then
                        if
                            List.any (\n -> Set.member n times1AsSet) (List.range (first1 + 1) first2)
                                && not (List.any (\n -> Set.member n times2AsSet) (List.range (first2 + 1) (first2 + maxNearMatchDistance)))
                        then
                            -- The times match within the interval but there's a nearer time next
                            -- on stopwatch 1 and the next time on stopwatch 2 isn't particularly near.
                            -- So it's likely that this time is on watch 1 only and the time on watch 2
                            -- will match a nearer time on watch 1.
                            OneWatchOnly StopwatchOne first1 :: (createTimes rest1 sortedTimes2)
                        else
                            NearMatch first1 first2 :: (createTimes rest1 rest2)
                    else if first1 == first2 then
                        ExactMatch first1 :: (createTimes rest1 rest2)
                    else if first2 < first1 && first1 <= first2 + maxNearMatchDistance then
                        if
                            List.any (\n -> Set.member n times2AsSet) (List.range (first2 + 1) first1)
                                && not (List.any (\n -> Set.member n times1AsSet) (List.range (first1 + 1) (first1 + maxNearMatchDistance)))
                        then
                            OneWatchOnly StopwatchTwo first2 :: (createTimes sortedTimes1 rest2)
                        else
                            NearMatch first1 first2 :: (createTimes rest1 rest2)
                    else
                        -- first1 > first2 + maxNearMatchDistance
                        OneWatchOnly StopwatchTwo first2 :: (createTimes sortedTimes1 rest2)
    in
        createTimes (List.sort times1) (List.sort times2)
