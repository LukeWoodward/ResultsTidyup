module MergedTable exposing (MergedTableRow, generateInitialTable, toggleRowInTable, deleteStopwatchFromTable)

import DataStructures exposing (WhichStopwatch(..))
import Merger exposing (MergeEntry(..))


type alias MergedTableRow =
    { index : Int
    , rowNumber : Maybe Int
    , entry : MergeEntry
    , included : Bool
    }


createInitialTableRow : Int -> MergeEntry -> MergedTableRow
createInitialTableRow index entry =
    MergedTableRow index (Just (index + 1)) entry True


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

        Watch1Only _ ->
            True

        Watch2Only _ ->
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


{-| Deletes a time from a row and returns the remaining time, if there is
still a remaining time.
-}
deleteTimeFromRow : WhichStopwatch -> MergedTableRow -> Maybe Int
deleteTimeFromRow which row =
    case ( row.entry, which ) of
        ( ExactMatch time, _ ) ->
            Just time

        ( NearMatch _ time2, StopwatchOne ) ->
            Just time2

        ( NearMatch time1 _, StopwatchTwo ) ->
            Just time1

        ( Watch1Only _, StopwatchOne ) ->
            Nothing

        ( Watch1Only time1, StopwatchTwo ) ->
            Just time1

        ( Watch2Only time2, StopwatchOne ) ->
            Just time2

        ( Watch2Only _, StopwatchTwo ) ->
            Nothing


deleteStopwatchFromTable : WhichStopwatch -> List MergedTableRow -> List Int
deleteStopwatchFromTable whichToDelete mergedRows =
    List.filterMap (deleteTimeFromRow whichToDelete) mergedRows
