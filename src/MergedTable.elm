module MergedTable exposing (MergedTableRow, generateInitialTable, toggleRowInTable)

import Merger exposing (MergeEntry(..))


type alias MergedTableRow =
    { index: Int
    , rowNumber: Maybe Int
    , entry: MergeEntry
    , included: Bool
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
                    if newRow.included then 1 else 0
                   
                newRowNumber : Maybe Int
                newRowNumber =
                    if newRow.included then Just currentRowNumber else Nothing
                    
                remainingRows : List MergedTableRow
                remainingRows =
                    toggleRowInTableInternal toggledIndex (currentRowNumber + rowNumberStep) rest
            in
                { newRow | rowNumber = newRowNumber } :: remainingRows
            
    
toggleRowInTable : Int -> List MergedTableRow -> List MergedTableRow
toggleRowInTable index rows =
    toggleRowInTableInternal index 1 rows
