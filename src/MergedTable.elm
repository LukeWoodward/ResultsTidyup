module MergedTable
    exposing
        ( MergedTableRow
        , Underlines
        , noUnderlines
        , generateInitialTable
        , toggleRowInTable
        , deleteStopwatchFromTable
        , flipTable
        , underlineTable
        )

import DataStructures exposing (WhichStopwatch(..))
import Merger exposing (MergeEntry(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import Set exposing (Set)


type alias Underlines =
    { position : Bool
    , stopwatch1 : Bool
    , stopwatch2 : Bool
    }


noUnderlines : Underlines
noUnderlines =
    Underlines False False False


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


flipRow : MergedTableRow -> MergedTableRow
flipRow row =
    case row.entry of
        ExactMatch _ ->
            row

        NearMatch time1 time2 ->
            { row | entry = NearMatch time2 time1 }

        Watch1Only time ->
            { row | entry = Watch2Only time }

        Watch2Only time ->
            { row | entry = Watch1Only time }


flipTable : List MergedTableRow -> List MergedTableRow
flipTable =
    List.map flipRow


type alias NumberSets =
    { stopwatch1 : Set Int
    , stopwatch2 : Set Int
    , finishTokens : Set Int
    }


underlineTableInternal : NumberSets -> Int -> Int -> Int -> List MergedTableRow -> List MergedTableRow
underlineTableInternal numberSets sw1Posn sw2Posn ftoksPosn mergedRows =
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

                        Watch1Only _ ->
                            sw1Posn + 1

                        Watch2Only _ ->
                            sw1Posn

                nextSw2Posn : Int
                nextSw2Posn =
                    case firstRow.entry of
                        ExactMatch _ ->
                            sw2Posn + 1

                        NearMatch _ _ ->
                            sw2Posn + 1

                        Watch1Only _ ->
                            sw2Posn

                        Watch2Only _ ->
                            sw2Posn + 1

                nextFtoksPosn : Int
                nextFtoksPosn =
                    if firstRow.included then
                        ftoksPosn + 1
                    else
                        ftoksPosn

                newUnderlines : Underlines
                newUnderlines =
                    { stopwatch1 = nextSw1Posn > sw1Posn && Set.member nextSw1Posn numberSets.stopwatch1
                    , stopwatch2 = nextSw2Posn > sw2Posn && Set.member nextSw2Posn numberSets.stopwatch2
                    , position = nextFtoksPosn > ftoksPosn && Set.member nextFtoksPosn numberSets.finishTokens
                    }

                underlinedFirstRow : MergedTableRow
                underlinedFirstRow =
                    { firstRow | underlines = newUnderlines }

                underlinedRestRows : List MergedTableRow
                underlinedRestRows =
                    underlineTableInternal numberSets nextSw1Posn nextSw2Posn nextFtoksPosn restRows
            in
                underlinedFirstRow :: underlinedRestRows


underlineTable : List AnnotatedNumberCheckerEntry -> List MergedTableRow -> List MergedTableRow
underlineTable numberCheckerEntries mergedRows =
    let
        stopwatch1Numbers : Set Int
        stopwatch1Numbers =
            numberCheckerEntries
                |> List.map .stopwatch1
                |> Set.fromList

        stopwatch2Numbers : Set Int
        stopwatch2Numbers =
            numberCheckerEntries
                |> List.map .stopwatch2
                |> Set.fromList

        finishTokensNumbers : Set Int
        finishTokensNumbers =
            numberCheckerEntries
                |> List.map .finishTokens
                |> Set.fromList
    in
        underlineTableInternal (NumberSets stopwatch1Numbers stopwatch2Numbers finishTokensNumbers) 0 0 0 mergedRows
