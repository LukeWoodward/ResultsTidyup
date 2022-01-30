module Problems exposing
    ( AthleteAndPositionPair
    , AthleteWithMultiplePositionsProblem
    , BarcodesScannedBeforeEventStartProblem
    , IgnoredProblems
    , PositionAndTime
    , PositionOffEndOfTimesProblem
    , PositionWithMultipleAthletesProblem
    , Problems
    , identifyProblems
    , noIgnoredProblems
    , noProblems
    )

import Array exposing (Array)
import BarcodeScanner exposing (BarcodeScannerData, BarcodeScannerFile, BarcodeScannerFileLine, DeletionStatus(..), LineContents(..), MisScannedItem, UnrecognisedLine)
import DateHandling exposing (dateTimeStringToPosix, posixToDateString)
import Dict exposing (Dict)
import EventDateAndTime exposing (EventDateAndTime)
import Set exposing (Set)
import Time exposing (posixToMillis)
import Timer exposing (MergeEntry(..), MergedTableRow, Timers(..))
import TimerOffsetDetection exposing (getTimerTimeOffset)


type alias BarcodesScannedBeforeEventStartProblem =
    { numberOfScansBeforeEventStart : Int
    , eventStartDateTimeMillis : Int
    , eventStartTime : String
    }


type alias AthleteAndPositionPair =
    { athlete : String
    , position : Int
    }


type alias PositionAndTime =
    { position : Int
    , time : Maybe Int
    }


type alias AthleteWithMultiplePositionsProblem =
    { athlete : String
    , positionsAndTimes : List PositionAndTime
    }


type alias PositionWithMultipleAthletesProblem =
    { position : Int
    , athletes : List String
    }


type alias PositionOffEndOfTimesProblem =
    { timerTimeCount : Int
    , maxPosition : Int
    }


type alias Problems =
    { barcodesScannedBeforeEventStart : Maybe BarcodesScannedBeforeEventStartProblem
    , athletesInSamePositionMultipleTimes : List AthleteAndPositionPair
    , athletesWithAndWithoutPosition : List AthleteAndPositionPair
    , positionsWithAndWithoutAthlete : List AthleteAndPositionPair
    , timerTimeOffset : Maybe Int
    , athletesWithMultiplePositions : List AthleteWithMultiplePositionsProblem
    , positionsWithMultipleAthletes : List PositionWithMultipleAthletesProblem
    , positionOffEndOfTimes : Maybe PositionOffEndOfTimesProblem
    , athletesMissingPosition : List String
    , positionsMissingAthlete : List Int
    , misScans : List String
    , unrecognisedBarcodeScannerLines : List String
    , identicalTimerTimes : Bool
    , timersInconsistentWithNumberChecker : Bool
    , timersAndFinishTokensInconsistentWithNumberChecker : Bool
    }


noProblems : Problems
noProblems =
    { barcodesScannedBeforeEventStart = Nothing
    , athletesInSamePositionMultipleTimes = []
    , athletesWithAndWithoutPosition = []
    , positionsWithAndWithoutAthlete = []
    , timerTimeOffset = Nothing
    , athletesWithMultiplePositions = []
    , positionsWithMultipleAthletes = []
    , positionOffEndOfTimes = Nothing
    , athletesMissingPosition = []
    , positionsMissingAthlete = []
    , misScans = []
    , unrecognisedBarcodeScannerLines = []
    , identicalTimerTimes = False
    , timersInconsistentWithNumberChecker = False
    , timersAndFinishTokensInconsistentWithNumberChecker = False
    }


type alias IgnoredProblems =
    { ignoreTimerTimeOffsets : Bool }


noIgnoredProblems : IgnoredProblems
noIgnoredProblems =
    { ignoreTimerTimeOffsets = False }


flattenItem : ( Int, List String ) -> List ( Int, String )
flattenItem ( position, athletes ) =
    List.map (\athlete -> ( position, athlete )) athletes


flattenDict : Dict Int (List String) -> List ( Int, String )
flattenDict inDict =
    Dict.toList inDict
        |> List.map flattenItem
        |> List.concat


getAthleteToPositionsDict : Dict Int (List String) -> Dict String (List Int)
getAthleteToPositionsDict positionToAthletesDict =
    let
        flattenedDict : List ( Int, String )
        flattenedDict =
            flattenDict positionToAthletesDict

        updater : Int -> Maybe (List Int) -> Maybe (List Int)
        updater position currentValue =
            currentValue
                |> Maybe.withDefault []
                |> List.append [ position ]
                |> Just

        mergeItem : ( Int, String ) -> Dict String (List Int) -> Dict String (List Int)
        mergeItem ( position, athlete ) dict =
            Dict.update athlete (updater position) dict
    in
    List.foldr mergeItem Dict.empty flattenedDict


hasDifferentValues : List comparable -> Maybe ( comparable, comparable )
hasDifferentValues list =
    case list of
        first :: second :: rest ->
            if first /= second then
                Just ( first, second )

            else
                hasDifferentValues (second :: rest)

        _ ->
            Nothing


isExactMatch : MergedTableRow -> Bool
isExactMatch row =
    case row.entry of
        ExactMatch _ ->
            True

        _ ->
            False


identifyIdenticalTimers : Timers -> Bool
identifyIdenticalTimers timers =
    case timers of
        None ->
            False

        Single _ _ ->
            False

        Double doubleTimerData ->
            List.all isExactMatch doubleTimerData.mergedTableRows


identifyAthletesWithMultiplePositions : Array Int -> Dict String (List Int) -> List AthleteWithMultiplePositionsProblem
identifyAthletesWithMultiplePositions times athleteToPositionsDict =
    let
        identifier : ( String, List Int ) -> Maybe AthleteWithMultiplePositionsProblem
        identifier ( athlete, positions ) =
            let
                dedupedPositions : List Int
                dedupedPositions =
                    deduplicate positions

                lookupTime : Int -> PositionAndTime
                lookupTime position =
                    PositionAndTime position (Array.get position times)
            in
            if List.length dedupedPositions > 1 then
                Just (AthleteWithMultiplePositionsProblem athlete (List.map lookupTime dedupedPositions))

            else
                Nothing
    in
    Dict.toList athleteToPositionsDict
        |> List.filterMap identifier


identifyPositionsWithMultipleAthletes : Dict Int (List String) -> List PositionWithMultipleAthletesProblem
identifyPositionsWithMultipleAthletes positionToAthletesDict =
    let
        identifier : ( Int, List String ) -> Maybe PositionWithMultipleAthletesProblem
        identifier ( position, athletes ) =
            let
                dedupedAthletes : List String
                dedupedAthletes =
                    deduplicate athletes
            in
            if List.length dedupedAthletes > 1 then
                Just (PositionWithMultipleAthletesProblem position dedupedAthletes)

            else
                Nothing
    in
    Dict.toList positionToAthletesDict
        |> List.filterMap identifier


checkPositionOffEndOfTimes : Int -> Dict Int (List String) -> Maybe PositionOffEndOfTimesProblem
checkPositionOffEndOfTimes timerTimeCount positionToAthletesDict =
    Dict.keys positionToAthletesDict
        |> List.filter (\pos -> pos > timerTimeCount)
        |> List.maximum
        |> Maybe.map (PositionOffEndOfTimesProblem timerTimeCount)


identifyPositionsOffEndOfTimes : Timers -> Dict Int (List String) -> Maybe PositionOffEndOfTimesProblem
identifyPositionsOffEndOfTimes timers positionToAthletesDict =
    case timers of
        None ->
            -- Don't report this problem if there are no timers.
            Nothing

        Single _ times ->
            checkPositionOffEndOfTimes (List.length times) positionToAthletesDict

        Double doubleTimerData ->
            let
                timerTimeCount : Int
                timerTimeCount =
                    List.filterMap .rowNumber doubleTimerData.mergedTableRows
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            checkPositionOffEndOfTimes timerTimeCount positionToAthletesDict


deduplicate : List comparable -> List comparable
deduplicate list =
    Set.fromList list
        |> Set.toList


repeatedItems : List comparable -> List comparable
repeatedItems items =
    let
        insertItem : comparable -> ( Set comparable, Set comparable ) -> ( Set comparable, Set comparable )
        insertItem item ( itemsSoFar, repeatedItemsSoFar ) =
            if Set.member item itemsSoFar then
                ( itemsSoFar, Set.insert item repeatedItemsSoFar )

            else
                ( Set.insert item itemsSoFar, repeatedItemsSoFar )
    in
    List.foldr insertItem ( Set.empty, Set.empty ) items
        |> Tuple.second
        |> Set.toList


getSingleValue : List comparable -> Maybe comparable
getSingleValue list =
    case deduplicate list of
        [ singleValue ] ->
            Just singleValue

        _ ->
            Nothing


countScansBefore : Int -> BarcodeScannerFile -> Int
countScansBefore timeMillis file =
    let
        countDatesBefore : List String -> Int
        countDatesBefore dates =
            List.filterMap dateTimeStringToPosix dates
                |> List.map Time.posixToMillis
                |> List.filter (\t -> t < timeMillis)
                |> List.length
    in
    List.filter (\line -> line.deletionStatus == NotDeleted) file.lines
        |> List.map .scanDateTime
        |> countDatesBefore


identifyAthletesWithNoPositions : List String -> Dict String (List Int) -> List String
identifyAthletesWithNoPositions unpairedAthletes athleteToPositionsDict =
    deduplicate unpairedAthletes
        |> List.filter (\athlete -> not (Dict.member athlete athleteToPositionsDict))


identifyPositionsWithNoAthletes : List Int -> Dict Int (List String) -> List Int
identifyPositionsWithNoAthletes unpairedPositions positionToAthletesDict =
    deduplicate unpairedPositions
        |> List.filter (\position -> not (Dict.member position positionToAthletesDict))


identifyMisScannedItems : List MisScannedItem -> List String
identifyMisScannedItems misScannedItems =
    List.map .scannedText misScannedItems


identifyUnrecognisedBarcodeScannerLines : List UnrecognisedLine -> List String
identifyUnrecognisedBarcodeScannerLines unrecognisedLines =
    List.map .line unrecognisedLines


identifyDuplicateScans : Dict Int (List String) -> List AthleteAndPositionPair
identifyDuplicateScans positionToAthletesDict =
    let
        identifyDuplicates : ( Int, List String ) -> List AthleteAndPositionPair
        identifyDuplicates ( position, athletes ) =
            repeatedItems athletes
                |> List.map (\athlete -> AthleteAndPositionPair athlete position)
    in
    Dict.toList positionToAthletesDict
        |> List.filter (\( position, athletes ) -> List.length athletes > 1)
        |> List.concatMap identifyDuplicates


identifyAthletesWithAndWithoutPosition : Dict String (List Int) -> List String -> List AthleteAndPositionPair
identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly =
    let
        getProblemIfSinglePosition : String -> Maybe AthleteAndPositionPair
        getProblemIfSinglePosition athlete =
            Dict.get athlete athleteToPositionsDict
                |> Maybe.andThen getSingleValue
                |> Maybe.map (AthleteAndPositionPair athlete)
    in
    List.filterMap getProblemIfSinglePosition athleteBarcodesOnly


identifyPositionsWithAndWithoutAthlete : Dict Int (List String) -> List Int -> List AthleteAndPositionPair
identifyPositionsWithAndWithoutAthlete positionToAthletesDict finishTokensOnly =
    let
        getProblemIfSingleAthlete : Int -> Maybe AthleteAndPositionPair
        getProblemIfSingleAthlete position =
            Dict.get position positionToAthletesDict
                |> Maybe.andThen getSingleValue
                |> Maybe.map (\athlete -> AthleteAndPositionPair athlete position)
    in
    List.filterMap getProblemIfSingleAthlete finishTokensOnly


identifyRecordsScannedBeforeEventStartTime : BarcodeScannerData -> String -> Int -> Maybe BarcodesScannedBeforeEventStartProblem
identifyRecordsScannedBeforeEventStartTime barcodeScannerData eventStartTimeAsString eventStartDateTimeMillis =
    let
        totalNumberOfScansBeforeEventStart : Int
        totalNumberOfScansBeforeEventStart =
            List.map (countScansBefore eventStartDateTimeMillis) barcodeScannerData.files
                |> List.sum
    in
    if totalNumberOfScansBeforeEventStart == 0 then
        Nothing

    else
        Just (BarcodesScannedBeforeEventStartProblem totalNumberOfScansBeforeEventStart eventStartDateTimeMillis eventStartTimeAsString)


type RepeatedElementResult a
    = NoElements
    | NoRepeatedElement
    | RepeatedElement a


{-| If a list contains only the same element repeated, return that element,
otherwise return Nothing.

    allElementsEqual [] == Nothing

    allElementsEqual [ 5 ] == Just 5

    allElementsEqual [ 5, 5, 5 ] == Just 5

    allElementsEqual [ 5, 5, 6 ] == Nothing

-}
repeatedElement : List a -> RepeatedElementResult a
repeatedElement list =
    case list of
        [] ->
            NoElements

        first :: rest ->
            if List.isEmpty (List.filter ((/=) first) rest) then
                RepeatedElement first

            else
                NoRepeatedElement


replaceZeroOffset : Maybe Int -> Maybe Int
replaceZeroOffset offset =
    if offset == Just 0 then
        Nothing

    else
        offset


mergeEntryToTime : MergeEntry -> Int
mergeEntryToTime entry =
    case entry of
        ExactMatch time ->
            time

        NearMatch time1 time2 ->
            min time1 time2

        NotNearMatch time1 time2 ->
            min time1 time2

        OneWatchOnly _ time ->
            time


timesListToArray : List Int -> Array Int
timesListToArray times =
    Array.fromList (0 :: times)


getTimes : Timers -> Array Int
getTimes timers =
    case timers of
        None ->
            Array.empty

        Single _ times ->
            timesListToArray times

        Double doubleTimerData ->
            doubleTimerData.mergedTableRows
                |> List.filter .included
                |> List.map (.entry >> mergeEntryToTime)
                |> timesListToArray


identifyProblems : Timers -> BarcodeScannerData -> EventDateAndTime -> IgnoredProblems -> Problems
identifyProblems timers barcodeScannerData eventDateAndTime ignoredProblems =
    let
        positionToAthletesDict : Dict Int (List String)
        positionToAthletesDict =
            barcodeScannerData.scannedBarcodes
                |> Dict.map (\key athleteAndTimePairs -> List.map .athlete athleteAndTimePairs)

        athleteToPositionsDict : Dict String (List Int)
        athleteToPositionsDict =
            getAthleteToPositionsDict positionToAthletesDict

        athleteBarcodesOnly : List String
        athleteBarcodesOnly =
            List.map .athlete barcodeScannerData.athleteBarcodesOnly

        finishTokensOnly : List Int
        finishTokensOnly =
            List.map .position barcodeScannerData.finishTokensOnly

        eventStartDateTimeMillis : Maybe Int
        eventStartDateTimeMillis =
            Maybe.map2
                (\dateAsPosix timeInMinutes -> Time.posixToMillis dateAsPosix + timeInMinutes * 60 * 1000)
                eventDateAndTime.date.parsedValue
                eventDateAndTime.time.parsedValue

        eventStartTimeAsString : String
        eventStartTimeAsString =
            eventDateAndTime.date.enteredValue ++ " " ++ eventDateAndTime.time.enteredValue

        times : Array Int
        times =
            getTimes timers
    in
    { barcodesScannedBeforeEventStart = Maybe.andThen (identifyRecordsScannedBeforeEventStartTime barcodeScannerData eventStartTimeAsString) eventStartDateTimeMillis
    , athletesInSamePositionMultipleTimes = identifyDuplicateScans positionToAthletesDict
    , athletesWithAndWithoutPosition = identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly
    , positionsWithAndWithoutAthlete = identifyPositionsWithAndWithoutAthlete positionToAthletesDict finishTokensOnly
    , timerTimeOffset =
        if ignoredProblems.ignoreTimerTimeOffsets then
            Nothing

        else
            replaceZeroOffset (getTimerTimeOffset timers)
    , athletesWithMultiplePositions = identifyAthletesWithMultiplePositions times athleteToPositionsDict
    , positionsWithMultipleAthletes = identifyPositionsWithMultipleAthletes positionToAthletesDict
    , identicalTimerTimes = identifyIdenticalTimers timers
    , positionOffEndOfTimes = identifyPositionsOffEndOfTimes timers positionToAthletesDict
    , athletesMissingPosition = identifyAthletesWithNoPositions athleteBarcodesOnly athleteToPositionsDict
    , positionsMissingAthlete = identifyPositionsWithNoAthletes finishTokensOnly positionToAthletesDict
    , misScans = identifyMisScannedItems barcodeScannerData.misScannedItems
    , unrecognisedBarcodeScannerLines = identifyUnrecognisedBarcodeScannerLines barcodeScannerData.unrecognisedLines
    , timersInconsistentWithNumberChecker = False
    , timersAndFinishTokensInconsistentWithNumberChecker = False
    }
