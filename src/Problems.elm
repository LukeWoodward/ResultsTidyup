module Problems exposing (FixableProblem(..), NonFixableProblem(..), Problem(..), identifyProblems)

import BarcodeScanner exposing (BarcodeScannerData, MisScannedItem, UnrecognisedLine)
import DataStructures exposing (EventDateAndTime)
import DateHandling exposing (dateStringToPosix, dateToString)
import Dict exposing (Dict)
import MergedTable exposing (Stopwatches(..))
import Set exposing (Set)
import StopwatchOffsetDetection exposing (getStopwatchTimeOffset)
import Time exposing (posixToMillis)


type FixableProblem
    = BarcodesScannedBeforeEventStart Int Int String
    | AthleteInSamePositionMultipleTimes String Int
    | AthleteWithAndWithoutPosition String Int
    | PositionWithAndWithoutAthlete Int String
    | StopwatchTimeOffset Int


type NonFixableProblem
    = InconsistentBarcodeScannerDates String String
    | AthleteWithMultiplePositions String (List Int)
    | PositionWithMultipleAthletes Int (List String)
    | PositionOffEndOfTimes Int Int
    | AthleteMissingPosition String
    | PositionMissingAthlete Int
    | MisScan String
    | UnrecognisedBarcodeScannerLine String
    | StopwatchesInconsistentWithNumberChecker
    | StopwatchesAndFinishTokensInconsistentWithNumberChecker


type Problem
    = Fixable FixableProblem
    | NonFixable NonFixableProblem


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


identifyInconsistentBarcodeScannerDates : BarcodeScannerData -> List Problem
identifyInconsistentBarcodeScannerDates barcodeScannerData =
    let
        maxScanDates : List String
        maxScanDates =
            List.filterMap .maxScanDate barcodeScannerData.files
                |> List.map Time.posixToMillis
                |> deduplicate
                |> List.map Time.millisToPosix
                |> List.map dateToString
    in
    case hasDifferentValues maxScanDates of
        Just ( first, second ) ->
            [ NonFixable (InconsistentBarcodeScannerDates first second) ]

        Nothing ->
            []


identifyAthletesWithMultiplePositions : Dict String (List Int) -> List Problem
identifyAthletesWithMultiplePositions athleteToPositionsDict =
    let
        identifier : ( String, List Int ) -> Maybe Problem
        identifier ( athlete, positions ) =
            let
                dedupedPositions : List Int
                dedupedPositions =
                    deduplicate positions
            in
            if List.length dedupedPositions > 1 then
                Just (NonFixable (AthleteWithMultiplePositions athlete dedupedPositions))

            else
                Nothing
    in
    Dict.toList athleteToPositionsDict
        |> List.filterMap identifier


identifyPositionsWithMultipleAthletes : Dict Int (List String) -> List Problem
identifyPositionsWithMultipleAthletes positionToAthletesDict =
    let
        identifier : ( Int, List String ) -> Maybe Problem
        identifier ( position, athletes ) =
            let
                dedupedAthletes : List String
                dedupedAthletes =
                    deduplicate athletes
            in
            if List.length dedupedAthletes > 1 then
                Just (NonFixable (PositionWithMultipleAthletes position dedupedAthletes))

            else
                Nothing
    in
    Dict.toList positionToAthletesDict
        |> List.filterMap identifier


checkPositionOffEndOfTimes : Int -> Dict Int (List String) -> List Problem
checkPositionOffEndOfTimes stopwatchTimeCount positionToAthletesDict =
    let
        maxOverPosition : Maybe Int
        maxOverPosition =
            Dict.keys positionToAthletesDict
                |> List.filter (\pos -> pos > stopwatchTimeCount)
                |> List.maximum
    in
    case maxOverPosition of
        Just maxPosition ->
            [ NonFixable (PositionOffEndOfTimes stopwatchTimeCount maxPosition) ]

        Nothing ->
            []


identifyPositionsOffEndOfTimes : Stopwatches -> Dict Int (List String) -> List Problem
identifyPositionsOffEndOfTimes stopwatches positionToAthletesDict =
    case stopwatches of
        None ->
            -- Don't report this problem if there are no stopwatches.
            []

        Single _ times ->
            checkPositionOffEndOfTimes (List.length times) positionToAthletesDict

        Double doubleStopwatchData ->
            let
                stopwatchTimeCount : Int
                stopwatchTimeCount =
                    List.filterMap .rowNumber doubleStopwatchData.mergedTableRows
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            checkPositionOffEndOfTimes stopwatchTimeCount positionToAthletesDict


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


identifyAthletesWithNoPositions : List String -> Dict String (List Int) -> List Problem
identifyAthletesWithNoPositions unpairedAthletes athleteToPositionsDict =
    deduplicate unpairedAthletes
        |> List.filter (\athlete -> not (Dict.member athlete athleteToPositionsDict))
        |> List.map (NonFixable << AthleteMissingPosition)


identifyPositionsWithNoAthletes : List Int -> Dict Int (List String) -> List Problem
identifyPositionsWithNoAthletes unpairedPositions positionToAthletesDict =
    deduplicate unpairedPositions
        |> List.filter (\position -> not (Dict.member position positionToAthletesDict))
        |> List.map (NonFixable << PositionMissingAthlete)


identifyMisScannedItems : List MisScannedItem -> List Problem
identifyMisScannedItems misScannedItems =
    List.map .scannedText misScannedItems
        |> List.map (NonFixable << MisScan)


identifyUnrecognisedBarcodeScannerLines : List UnrecognisedLine -> List Problem
identifyUnrecognisedBarcodeScannerLines unrecognisedLines =
    List.map .line unrecognisedLines
        |> List.map (NonFixable << UnrecognisedBarcodeScannerLine)


identifyDuplicateScans : Dict Int (List String) -> List Problem
identifyDuplicateScans positionToAthletesDict =
    let
        identifyDuplicates : ( Int, List String ) -> List Problem
        identifyDuplicates ( position, athletes ) =
            repeatedItems athletes
                |> List.map (\athlete -> Fixable (AthleteInSamePositionMultipleTimes athlete position))
    in
    Dict.toList positionToAthletesDict
        |> List.filter (\( position, athletes ) -> List.length athletes > 1)
        |> List.concatMap identifyDuplicates


identifyAthletesWithAndWithoutPosition : Dict String (List Int) -> List String -> List Problem
identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly =
    let
        getFixableProblemIfSinglePosition : String -> Maybe Problem
        getFixableProblemIfSinglePosition athlete =
            Dict.get athlete athleteToPositionsDict
                |> Maybe.andThen getSingleValue
                |> Maybe.map (Fixable << AthleteWithAndWithoutPosition athlete)
    in
    List.filterMap getFixableProblemIfSinglePosition athleteBarcodesOnly


identifyPositionsWithAndWithoutAthlete : Dict Int (List String) -> List Int -> List Problem
identifyPositionsWithAndWithoutAthlete positionToAthletesDict finishTokensOnly =
    let
        getFixableProblemIfSingleAthlete : Int -> Maybe Problem
        getFixableProblemIfSingleAthlete position =
            Dict.get position positionToAthletesDict
                |> Maybe.andThen getSingleValue
                |> Maybe.map (Fixable << PositionWithAndWithoutAthlete position)
    in
    finishTokensOnly
        |> List.filterMap getFixableProblemIfSingleAthlete


identifyRecordsScannedBeforeEventStartTime : BarcodeScannerData -> String -> Int -> List Problem
identifyRecordsScannedBeforeEventStartTime barcodeScannerData eventStartTimeAsString eventStartTimeMillis =
    let
        countDatesBeforeEventStart : List String -> Int
        countDatesBeforeEventStart dates =
            List.filterMap dateStringToPosix dates
                |> List.map Time.posixToMillis
                |> List.filter (\t -> t < eventStartTimeMillis)
                |> List.length

        scannedBarcodesBeforeEventStart : Int
        scannedBarcodesBeforeEventStart =
            barcodeScannerData.scannedBarcodes
                |> Dict.values
                |> List.concat
                |> List.map .scanTime
                |> countDatesBeforeEventStart

        athleteBarcodesOnlyBeforeEventStart : Int
        athleteBarcodesOnlyBeforeEventStart =
            barcodeScannerData.athleteBarcodesOnly
                |> List.map .scanTime
                |> countDatesBeforeEventStart

        finishTokensOnlyBeforeEventStart : Int
        finishTokensOnlyBeforeEventStart =
            barcodeScannerData.finishTokensOnly
                |> List.map .scanTime
                |> countDatesBeforeEventStart

        totalNumberOfScansBeforeEventStart : Int
        totalNumberOfScansBeforeEventStart =
            scannedBarcodesBeforeEventStart + athleteBarcodesOnlyBeforeEventStart + finishTokensOnlyBeforeEventStart
    in
    if totalNumberOfScansBeforeEventStart == 0 then
        []

    else
        [ Fixable (BarcodesScannedBeforeEventStart totalNumberOfScansBeforeEventStart eventStartTimeMillis eventStartTimeAsString) ]


identifyStopwatchTimeOffset : Stopwatches -> List Problem
identifyStopwatchTimeOffset stopwatches =
    let
        offset : Int
        offset =
            getStopwatchTimeOffset stopwatches
    in
    if offset == 0 then
        []

    else
        [ Fixable (StopwatchTimeOffset offset) ]


identifyProblems : Stopwatches -> BarcodeScannerData -> EventDateAndTime -> List Problem
identifyProblems stopwatches barcodeScannerData eventDateAndTime =
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

        eventStartTimeMillis : Maybe Int
        eventStartTimeMillis =
            Maybe.map2
                (\dateAsPosix timeInMinutes -> Time.posixToMillis dateAsPosix + timeInMinutes * 60 * 1000)
                eventDateAndTime.validatedDate
                eventDateAndTime.validatedTime

        eventStartTimeAsString : String
        eventStartTimeAsString =
            eventDateAndTime.enteredDate ++ " " ++ eventDateAndTime.enteredTime
    in
    List.concat
        [ Maybe.map (identifyRecordsScannedBeforeEventStartTime barcodeScannerData eventStartTimeAsString) eventStartTimeMillis
            |> Maybe.withDefault []
        , identifyStopwatchTimeOffset stopwatches
        , identifyDuplicateScans positionToAthletesDict
        , identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly
        , identifyPositionsWithAndWithoutAthlete positionToAthletesDict finishTokensOnly
        , identifyInconsistentBarcodeScannerDates barcodeScannerData
        , identifyAthletesWithMultiplePositions athleteToPositionsDict
        , identifyPositionsWithMultipleAthletes positionToAthletesDict
        , identifyPositionsOffEndOfTimes stopwatches positionToAthletesDict
        , identifyAthletesWithNoPositions athleteBarcodesOnly athleteToPositionsDict
        , identifyPositionsWithNoAthletes finishTokensOnly positionToAthletesDict
        , identifyMisScannedItems barcodeScannerData.misScannedItems
        , identifyUnrecognisedBarcodeScannerLines barcodeScannerData.unrecognisedLines
        ]
