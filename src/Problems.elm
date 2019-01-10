module Problems exposing (MinorProblem(..), Problem(..), ProblemsContainer, empty, identifyProblems, problemToString)

import BarcodeScanner exposing (BarcodeScannerData, MisScannedItem, UnrecognisedLine)
import DataStructures exposing (EventDateAndTime)
import DateHandling exposing (dateStringToPosix)
import Dict exposing (Dict)
import MergedTable exposing (Stopwatches(..))
import Set exposing (Set)
import Time exposing (posixToMillis)


type Problem
    = AthleteWithMultiplePositions String (List Int)
    | PositionWithMultipleAthletes Int (List String)
    | PositionOffEndOfTimes Int Int
    | AthleteMissingPosition String
    | PositionMissingAthlete Int
    | MisScan String
    | UnrecognisedBarcodeScannerLine String
    | StopwatchesInconsistentWithNumberChecker
    | StopwatchesAndFinishTokensInconsistentWithNumberChecker


type MinorProblem
    = AthleteInSamePositionMultipleTimes String Int
    | AthleteWithAndWithoutPosition String Int
    | PositionWithAndWithoutAthlete Int String
    | BarcodesScannedBeforeEventStart Int Int String


type alias ProblemsContainer =
    { problems : List Problem
    , minorProblems : List MinorProblem
    }


empty : ProblemsContainer
empty =
    ProblemsContainer [] []


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


identifyAthletesWithMultiplePositions : Dict String (List Int) -> List Problem
identifyAthletesWithMultiplePositions athleteToPositionsDict =
    let
        identifier : ( String, List Int ) -> Maybe Problem
        identifier ( athlete, positions ) =
            if List.length (deduplicate positions) > 1 then
                Just (AthleteWithMultiplePositions athlete positions)

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
            if List.length (deduplicate athletes) > 1 then
                Just (PositionWithMultipleAthletes position athletes)

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
            [ PositionOffEndOfTimes stopwatchTimeCount maxPosition ]

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

        Double _ _ mergedTable ->
            let
                stopwatchTimeCount : Int
                stopwatchTimeCount =
                    List.filterMap .rowNumber mergedTable
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
        |> List.map AthleteMissingPosition


identifyPositionsWithNoAthletes : List Int -> Dict Int (List String) -> List Problem
identifyPositionsWithNoAthletes unpairedPositions positionToAthletesDict =
    deduplicate unpairedPositions
        |> List.filter (\position -> not (Dict.member position positionToAthletesDict))
        |> List.map PositionMissingAthlete


identifyMisScannedItems : List MisScannedItem -> List Problem
identifyMisScannedItems misScannedItems =
    List.map .scannedText misScannedItems
        |> List.map MisScan


identifyUnrecognisedBarcodeScannerLines : List UnrecognisedLine -> List Problem
identifyUnrecognisedBarcodeScannerLines unrecognisedLines =
    List.map .line unrecognisedLines
        |> List.map UnrecognisedBarcodeScannerLine


identifyDuplicateScans : Dict Int (List String) -> List MinorProblem
identifyDuplicateScans positionToAthletesDict =
    let
        identifyDuplicates : ( Int, List String ) -> List MinorProblem
        identifyDuplicates ( position, athletes ) =
            repeatedItems athletes
                |> List.map (\athlete -> AthleteInSamePositionMultipleTimes athlete position)
    in
    Dict.toList positionToAthletesDict
        |> List.filter (\( position, athletes ) -> List.length athletes > 1)
        |> List.concatMap identifyDuplicates


identifyAthletesWithAndWithoutPosition : Dict String (List Int) -> List String -> List MinorProblem
identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly =
    let
        getMinorProblemIfSinglePosition : String -> Maybe MinorProblem
        getMinorProblemIfSinglePosition athlete =
            Dict.get athlete athleteToPositionsDict
                |> Maybe.andThen getSingleValue
                |> Maybe.map (AthleteWithAndWithoutPosition athlete)
    in
    List.filterMap getMinorProblemIfSinglePosition athleteBarcodesOnly


identifyPositionsWithAndWithoutAthlete : Dict Int (List String) -> List Int -> List MinorProblem
identifyPositionsWithAndWithoutAthlete positionToAthletesDict finishTokensOnly =
    let
        getMinorProblemIfSingleAthlete : Int -> Maybe MinorProblem
        getMinorProblemIfSingleAthlete position =
            Dict.get position positionToAthletesDict
                |> Maybe.andThen getSingleValue
                |> Maybe.map (PositionWithAndWithoutAthlete position)
    in
    finishTokensOnly
        |> List.filterMap getMinorProblemIfSingleAthlete


identifyRecordsScannedBeforeEventStartTime : BarcodeScannerData -> String -> Int -> List MinorProblem
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
        [ BarcodesScannedBeforeEventStart totalNumberOfScansBeforeEventStart eventStartTimeMillis eventStartTimeAsString ]


identifyProblems : Stopwatches -> BarcodeScannerData -> EventDateAndTime -> ProblemsContainer
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

        allProblems : List (List Problem)
        allProblems =
            [ identifyAthletesWithMultiplePositions athleteToPositionsDict
            , identifyPositionsWithMultipleAthletes positionToAthletesDict
            , identifyPositionsOffEndOfTimes stopwatches positionToAthletesDict
            , identifyAthletesWithNoPositions athleteBarcodesOnly athleteToPositionsDict
            , identifyPositionsWithNoAthletes finishTokensOnly positionToAthletesDict
            , identifyMisScannedItems barcodeScannerData.misScannedItems
            , identifyUnrecognisedBarcodeScannerLines barcodeScannerData.unrecognisedLines
            ]

        allMinorProblems : List (List MinorProblem)
        allMinorProblems =
            [ identifyDuplicateScans positionToAthletesDict
            , identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly
            , identifyPositionsWithAndWithoutAthlete positionToAthletesDict finishTokensOnly
            , Maybe.map (identifyRecordsScannedBeforeEventStartTime barcodeScannerData eventStartTimeAsString) eventStartTimeMillis
                |> Maybe.withDefault []
            ]
    in
    { problems = List.concat allProblems
    , minorProblems = List.concat allMinorProblems
    }


problemToString : Problem -> String
problemToString problem =
    case problem of
        AthleteWithMultiplePositions athlete positions ->
            "Athlete barcode " ++ athlete ++ " has been scanned with more than one finish token: " ++ String.join ", " (List.map String.fromInt positions)

        PositionWithMultipleAthletes position athletes ->
            "Multiple athlete barcodes have been scanned with finish token " ++ String.fromInt position ++ ": " ++ String.join ", " athletes

        PositionOffEndOfTimes numberOfTimes maxPosition ->
            "The highest finish token scanned was " ++ String.fromInt maxPosition ++ " but there are only " ++ String.fromInt numberOfTimes ++ " times recorded on the stopwatch(es)"

        AthleteMissingPosition athlete ->
            "Athlete barcode " ++ athlete ++ " was scanned without a corresponding finish token"

        PositionMissingAthlete position ->
            "Finish token " ++ String.fromInt position ++ " was scanned without a corresponding athlete barcode"

        MisScan misScannedText ->
            "An unrecognised item '" ++ misScannedText ++ "' was scanned"

        UnrecognisedBarcodeScannerLine line ->
            "The line '" ++ line ++ "' in a barcode scanner file was not recognised"

        StopwatchesInconsistentWithNumberChecker ->
            "TODO"

        StopwatchesAndFinishTokensInconsistentWithNumberChecker ->
            "TODO"
