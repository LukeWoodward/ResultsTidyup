module Problems exposing (FixableProblem(..), NonFixableProblem(..), Problem(..), identifyProblems)

import BarcodeScanner exposing (BarcodeScannerData, BarcodeScannerFile, BarcodeScannerFileLine, DeletionStatus(..), LineContents(..), MisScannedItem, UnrecognisedLine)
import DateHandling exposing (dateStringToPosix, dateToString)
import Dict exposing (Dict)
import EventDateAndTime exposing (EventDateAndTime)
import Set exposing (Set)
import Stopwatch exposing (Stopwatches(..))
import StopwatchOffsetDetection exposing (getStopwatchTimeOffset)
import Time exposing (posixToMillis)


type FixableProblem
    = BarcodesScannedBeforeEventStart Int Int String
    | AthleteInSamePositionMultipleTimes String Int
    | AthleteWithAndWithoutPosition String Int
    | PositionWithAndWithoutAthlete Int String
    | BarcodesScannedTheWrongWayAround String Int Int
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

        linesBeforeEventStart : BarcodeScannerFile -> ( Int, Int )
        linesBeforeEventStart file =
            ( List.filter (\line -> line.deletionStatus == NotDeleted) file.lines
                |> List.map .scanTime
                |> countDatesBeforeEventStart
            , List.length file.lines
            )

        totalNumberOfScansBeforeEventStart : Int
        totalNumberOfScansBeforeEventStart =
            List.map linesBeforeEventStart barcodeScannerData.files
                |> List.map Tuple.first
                |> List.sum
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


{-| We've found the start of a possible wrong-way-around section. See if we can
find the end. If we find the end, return the range of lines that the
wrong-way-around section covers. If not, return Nothing.
-}
findEndOfWrongWayAroundSection : Int -> Int -> List BarcodeScannerFileLine -> Maybe ( Int, Int )
findEndOfWrongWayAroundSection startLineNumber prevFinishToken lines =
    case lines of
        [] ->
            -- Hit the end: nothing wrong-way-around.
            Nothing

        first :: rest ->
            case first.contents of
                BarcodeScanner.MisScan text ->
                    -- Hit a mis-scan: abandon the search.
                    Nothing

                Ordinary "" _ ->
                    -- Another position-only record.  Abandon the search.
                    Nothing

                Ordinary athlete Nothing ->
                    -- Athlete-only record.  Stop things here.
                    Just ( startLineNumber, first.lineNumber )

                Ordinary athlete (Just thisFinishToken) ->
                    if thisFinishToken == prevFinishToken then
                        if startLineNumber + 1 == first.lineNumber then
                            -- We have a complete record immediately following a finish-token-only
                            -- record with the same token.  Ignore the former record: there's no
                            -- sequence of reversed scans.
                            Nothing

                        else
                            -- End things here on the previous row: this is an intact record, and
                            -- the finish token in the previous record will be an error.
                            Just ( startLineNumber, first.lineNumber - 1 )

                    else
                        findEndOfWrongWayAroundSection startLineNumber thisFinishToken rest


identifyWrongWayArounds : String -> List BarcodeScannerFileLine -> List FixableProblem
identifyWrongWayArounds filename lines =
    case lines of
        [] ->
            []

        firstLine :: remainingLines ->
            case firstLine.contents of
                Ordinary "" (Just finishToken) ->
                    -- Finish token with no athlete: here's the start of a possible run of
                    -- reversed scans.
                    let
                        wrongWayAroundStatus : Maybe ( Int, Int )
                        wrongWayAroundStatus =
                            findEndOfWrongWayAroundSection firstLine.lineNumber finishToken remainingLines
                    in
                    case wrongWayAroundStatus of
                        Just ( startLineNumber, endLineNumber ) ->
                            let
                                -- Skip the lines within the range given.
                                subsequentLines : List BarcodeScannerFileLine
                                subsequentLines =
                                    List.filter (\line -> line.lineNumber > endLineNumber) remainingLines
                            in
                            BarcodesScannedTheWrongWayAround filename startLineNumber endLineNumber :: identifyWrongWayArounds filename subsequentLines

                        Nothing ->
                            identifyWrongWayArounds filename remainingLines

                _ ->
                    -- Anything else: scan remaining lines.
                    identifyWrongWayArounds filename remainingLines


identifyBarcodesScannedTheWrongWayAroundInFile : BarcodeScannerFile -> List FixableProblem
identifyBarcodesScannedTheWrongWayAroundInFile file =
    identifyWrongWayArounds file.name file.lines


identifyBarcodesScannedTheWrongWayAround : BarcodeScannerData -> List Problem
identifyBarcodesScannedTheWrongWayAround barcodeScannerData =
    List.map identifyBarcodesScannedTheWrongWayAroundInFile barcodeScannerData.files
        |> List.concat
        |> List.map Fixable


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
        , identifyBarcodesScannedTheWrongWayAround barcodeScannerData
        ]
