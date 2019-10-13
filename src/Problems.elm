module Problems exposing
    ( AthleteAndPositionPair
    , AthleteWithMultiplePositionsProblem
    , BarcodeScannerClockDifference
    , BarcodeScannerClockDifferenceType(..)
    , BarcodeScannerClockDifferences(..)
    , BarcodesScannedBeforeEventStartProblem
    , BarcodesScannedTheWrongWayAroundProblem
    , InconsistentBarcodeScannerDatesProblem
    , PositionOffEndOfTimesProblem
    , PositionWithMultipleAthletesProblem
    , Problems
    , identifyProblems
    , noProblems
    )

import BarcodeScanner exposing (BarcodeScannerData, BarcodeScannerFile, BarcodeScannerFileLine, DeletionStatus(..), LineContents(..), MisScannedItem, UnrecognisedLine)
import DateHandling exposing (dateStringToPosix, dateToString)
import Dict exposing (Dict)
import EventDateAndTime exposing (EventDateAndTime)
import Set exposing (Set)
import Stopwatch exposing (MergeEntry(..), MergedTableRow, Stopwatches(..))
import StopwatchOffsetDetection exposing (getStopwatchTimeOffset)
import Time exposing (posixToMillis)


type BarcodeScannerClockDifferenceType
    = OneHourSlow
    | OneHourFast


type alias BarcodeScannerClockDifference =
    { filename : String
    , difference : BarcodeScannerClockDifferenceType
    }


type BarcodeScannerClockDifferences
    = NoClockDifferences
    | SomeClocksDifferent (List BarcodeScannerClockDifference)
    | AllClocksDifferent BarcodeScannerClockDifferenceType


type alias BarcodesScannedBeforeEventStartProblem =
    { numberOfScansBeforeEventStart : Int
    , eventStartTimeMillis : Int
    , eventStartTime : String
    }


type alias AthleteAndPositionPair =
    { athlete : String
    , position : Int
    }


type alias BarcodesScannedTheWrongWayAroundProblem =
    { filename : String
    , startLineNumber : Int
    , endLineNumber : Int
    }


type alias InconsistentBarcodeScannerDatesProblem =
    { scannerDate1 : String
    , scannerDate2 : String
    }


type alias AthleteWithMultiplePositionsProblem =
    { athlete : String
    , positions : List Int
    }


type alias PositionWithMultipleAthletesProblem =
    { position : Int
    , athletes : List String
    }


type alias PositionOffEndOfTimesProblem =
    { stopwatchTimeCount : Int
    , maxPosition : Int
    }


type alias Problems =
    { barcodeScannerClockDifferences : BarcodeScannerClockDifferences
    , barcodesScannedBeforeEventStart : Maybe BarcodesScannedBeforeEventStartProblem
    , athletesInSamePositionMultipleTimes : List AthleteAndPositionPair
    , athletesWithAndWithoutPosition : List AthleteAndPositionPair
    , positionsWithAndWithoutAthlete : List AthleteAndPositionPair
    , barcodesScannedTheWrongWayAround : List BarcodesScannedTheWrongWayAroundProblem
    , stopwatchTimeOffset : Maybe Int
    , inconsistentBarcodeScannerDates : Maybe InconsistentBarcodeScannerDatesProblem
    , athletesWithMultiplePositions : List AthleteWithMultiplePositionsProblem
    , positionsWithMultipleAthletes : List PositionWithMultipleAthletesProblem
    , positionOffEndOfTimes : Maybe PositionOffEndOfTimesProblem
    , athletesMissingPosition : List String
    , positionsMissingAthlete : List Int
    , misScans : List String
    , unrecognisedBarcodeScannerLines : List String
    , identicalStopwatchTimes : Bool
    , stopwatchesInconsistentWithNumberChecker : Bool
    , stopwatchesAndFinishTokensInconsistentWithNumberChecker : Bool
    }


noProblems : Problems
noProblems =
    { barcodeScannerClockDifferences = NoClockDifferences
    , barcodesScannedBeforeEventStart = Nothing
    , athletesInSamePositionMultipleTimes = []
    , athletesWithAndWithoutPosition = []
    , positionsWithAndWithoutAthlete = []
    , barcodesScannedTheWrongWayAround = []
    , stopwatchTimeOffset = Nothing
    , inconsistentBarcodeScannerDates = Nothing
    , athletesWithMultiplePositions = []
    , positionsWithMultipleAthletes = []
    , positionOffEndOfTimes = Nothing
    , athletesMissingPosition = []
    , positionsMissingAthlete = []
    , misScans = []
    , unrecognisedBarcodeScannerLines = []
    , identicalStopwatchTimes = False
    , stopwatchesInconsistentWithNumberChecker = False
    , stopwatchesAndFinishTokensInconsistentWithNumberChecker = False
    }


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


identifyIdenticalStopwatches : Stopwatches -> Bool
identifyIdenticalStopwatches stopwatches =
    case stopwatches of
        None ->
            False

        Single _ _ ->
            False

        Double doubleStopwatchData ->
            List.all isExactMatch doubleStopwatchData.mergedTableRows


identifyInconsistentBarcodeScannerDates : BarcodeScannerData -> Maybe InconsistentBarcodeScannerDatesProblem
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
    Maybe.map (\( first, second ) -> InconsistentBarcodeScannerDatesProblem first second) (hasDifferentValues maxScanDates)


identifyAthletesWithMultiplePositions : Dict String (List Int) -> List AthleteWithMultiplePositionsProblem
identifyAthletesWithMultiplePositions athleteToPositionsDict =
    let
        identifier : ( String, List Int ) -> Maybe AthleteWithMultiplePositionsProblem
        identifier ( athlete, positions ) =
            let
                dedupedPositions : List Int
                dedupedPositions =
                    deduplicate positions
            in
            if List.length dedupedPositions > 1 then
                Just (AthleteWithMultiplePositionsProblem athlete dedupedPositions)

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
checkPositionOffEndOfTimes stopwatchTimeCount positionToAthletesDict =
    Dict.keys positionToAthletesDict
        |> List.filter (\pos -> pos > stopwatchTimeCount)
        |> List.maximum
        |> Maybe.map (PositionOffEndOfTimesProblem stopwatchTimeCount)


identifyPositionsOffEndOfTimes : Stopwatches -> Dict Int (List String) -> Maybe PositionOffEndOfTimesProblem
identifyPositionsOffEndOfTimes stopwatches positionToAthletesDict =
    case stopwatches of
        None ->
            -- Don't report this problem if there are no stopwatches.
            Nothing

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


countScansBeforeTime : Int -> BarcodeScannerFile -> Int
countScansBeforeTime timeMillis file =
    let
        countDatesBefore : List String -> Int
        countDatesBefore dates =
            List.filterMap dateStringToPosix dates
                |> List.map Time.posixToMillis
                |> List.filter (\t -> t < timeMillis)
                |> List.length
    in
    List.filter (\line -> line.deletionStatus == NotDeleted) file.lines
        |> List.map .scanTime
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
identifyRecordsScannedBeforeEventStartTime barcodeScannerData eventStartTimeAsString eventStartTimeMillis =
    let
        totalNumberOfScansBeforeEventStart : Int
        totalNumberOfScansBeforeEventStart =
            List.map (countScansBeforeTime eventStartTimeMillis) barcodeScannerData.files
                |> List.sum
    in
    if totalNumberOfScansBeforeEventStart == 0 then
        Nothing

    else
        Just (BarcodesScannedBeforeEventStartProblem totalNumberOfScansBeforeEventStart eventStartTimeMillis eventStartTimeAsString)


identifyBarcodeScannerClockBeingOut : Int -> BarcodeScannerFile -> Maybe BarcodeScannerClockDifference
identifyBarcodeScannerClockBeingOut eventStartTimeMillis file =
    let
        totalScans : Int
        totalScans =
            List.length file.lines

        countBeforeEventStart : Int
        countBeforeEventStart =
            countScansBeforeTime eventStartTimeMillis file

        countOneHourBeforeEventStart : Int
        countOneHourBeforeEventStart =
            countScansBeforeTime (eventStartTimeMillis - 60 * 60 * 1000) file

        countWithinOneHourAfterEventStart : Int
        countWithinOneHourAfterEventStart =
            countScansBeforeTime (eventStartTimeMillis + 60 * 60 * 1000) file

        countWithinTwoHoursAfterEventStart : Int
        countWithinTwoHoursAfterEventStart =
            countScansBeforeTime (eventStartTimeMillis + 2 * 60 * 60 * 1000) file

        mostOf : Int -> Bool
        mostOf num =
            totalScans > 1 && toFloat num > toFloat totalScans * 0.75
    in
    if mostOf countBeforeEventStart && not (mostOf countOneHourBeforeEventStart) then
        Just { filename = file.name, difference = OneHourSlow }

    else if not (mostOf countWithinOneHourAfterEventStart) && mostOf countWithinTwoHoursAfterEventStart then
        Just { filename = file.name, difference = OneHourFast }

    else
        Nothing


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


identifyBarcodeScannerClocksBeingOut : BarcodeScannerData -> Maybe Int -> BarcodeScannerClockDifferences
identifyBarcodeScannerClocksBeingOut barcodeScannerData eventStartTimeMillisMaybe =
    case eventStartTimeMillisMaybe of
        Just eventStartTimeMillis ->
            let
                allClockDifferences : List BarcodeScannerClockDifference
                allClockDifferences =
                    List.filterMap (identifyBarcodeScannerClockBeingOut eventStartTimeMillis) barcodeScannerData.files

                allDifferenceTypes : List BarcodeScannerClockDifferenceType
                allDifferenceTypes =
                    List.map .difference allClockDifferences

                commonDifferenceType : RepeatedElementResult BarcodeScannerClockDifferenceType
                commonDifferenceType =
                    repeatedElement allDifferenceTypes
            in
            case commonDifferenceType of
                NoElements ->
                    NoClockDifferences

                RepeatedElement differenceType ->
                    if List.length allClockDifferences == List.length barcodeScannerData.files && List.length barcodeScannerData.files > 1 then
                        AllClocksDifferent differenceType

                    else
                        SomeClocksDifferent allClockDifferences

                NoRepeatedElement ->
                    SomeClocksDifferent allClockDifferences

        Nothing ->
            NoClockDifferences


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
            if first.deletionStatus == NotDeleted then
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

            else
                findEndOfWrongWayAroundSection startLineNumber prevFinishToken rest


identifyWrongWayArounds : String -> List BarcodeScannerFileLine -> List BarcodesScannedTheWrongWayAroundProblem
identifyWrongWayArounds filename lines =
    case lines of
        [] ->
            []

        firstLine :: remainingLines ->
            case ( firstLine.contents, firstLine.deletionStatus ) of
                ( Ordinary "" (Just finishToken), NotDeleted ) ->
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
                            BarcodesScannedTheWrongWayAroundProblem filename startLineNumber endLineNumber :: identifyWrongWayArounds filename subsequentLines

                        Nothing ->
                            identifyWrongWayArounds filename remainingLines

                _ ->
                    -- Anything else: scan remaining lines.
                    identifyWrongWayArounds filename remainingLines


identifyBarcodesScannedTheWrongWayAroundInFile : BarcodeScannerFile -> List BarcodesScannedTheWrongWayAroundProblem
identifyBarcodesScannedTheWrongWayAroundInFile file =
    identifyWrongWayArounds file.name file.lines


identifyBarcodesScannedTheWrongWayAround : BarcodeScannerData -> List BarcodesScannedTheWrongWayAroundProblem
identifyBarcodesScannedTheWrongWayAround barcodeScannerData =
    List.map identifyBarcodesScannedTheWrongWayAroundInFile barcodeScannerData.files
        |> List.concat


replaceZeroOffset : Maybe Int -> Maybe Int
replaceZeroOffset offset =
    if offset == Just 0 then
        Nothing

    else
        offset


identifyProblems : Stopwatches -> BarcodeScannerData -> EventDateAndTime -> Problems
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
    { barcodeScannerClockDifferences = identifyBarcodeScannerClocksBeingOut barcodeScannerData eventStartTimeMillis
    , barcodesScannedBeforeEventStart = Maybe.andThen (identifyRecordsScannedBeforeEventStartTime barcodeScannerData eventStartTimeAsString) eventStartTimeMillis
    , athletesInSamePositionMultipleTimes = identifyDuplicateScans positionToAthletesDict
    , athletesWithAndWithoutPosition = identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly
    , positionsWithAndWithoutAthlete = identifyPositionsWithAndWithoutAthlete positionToAthletesDict finishTokensOnly
    , barcodesScannedTheWrongWayAround = identifyBarcodesScannedTheWrongWayAround barcodeScannerData
    , stopwatchTimeOffset = replaceZeroOffset (getStopwatchTimeOffset stopwatches)
    , inconsistentBarcodeScannerDates = identifyInconsistentBarcodeScannerDates barcodeScannerData
    , athletesWithMultiplePositions = identifyAthletesWithMultiplePositions athleteToPositionsDict
    , positionsWithMultipleAthletes = identifyPositionsWithMultipleAthletes positionToAthletesDict
    , identicalStopwatchTimes = identifyIdenticalStopwatches stopwatches
    , positionOffEndOfTimes = identifyPositionsOffEndOfTimes stopwatches positionToAthletesDict
    , athletesMissingPosition = identifyAthletesWithNoPositions athleteBarcodesOnly athleteToPositionsDict
    , positionsMissingAthlete = identifyPositionsWithNoAthletes finishTokensOnly positionToAthletesDict
    , misScans = identifyMisScannedItems barcodeScannerData.misScannedItems
    , unrecognisedBarcodeScannerLines = identifyUnrecognisedBarcodeScannerLines barcodeScannerData.unrecognisedLines
    , stopwatchesInconsistentWithNumberChecker = False
    , stopwatchesAndFinishTokensInconsistentWithNumberChecker = False
    }
