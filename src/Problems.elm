module Problems exposing
    ( AthleteAndPositionPair
    , AthleteWithAndWithoutPositionProblem
    , AthleteWithMultiplePositionsProblem
    , IgnoredProblems
    , MisScannedAthleteBarcodeProblem
    , PositionAndTime
    , PositionOffEndOfTimesProblem
    , PositionWithMultipleAthletesProblem
    , Problems
    , identifyProblems
    , noIgnoredProblems
    , noProblems
    )

import Array exposing (Array)
import BarcodeScanner exposing (BarcodeScannerData, DeletionStatus(..), LineContents(..), MisScannedItem, UnrecognisedLine)
import Dict exposing (Dict)
import Set exposing (Set)
import Timer exposing (MergeEntry(..), Timers(..))
import TimerOffsetDetection exposing (getTimerTimeOffset)


{-| The maximum permitted length of an athlete barcode, as a string, including
the leading "A". This is presently 9, to allow for 8-digit barcodes, for
up to 100 million athletes. At the time of writing there are no 8-digit
barcodes in use.
-}
maxAthleteBarcodeLength : Int
maxAthleteBarcodeLength =
    9


{-| When a too-long barcode has been found, the length of barcode prefix we
check for to identify a similar barcode. This includes the leading "A".
-}
misScannedAthleteBarcodeSimilarityLength : Int
misScannedAthleteBarcodeSimilarityLength =
    6


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


type alias AthleteWithAndWithoutPositionProblem =
    { athlete : String
    , count : Int
    , position : Int
    }


type alias MisScannedAthleteBarcodeProblem =
    { scannedBarcode : String
    , similarBarcode : String
    }


type alias Problems =
    { athletesWithAndWithoutPosition : List AthleteWithAndWithoutPositionProblem
    , timerTimeOffset : Maybe Int
    , athletesWithMultiplePositions : List AthleteWithMultiplePositionsProblem
    , positionsWithMultipleAthletes : List PositionWithMultipleAthletesProblem
    , positionOffEndOfTimes : Maybe PositionOffEndOfTimesProblem
    , athletesMissingPosition : List String
    , misScannedAthleteBarcodes : List MisScannedAthleteBarcodeProblem
    , misScans : List String
    , unrecognisedBarcodeScannerLines : List String
    , timersInconsistentWithNumberChecker : Bool
    , timersAndFinishTokensInconsistentWithNumberChecker : Bool
    }


noProblems : Problems
noProblems =
    { athletesWithAndWithoutPosition = []
    , timerTimeOffset = Nothing
    , athletesWithMultiplePositions = []
    , positionsWithMultipleAthletes = []
    , positionOffEndOfTimes = Nothing
    , athletesMissingPosition = []
    , misScannedAthleteBarcodes = []
    , misScans = []
    , unrecognisedBarcodeScannerLines = []
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


getSingleValue : List comparable -> Maybe comparable
getSingleValue list =
    case deduplicate list of
        [ singleValue ] ->
            Just singleValue

        _ ->
            Nothing


identifyAthletesWithNoPositions : List String -> Dict String (List Int) -> Set String -> List String
identifyAthletesWithNoPositions unpairedAthletes athleteToPositionsDict athletesToExclude =
    deduplicate unpairedAthletes
        |> List.filter (\athlete -> not (Set.member athlete athletesToExclude))
        |> List.filter (\athlete -> not (Dict.member athlete athleteToPositionsDict))


identifyMisScannedItems : List MisScannedItem -> List String
identifyMisScannedItems misScannedItems =
    List.map .scannedText misScannedItems


identifyUnrecognisedBarcodeScannerLines : List UnrecognisedLine -> List String
identifyUnrecognisedBarcodeScannerLines unrecognisedLines =
    List.map .line unrecognisedLines


identifyMisScannedAthleteBarcodes : Dict String (List Int) -> List String -> List MisScannedAthleteBarcodeProblem
identifyMisScannedAthleteBarcodes athleteToPositionsDict athleteBarcodesOnly =
    let
        athleteBarcodesTooLong : List String
        athleteBarcodesTooLong =
            List.filter (\barcode -> String.length barcode > maxAthleteBarcodeLength) athleteBarcodesOnly
                |> Set.fromList
                |> Set.toList

        findSimilarBarcode : String -> Maybe MisScannedAthleteBarcodeProblem
        findSimilarBarcode misScannedBarcode =
            let
                misScannedBarcodePrefix : String
                misScannedBarcodePrefix =
                    String.slice 0 misScannedAthleteBarcodeSimilarityLength misScannedBarcode
            in
            List.append (Dict.keys athleteToPositionsDict) athleteBarcodesOnly
                |> List.filter (\barcode -> String.length barcode <= maxAthleteBarcodeLength)
                |> List.filter (\barcode -> String.slice 0 misScannedAthleteBarcodeSimilarityLength barcode == misScannedBarcodePrefix)
                |> List.head
                |> Maybe.map (MisScannedAthleteBarcodeProblem misScannedBarcode)
    in
    List.filterMap findSimilarBarcode athleteBarcodesTooLong


identifyAthletesWithAndWithoutPosition : Dict String (List Int) -> List String -> List AthleteWithAndWithoutPositionProblem
identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly =
    let
        insert : String -> List ( String, Int ) -> List ( String, Int )
        insert value items =
            case items of
                [] ->
                    [ ( value, 1 ) ]

                ( existingValue, count ) :: remainder ->
                    if value == existingValue then
                        ( existingValue, count + 1 ) :: remainder

                    else
                        ( existingValue, count ) :: insert value remainder

        athleteBarcodesOnlyWithCounts : List ( String, Int )
        athleteBarcodesOnlyWithCounts =
            List.foldl insert [] athleteBarcodesOnly

        getProblemIfSinglePosition : ( String, Int ) -> Maybe AthleteWithAndWithoutPositionProblem
        getProblemIfSinglePosition ( athlete, count ) =
            Dict.get athlete athleteToPositionsDict
                |> Maybe.andThen getSingleValue
                |> Maybe.map (AthleteWithAndWithoutPositionProblem athlete count)
    in
    List.filterMap getProblemIfSinglePosition athleteBarcodesOnlyWithCounts


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


identifyProblems : Timers -> BarcodeScannerData -> IgnoredProblems -> Problems
identifyProblems timers barcodeScannerData ignoredProblems =
    let
        positionToAthletesDict : Dict Int (List String)
        positionToAthletesDict =
            barcodeScannerData.scannedBarcodes
                |> Dict.map (\_ athleteAndTimePairs -> List.map .athlete athleteAndTimePairs)

        athleteToPositionsDict : Dict String (List Int)
        athleteToPositionsDict =
            getAthleteToPositionsDict positionToAthletesDict

        athleteBarcodesOnly : List String
        athleteBarcodesOnly =
            List.map .athlete barcodeScannerData.athleteBarcodesOnly

        times : Array Int
        times =
            getTimes timers

        misScannedAthleteBarcodes : List MisScannedAthleteBarcodeProblem
        misScannedAthleteBarcodes =
            identifyMisScannedAthleteBarcodes athleteToPositionsDict athleteBarcodesOnly

        misScannedAthleteBarcodesSet : Set String
        misScannedAthleteBarcodesSet =
            List.map .scannedBarcode misScannedAthleteBarcodes
                |> Set.fromList
    in
    { athletesWithAndWithoutPosition = identifyAthletesWithAndWithoutPosition athleteToPositionsDict athleteBarcodesOnly
    , timerTimeOffset =
        if ignoredProblems.ignoreTimerTimeOffsets then
            Nothing

        else
            replaceZeroOffset (getTimerTimeOffset timers)
    , athletesWithMultiplePositions = identifyAthletesWithMultiplePositions times athleteToPositionsDict
    , positionsWithMultipleAthletes = identifyPositionsWithMultipleAthletes positionToAthletesDict
    , positionOffEndOfTimes = identifyPositionsOffEndOfTimes timers positionToAthletesDict
    , athletesMissingPosition = identifyAthletesWithNoPositions athleteBarcodesOnly athleteToPositionsDict misScannedAthleteBarcodesSet
    , misScannedAthleteBarcodes = misScannedAthleteBarcodes
    , misScans = identifyMisScannedItems barcodeScannerData.misScannedItems
    , unrecognisedBarcodeScannerLines = identifyUnrecognisedBarcodeScannerLines barcodeScannerData.unrecognisedLines
    , timersInconsistentWithNumberChecker = False
    , timersAndFinishTokensInconsistentWithNumberChecker = False
    }
