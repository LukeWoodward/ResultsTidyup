module BarcodeScanner exposing
    ( AthleteAndTimePair
    , BarcodeScannerData
    , BarcodeScannerFile
    , BarcodeScannerFileLine
    , DeletionReason(..)
    , DeletionStatus(..)
    , LineContents(..)
    , MisScannedItem
    , PositionAndTimePair
    , UnrecognisedLine
    , WrongWayAroundStatus(..)
    , empty
    , generateDownloadText
    , isEmpty
    , maxFinishToken
    , mergeScannerData
    , readBarcodeScannerData
    , regenerate
    )

import DateHandling exposing (dateStringToPosix)
import Dict exposing (Dict)
import Error exposing (Error)
import FileHandling exposing (crlf, isPossibleBinary, splitLines)
import Parser exposing ((|.), (|=), Parser, end, int, run, succeed, symbol)
import Parsers exposing (digitsRange)
import Result.Extra
import Time exposing (Posix)


type alias Timed a =
    { a | scanTime : String }


maxUnrecognisedLines : Int
maxUnrecognisedLines =
    10


type alias AthleteAndTimePair =
    { athlete : String
    , scanTime : String
    }


type alias PositionAndTimePair =
    { position : Int
    , scanTime : String
    }


type alias MisScannedItem =
    { scannedText : String
    , scanTime : String
    }


type alias UnrecognisedLine =
    { line : String
    , errorCode : String
    , errorMessage : String
    }


type DeletionReason
    = BeforeEventStart
    | DuplicateScan String Int
    | AthleteScannedWithFinishTokenElsewhere String
    | FinishTokenScannedWithAthleteElsewhere Int
    | EndOfWrongWayAroundSection


type DeletionStatus
    = NotDeleted
    | Deleted DeletionReason


type LineContents
    = Ordinary String (Maybe Int)
    | MisScan String


type WrongWayAroundStatus
    = NotWrongWayAround
    | FirstWrongWayAround Int Int
    | SubsequentWrongWayAround


type alias BarcodeScannerFileLine =
    { lineNumber : Int
    , contents : LineContents
    , scanTime : String
    , deletionStatus : DeletionStatus
    , wrongWayAroundStatus : WrongWayAroundStatus
    }


type alias BarcodeScannerFile =
    { name : String
    , lines : List BarcodeScannerFileLine
    , maxScanDate : Maybe Posix
    }


type alias BarcodeScannerData =
    { files : List BarcodeScannerFile
    , scannedBarcodes : Dict Int (List AthleteAndTimePair)
    , athleteBarcodesOnly : List AthleteAndTimePair
    , finishTokensOnly : List PositionAndTimePair
    , misScannedItems : List MisScannedItem
    , unrecognisedLines : List UnrecognisedLine
    , lastScanDate : Maybe Posix
    }


toMaybeError : Result e x -> Maybe e
toMaybeError result =
    case result of
        Ok _ ->
            Nothing

        Err error ->
            Just error


empty : BarcodeScannerData
empty =
    BarcodeScannerData [] Dict.empty [] [] [] [] Nothing


isEmpty : BarcodeScannerData -> Bool
isEmpty barcodeScannerData =
    Dict.isEmpty barcodeScannerData.scannedBarcodes
        && List.isEmpty barcodeScannerData.athleteBarcodesOnly
        && List.isEmpty barcodeScannerData.finishTokensOnly


athleteParser : Parser ()
athleteParser =
    symbol "A"
        |. int
        |. end


positionParser : Parser ()
positionParser =
    symbol "P"
        |. digitsRange 1 5
        |. end


okDefaultFileLine : Int -> LineContents -> String -> Result e BarcodeScannerFileLine
okDefaultFileLine lineNumber contents scanTime =
    Ok (BarcodeScannerFileLine lineNumber contents scanTime NotDeleted NotWrongWayAround)


readLine : Int -> String -> Result UnrecognisedLine BarcodeScannerFileLine
readLine lineNumber line =
    let
        parts : List String
        parts =
            String.split "," line

        unrecognisedLine : String -> String -> Result UnrecognisedLine BarcodeScannerFileLine
        unrecognisedLine code message =
            UnrecognisedLine line code message
                |> Err
    in
    case parts of
        [ athlete, position, time ] ->
            let
                isAthleteMissing : Bool
                isAthleteMissing =
                    athlete == ""

                hasInvalidAthlete : Bool
                hasInvalidAthlete =
                    not isAthleteMissing && Result.Extra.isErr (run athleteParser athlete)

                isPositionMissing : Bool
                isPositionMissing =
                    position == ""

                hasInvalidPosition : Bool
                hasInvalidPosition =
                    not isPositionMissing && Result.Extra.isErr (run positionParser position)

                positionNumber : Maybe Int
                positionNumber =
                    if hasInvalidPosition then
                        Nothing

                    else
                        String.toInt (String.dropLeft 1 position)
            in
            if hasInvalidAthlete then
                unrecognisedLine "INVALID_ATHLETE_RECORD" ("Invalid athlete record '" ++ athlete ++ "' found in barcode scanner file")

            else if hasInvalidPosition then
                unrecognisedLine "INVALID_POSITION_RECORD" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")

            else if isPositionMissing && isAthleteMissing then
                unrecognisedLine "ATHLETE_AND_FINISH_TOKEN_MISSING"
                    ("Barcode scanner file contains line '"
                        ++ line
                        ++ "' with neither athlete nor finish token"
                    )

            else if isPositionMissing then
                okDefaultFileLine lineNumber (Ordinary athlete Nothing) time

            else
                case positionNumber of
                    Just 0 ->
                        unrecognisedLine "INVALID_POSITION_ZERO" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")

                    Just pos ->
                        okDefaultFileLine lineNumber (Ordinary athlete positionNumber) time

                    Nothing ->
                        unrecognisedLine "NON_NUMERIC_POSITION" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")

        [ misScannedText, time ] ->
            okDefaultFileLine lineNumber (MisScan misScannedText) time

        _ ->
            unrecognisedLine "NOT_TWO_OR_THREE_PARTS" ("Line " ++ line ++ " does not contain the expected two or three comma-separated parts")


mergeEntry : BarcodeScannerFileLine -> BarcodeScannerData -> BarcodeScannerData
mergeEntry line barcodeData =
    case line.contents of
        Ordinary "" Nothing ->
            -- Unexpected, do nothing.
            barcodeData

        Ordinary athlete Nothing ->
            { barcodeData | athleteBarcodesOnly = List.append barcodeData.athleteBarcodesOnly [ AthleteAndTimePair athlete line.scanTime ] }

        Ordinary "" (Just position) ->
            { barcodeData | finishTokensOnly = List.append barcodeData.finishTokensOnly [ PositionAndTimePair position line.scanTime ] }

        Ordinary athlete (Just pos) ->
            let
                updater : Maybe (List AthleteAndTimePair) -> Maybe (List AthleteAndTimePair)
                updater currentValue =
                    currentValue
                        |> Maybe.withDefault []
                        |> (\x -> List.append x [ AthleteAndTimePair athlete line.scanTime ])
                        |> Just
            in
            { barcodeData | scannedBarcodes = Dict.update pos updater barcodeData.scannedBarcodes }

        MisScan misScannedText ->
            { barcodeData | misScannedItems = List.append barcodeData.misScannedItems [ MisScannedItem misScannedText line.scanTime ] }


mergeEntries : List BarcodeScannerFileLine -> BarcodeScannerData
mergeEntries scannerFileLines =
    List.foldl mergeEntry empty scannerFileLines


mergeScannerDictEntry : ( Int, List AthleteAndTimePair ) -> Dict Int (List AthleteAndTimePair) -> Dict Int (List AthleteAndTimePair)
mergeScannerDictEntry ( pos, athleteAndTimePairs ) dict =
    let
        updater : Maybe (List AthleteAndTimePair) -> Maybe (List AthleteAndTimePair)
        updater currentAthleteAndTimePairs =
            currentAthleteAndTimePairs
                |> Maybe.withDefault []
                |> (\x -> List.append x athleteAndTimePairs)
                |> Just
    in
    Dict.update pos updater dict


mergeScannerDicts : Dict Int (List AthleteAndTimePair) -> Dict Int (List AthleteAndTimePair) -> Dict Int (List AthleteAndTimePair)
mergeScannerDicts dict1 dict2 =
    List.foldl mergeScannerDictEntry dict1 (Dict.toList dict2)


maxDate : Maybe Posix -> Maybe Posix -> Maybe Posix
maxDate maxDate1 maxDate2 =
    case ( maxDate1, maxDate2 ) of
        ( Just date1, Just date2 ) ->
            max (Time.posixToMillis date1) (Time.posixToMillis date2)
                |> Time.millisToPosix
                |> Just

        ( Just date1, Nothing ) ->
            maxDate1

        ( Nothing, _ ) ->
            maxDate2


withLastScanDate : BarcodeScannerData -> BarcodeScannerData
withLastScanDate barcodeScannerData =
    let
        allTimes : List String
        allTimes =
            List.concat
                [ Dict.values barcodeScannerData.scannedBarcodes
                    |> List.concat
                    |> List.map .scanTime
                , List.map .scanTime barcodeScannerData.athleteBarcodesOnly
                , List.map .scanTime barcodeScannerData.finishTokensOnly
                , List.map .scanTime barcodeScannerData.misScannedItems
                ]

        lastScanDate : Maybe Posix
        lastScanDate =
            allTimes
                |> List.filterMap dateStringToPosix
                |> List.map Time.posixToMillis
                |> List.maximum
                |> Maybe.map Time.millisToPosix
    in
    { barcodeScannerData | lastScanDate = lastScanDate }


withFile : String -> List BarcodeScannerFileLine -> BarcodeScannerData -> BarcodeScannerData
withFile filename lines barcodeScannerData =
    { barcodeScannerData | files = barcodeScannerData.files ++ [ BarcodeScannerFile filename lines barcodeScannerData.lastScanDate ] }


mergeScannerData : BarcodeScannerData -> BarcodeScannerData -> BarcodeScannerData
mergeScannerData data1 data2 =
    BarcodeScannerData
        (data1.files ++ data2.files)
        (mergeScannerDicts data1.scannedBarcodes data2.scannedBarcodes)
        (data1.athleteBarcodesOnly ++ data2.athleteBarcodesOnly)
        (data1.finishTokensOnly ++ data2.finishTokensOnly)
        (data1.misScannedItems ++ data2.misScannedItems)
        (data1.unrecognisedLines ++ data2.unrecognisedLines)
        (maxDate data1.lastScanDate data2.lastScanDate)


readBarcodeScannerData : String -> String -> Result Error BarcodeScannerData
readBarcodeScannerData filename text =
    if isPossibleBinary text then
        Error "BINARY_FILE" "File appears to be a binary file"
            |> Err

    else
        let
            lines : List (Result UnrecognisedLine BarcodeScannerFileLine)
            lines =
                text
                    |> splitLines
                    |> List.filter (not << String.isEmpty)
                    |> List.indexedMap (\index line -> readLine (index + 1) line)

            validLines : List BarcodeScannerFileLine
            validLines =
                List.filterMap Result.toMaybe lines

            unrecognisedLines : List UnrecognisedLine
            unrecognisedLines =
                List.filterMap toMaybeError lines

            withUnrecognisedLines : BarcodeScannerData -> BarcodeScannerData
            withUnrecognisedLines barcodeScannerData =
                { barcodeScannerData | unrecognisedLines = unrecognisedLines }
        in
        if List.isEmpty validLines && List.isEmpty unrecognisedLines then
            Error "NO_RESULTS" "Barcode scanner data contained no results"
                |> Err

        else
            mergeEntries validLines
                |> withUnrecognisedLines
                |> withLastScanDate
                |> withFile filename validLines
                |> Ok


maxFinishToken : BarcodeScannerData -> Maybe Int
maxFinishToken barcodeScannerData =
    Dict.keys barcodeScannerData.scannedBarcodes
        |> List.maximum


{-| Placeholder time (in milliseconds) used if a time fails to parse.
-}
placeholderMaxEventTime : Int
placeholderMaxEventTime =
    9999999999999


toScanTimeMillis : Timed a -> Int
toScanTimeMillis { scanTime } =
    dateStringToPosix scanTime
        |> Maybe.map Time.posixToMillis
        |> Maybe.withDefault placeholderMaxEventTime


sortByTime : List (Timed a) -> List (Timed a)
sortByTime times =
    List.sortBy toScanTimeMillis times


type alias TimedItem =
    { line : String
    , time : Int
    }


formatPosition : Int -> String
formatPosition position =
    let
        stringifiedPosition : String
        stringifiedPosition =
            String.fromInt position
    in
    "P" ++ String.repeat (4 - String.length stringifiedPosition) "0" ++ stringifiedPosition


formatAthleteEntries : ( Int, List AthleteAndTimePair ) -> List TimedItem
formatAthleteEntries ( position, athleteAndTimePairs ) =
    List.map
        (\athleteAndTimePair ->
            TimedItem
                (athleteAndTimePair.athlete ++ "," ++ formatPosition position ++ "," ++ athleteAndTimePair.scanTime)
                (toScanTimeMillis athleteAndTimePair)
        )
        athleteAndTimePairs


notDeleted : BarcodeScannerFileLine -> Bool
notDeleted line =
    case line.deletionStatus of
        Deleted _ ->
            False

        NotDeleted ->
            True


{-| Recreates the barcode-scanner data from the entries in the files.
-}
regenerate : BarcodeScannerData -> BarcodeScannerData
regenerate barcodeScannerData =
    let
        mergedData : BarcodeScannerData
        mergedData =
            List.map .lines barcodeScannerData.files
                |> List.concat
                |> List.filter notDeleted
                |> mergeEntries

        newMaxScanDate : Maybe Posix
        newMaxScanDate =
            List.filterMap .maxScanDate barcodeScannerData.files
                |> List.map Time.posixToMillis
                |> List.maximum
                |> Maybe.map Time.millisToPosix
    in
    { mergedData
        | unrecognisedLines = barcodeScannerData.unrecognisedLines
        , lastScanDate = newMaxScanDate
        , files = barcodeScannerData.files
    }


generateDownloadText : BarcodeScannerFile -> String
generateDownloadText file =
    let
        contentsToString : LineContents -> String
        contentsToString contents =
            case contents of
                Ordinary athlete Nothing ->
                    athlete ++ ","

                Ordinary athlete (Just position) ->
                    athlete ++ "," ++ formatPosition position

                MisScan text ->
                    text

        lineToString : BarcodeScannerFileLine -> String
        lineToString line =
            contentsToString line.contents ++ "," ++ line.scanTime ++ crlf
    in
    file.lines
        |> List.filter notDeleted
        |> List.map lineToString
        |> String.join ""
