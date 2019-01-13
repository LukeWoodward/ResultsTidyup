module BarcodeScanner exposing
    ( AthleteAndTimePair
    , BarcodeScannerData
    , MisScannedItem
    , PositionAndTimePair
    , UnrecognisedLine
    , empty
    , isEmpty
    , maxFinishToken
    , mergeScannerData
    , readBarcodeScannerData
    , toDownloadText
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


type alias BarcodeScannerData =
    { scannedBarcodes : Dict Int (List AthleteAndTimePair)
    , athleteBarcodesOnly : List AthleteAndTimePair
    , finishTokensOnly : List PositionAndTimePair
    , misScannedItems : List MisScannedItem
    , unrecognisedLines : List UnrecognisedLine
    , lastScanDate : Maybe Posix
    }


type BarcodeScannerEntry
    = Successful String Int String
    | AthleteOnly String String
    | FinishTokenOnly Int String
    | MisScanned String String


toMaybeError : Result e x -> Maybe e
toMaybeError result =
    case result of
        Ok _ ->
            Nothing

        Err error ->
            Just error


empty : BarcodeScannerData
empty =
    BarcodeScannerData Dict.empty [] [] [] [] Nothing


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


readLine : String -> Result UnrecognisedLine BarcodeScannerEntry
readLine line =
    let
        parts : List String
        parts =
            String.split "," line

        unrecognisedLine : String -> String -> Result UnrecognisedLine BarcodeScannerEntry
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
                Ok (AthleteOnly athlete time)

            else
                case positionNumber of
                    Just 0 ->
                        unrecognisedLine "INVALID_POSITION_ZERO" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")

                    Just pos ->
                        if isAthleteMissing then
                            Ok (FinishTokenOnly pos time)

                        else
                            Ok (Successful athlete pos time)

                    Nothing ->
                        unrecognisedLine "NON_NUMERIC_POSITION" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")

        [ misScannedText, time ] ->
            Ok (MisScanned misScannedText time)

        _ ->
            unrecognisedLine "NOT_TWO_OR_THREE_PARTS" ("Line " ++ line ++ " does not contain the expected two or three comma-separated parts")


mergeEntry : BarcodeScannerEntry -> BarcodeScannerData -> BarcodeScannerData
mergeEntry entry barcodeData =
    case entry of
        Successful athlete pos time ->
            let
                updater : Maybe (List AthleteAndTimePair) -> Maybe (List AthleteAndTimePair)
                updater currentValue =
                    currentValue
                        |> Maybe.withDefault []
                        |> List.append [ AthleteAndTimePair athlete time ]
                        |> Just
            in
            { barcodeData | scannedBarcodes = Dict.update pos updater barcodeData.scannedBarcodes }

        AthleteOnly athlete time ->
            { barcodeData | athleteBarcodesOnly = List.append barcodeData.athleteBarcodesOnly [ AthleteAndTimePair athlete time ] }

        FinishTokenOnly finishToken time ->
            { barcodeData | finishTokensOnly = List.append barcodeData.finishTokensOnly [ PositionAndTimePair finishToken time ] }

        MisScanned misScannedText time ->
            { barcodeData | misScannedItems = List.append barcodeData.misScannedItems [ MisScannedItem misScannedText time ] }


mergeEntries : List BarcodeScannerEntry -> BarcodeScannerData
mergeEntries scannerEntries =
    List.foldr mergeEntry empty scannerEntries


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
    List.foldr mergeScannerDictEntry dict1 (Dict.toList dict2)


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


mergeScannerData : BarcodeScannerData -> BarcodeScannerData -> BarcodeScannerData
mergeScannerData data1 data2 =
    BarcodeScannerData
        (mergeScannerDicts data1.scannedBarcodes data2.scannedBarcodes)
        (data1.athleteBarcodesOnly ++ data2.athleteBarcodesOnly)
        (data1.finishTokensOnly ++ data2.finishTokensOnly)
        (data1.misScannedItems ++ data2.misScannedItems)
        (data1.unrecognisedLines ++ data2.unrecognisedLines)
        (maxDate data1.lastScanDate data2.lastScanDate)


readBarcodeScannerData : String -> Result Error BarcodeScannerData
readBarcodeScannerData text =
    if isPossibleBinary text then
        Error "BINARY_FILE" "File appears to be a binary file"
            |> Err

    else
        let
            lines : List (Result UnrecognisedLine BarcodeScannerEntry)
            lines =
                text
                    |> splitLines
                    |> List.filter (not << String.isEmpty)
                    |> List.map readLine

            validLines : List BarcodeScannerEntry
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


toDownloadText : BarcodeScannerData -> String
toDownloadText barcodeScannerData =
    let
        scannedBarcodeItems : List TimedItem
        scannedBarcodeItems =
            Dict.toList barcodeScannerData.scannedBarcodes
                |> List.map formatAthleteEntries
                |> List.concat

        athleteBarcodeOnlyItems : List TimedItem
        athleteBarcodeOnlyItems =
            List.map
                (\athleteAndTimePair ->
                    TimedItem
                        (athleteAndTimePair.athlete ++ ",," ++ athleteAndTimePair.scanTime)
                        (toScanTimeMillis athleteAndTimePair)
                )
                barcodeScannerData.athleteBarcodesOnly

        finishTokensOnlyItems : List TimedItem
        finishTokensOnlyItems =
            List.map
                (\positionAndTimePair ->
                    TimedItem
                        ("," ++ formatPosition positionAndTimePair.position ++ "," ++ positionAndTimePair.scanTime)
                        (toScanTimeMillis positionAndTimePair)
                )
                barcodeScannerData.finishTokensOnly

        misScanItems : List TimedItem
        misScanItems =
            List.map
                (\misScanItem ->
                    TimedItem
                        (misScanItem.scannedText ++ "," ++ misScanItem.scanTime)
                        (toScanTimeMillis misScanItem)
                )
                barcodeScannerData.misScannedItems
    in
    List.concat [ scannedBarcodeItems, athleteBarcodeOnlyItems, finishTokensOnlyItems, misScanItems ]
        |> List.sortBy .time
        |> List.map (\item -> item.line ++ crlf)
        |> String.join ""
