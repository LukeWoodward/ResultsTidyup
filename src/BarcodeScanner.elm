module BarcodeScanner exposing
    ( AthleteAndTimePair
    , BarcodeScannerData
    , BarcodeScannerFile
    , BarcodeScannerFileLine
    , DeletionReason(..)
    , DeletionStatus(..)
    , LineContents(..)
    , UnrecognisedLine
    , allTokensUsed
    , deleteBarcodeScannerLine
    , empty
    , generateDownloadText
    , generateDownloadTextForAllScanners
    , isEmpty
    , maxFinishToken
    , mergeScannerData
    , readBarcodeScannerData
    , regenerate
    , updateBarcodeScannerLine
    )

import DateHandling exposing (dateTimeStringToPosix)
import Dict exposing (Dict)
import Error exposing (Error)
import FileHandling exposing (AddedFile, crlf, isPossibleBinary, splitLines)
import Parser exposing ((|.), Parser, end, int, run, symbol)
import Parsers exposing (digitsRange)
import Result.Extra
import Set exposing (Set)
import Time exposing (Posix)


type alias AthleteAndTimePair =
    { athlete : String
    , scanDateTime : String
    }


type alias UnrecognisedLine =
    { line : String
    , errorCode : String
    , errorMessage : String
    }


type DeletionReason
    = DuplicateScan String Int
    | AthleteScannedWithFinishTokenElsewhere String
    | DeletedByUser


type DeletionStatus
    = NotDeleted
    | Deleted DeletionReason


type LineContents
    = Ordinary String (Maybe Int)


type alias BarcodeScannerFileLine =
    { lineNumber : Int
    , contents : LineContents
    , scanDateTime : String
    , deletionStatus : DeletionStatus
    }


type alias BarcodeScannerFile =
    { filename : String
    , name : String
    , lines : List BarcodeScannerFileLine
    , maxScanDateTime : Maybe Posix
    }


type alias BarcodeScannerData =
    { files : List BarcodeScannerFile
    , scannedBarcodes : Dict Int (List AthleteAndTimePair)
    , athleteBarcodesOnly : List AthleteAndTimePair
    , unrecognisedLines : List UnrecognisedLine
    , lastScanDateTime : Maybe Posix
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
    BarcodeScannerData [] Dict.empty [] [] Nothing


isEmpty : BarcodeScannerData -> Bool
isEmpty barcodeScannerData =
    Dict.isEmpty barcodeScannerData.scannedBarcodes
        && List.isEmpty barcodeScannerData.athleteBarcodesOnly


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
okDefaultFileLine lineNumber contents scanDateTime =
    Ok (BarcodeScannerFileLine lineNumber contents scanDateTime NotDeleted)


removeNull : String -> String
removeNull value =
    if value == "null" then
        ""

    else
        value


readLine : Int -> String -> Result UnrecognisedLine BarcodeScannerFileLine
readLine lineNumber line =
    let
        parts : List String
        parts =
            String.split "," line
                |> List.map String.trim
                |> List.map removeNull

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

            else if positionNumber == Just 0 then
                unrecognisedLine "INVALID_POSITION_ZERO" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")

            else
                okDefaultFileLine lineNumber (Ordinary athlete positionNumber) time

        _ ->
            unrecognisedLine "NOT_TWO_OR_THREE_PARTS" ("Line " ++ line ++ " does not contain the expected two or three comma-separated parts")


mergeEntry : BarcodeScannerFileLine -> BarcodeScannerData -> BarcodeScannerData
mergeEntry line barcodeData =
    case line.contents of
        Ordinary "" _ ->
            -- Unexpected, do nothing.
            barcodeData

        Ordinary athlete Nothing ->
            { barcodeData | athleteBarcodesOnly = List.append barcodeData.athleteBarcodesOnly [ AthleteAndTimePair athlete line.scanDateTime ] }

        Ordinary athlete (Just pos) ->
            let
                updater : Maybe (List AthleteAndTimePair) -> Maybe (List AthleteAndTimePair)
                updater currentValue =
                    currentValue
                        |> Maybe.withDefault []
                        |> (\x -> List.append x [ AthleteAndTimePair athlete line.scanDateTime ])
                        |> Just
            in
            { barcodeData | scannedBarcodes = Dict.update pos updater barcodeData.scannedBarcodes }


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

        ( Just _, Nothing ) ->
            maxDate1

        ( Nothing, _ ) ->
            maxDate2


withLastScanDateTime : BarcodeScannerData -> BarcodeScannerData
withLastScanDateTime barcodeScannerData =
    let
        allDateTimes : List String
        allDateTimes =
            List.concat
                [ Dict.values barcodeScannerData.scannedBarcodes
                    |> List.concat
                    |> List.map .scanDateTime
                , List.map .scanDateTime barcodeScannerData.athleteBarcodesOnly
                ]

        lastScanDateTime : Maybe Posix
        lastScanDateTime =
            allDateTimes
                |> List.filterMap dateTimeStringToPosix
                |> List.map Time.posixToMillis
                |> List.maximum
                |> Maybe.map Time.millisToPosix
    in
    { barcodeScannerData | lastScanDateTime = lastScanDateTime }


withFile : String -> String -> List BarcodeScannerFileLine -> BarcodeScannerData -> BarcodeScannerData
withFile filename name lines barcodeScannerData =
    { barcodeScannerData | files = barcodeScannerData.files ++ [ BarcodeScannerFile filename name lines barcodeScannerData.lastScanDateTime ] }


mergeScannerData : BarcodeScannerData -> BarcodeScannerData -> BarcodeScannerData
mergeScannerData data1 data2 =
    BarcodeScannerData
        (data1.files ++ data2.files)
        (mergeScannerDicts data1.scannedBarcodes data2.scannedBarcodes)
        (data1.athleteBarcodesOnly ++ data2.athleteBarcodesOnly)
        (data1.unrecognisedLines ++ data2.unrecognisedLines)
        (maxDate data1.lastScanDateTime data2.lastScanDateTime)


readBarcodeScannerData : AddedFile -> Result Error BarcodeScannerData
readBarcodeScannerData addedFile =
    if isPossibleBinary addedFile.fileText then
        Error "BINARY_FILE" "File appears to be a binary file"
            |> Err

    else
        let
            lines : List (Result UnrecognisedLine BarcodeScannerFileLine)
            lines =
                addedFile.fileText
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
                |> withLastScanDateTime
                |> withFile addedFile.fileName addedFile.name validLines
                |> Ok


maxFinishToken : BarcodeScannerData -> Maybe Int
maxFinishToken barcodeScannerData =
    Dict.keys barcodeScannerData.scannedBarcodes
        |> List.maximum


formatPosition : Int -> String
formatPosition position =
    let
        stringifiedPosition : String
        stringifiedPosition =
            String.fromInt position
    in
    "P" ++ String.repeat (4 - String.length stringifiedPosition) "0" ++ stringifiedPosition


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

        newMaxScanDateTime : Maybe Posix
        newMaxScanDateTime =
            List.filterMap .maxScanDateTime barcodeScannerData.files
                |> List.map Time.posixToMillis
                |> List.maximum
                |> Maybe.map Time.millisToPosix
    in
    { mergedData
        | unrecognisedLines = barcodeScannerData.unrecognisedLines
        , lastScanDateTime = newMaxScanDateTime
        , files = barcodeScannerData.files
    }


lineContentsToString : LineContents -> String
lineContentsToString contents =
    case contents of
        Ordinary athlete Nothing ->
            athlete ++ ","

        Ordinary athlete (Just position) ->
            athlete ++ "," ++ formatPosition position


lineToString : BarcodeScannerFileLine -> String
lineToString line =
    lineContentsToString line.contents ++ "," ++ line.scanDateTime ++ crlf


generateDownloadText : BarcodeScannerFile -> String
generateDownloadText file =
    file.lines
        |> List.filter notDeleted
        |> List.map lineToString
        |> String.join ""


generateDownloadTextForAllScanners : List BarcodeScannerFile -> String
generateDownloadTextForAllScanners files =
    let
        lineSortKey : BarcodeScannerFileLine -> Int
        lineSortKey line =
            dateTimeStringToPosix line.scanDateTime
                |> Maybe.map Time.posixToMillis
                |> Maybe.withDefault 99999999999999
    in
    files
        |> List.map .lines
        |> List.concat
        |> List.filter notDeleted
        |> List.sortBy lineSortKey
        |> List.map lineToString
        |> String.join ""


applyBarcodeScannerDataModification : (BarcodeScannerFileLine -> BarcodeScannerFileLine) -> String -> Int -> BarcodeScannerData -> BarcodeScannerData
applyBarcodeScannerDataModification modifier fileName lineNumber barcodeScannerData =
    let
        applyLineModification : BarcodeScannerFileLine -> BarcodeScannerFileLine
        applyLineModification line =
            if line.lineNumber == lineNumber then
                modifier line

            else
                line

        applyFileModification : BarcodeScannerFile -> BarcodeScannerFile
        applyFileModification file =
            if file.filename == fileName then
                { file | lines = List.map applyLineModification file.lines }

            else
                file
    in
    { barcodeScannerData | files = List.map applyFileModification barcodeScannerData.files }
        |> regenerate


updateBarcodeScannerLine : String -> Int -> String -> Maybe Int -> BarcodeScannerData -> BarcodeScannerData
updateBarcodeScannerLine fileName lineNumber athlete finishPosition barcodeScannerData =
    let
        updateLine : BarcodeScannerFileLine -> BarcodeScannerFileLine
        updateLine line =
            { line | contents = Ordinary athlete finishPosition, deletionStatus = NotDeleted }
    in
    applyBarcodeScannerDataModification updateLine fileName lineNumber barcodeScannerData


deleteBarcodeScannerLine : String -> Int -> BarcodeScannerData -> BarcodeScannerData
deleteBarcodeScannerLine fileName lineNumber barcodeScannerData =
    let
        deleteLine : BarcodeScannerFileLine -> BarcodeScannerFileLine
        deleteLine line =
            { line | deletionStatus = Deleted DeletedByUser }
    in
    applyBarcodeScannerDataModification deleteLine fileName lineNumber barcodeScannerData


allTokensUsed : BarcodeScannerData -> Set Int
allTokensUsed barcodeScannerData =
    Dict.keys barcodeScannerData.scannedBarcodes
        |> Set.fromList
