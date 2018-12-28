module BarcodeScanner exposing (AthleteAndTimePair, BarcodeScannerData, PositionAndTimePair, empty, isEmpty, maxFinishToken, mergeScannerData, readBarcodeScannerData)

import Dict exposing (Dict)
import Error exposing (Error)
import FileHandling exposing (isPossibleBinary, splitLines)
import Regex exposing (Regex)
import Result.Extra


type alias AthleteAndTimePair =
    { athlete : String
    , scanTime : String
    }


type alias PositionAndTimePair =
    { position : Int
    , scanTime : String
    }


type alias BarcodeScannerData =
    { scannedBarcodes : Dict Int (List AthleteAndTimePair)
    , athleteBarcodesOnly : List AthleteAndTimePair
    , finishTokensOnly : List PositionAndTimePair
    }


type BarcodeScannerEntry
    = Successful String Int String
    | AthleteOnly String String
    | FinishTokenOnly Int String


empty : BarcodeScannerData
empty =
    BarcodeScannerData Dict.empty [] []


isEmpty : BarcodeScannerData -> Bool
isEmpty barcodeScannerData =
    Dict.isEmpty barcodeScannerData.scannedBarcodes
        && List.isEmpty barcodeScannerData.athleteBarcodesOnly
        && List.isEmpty barcodeScannerData.finishTokensOnly


athleteRegex : Regex
athleteRegex =
    Regex.fromString "^A[0-9]+$"
        |> Maybe.withDefault Regex.never


positionRegex : Regex
positionRegex =
    Regex.fromString "^P[0-9]+$"
        |> Maybe.withDefault Regex.never


readLine : String -> Result Error BarcodeScannerEntry
readLine line =
    let
        parts : List String
        parts =
            String.split "," line
    in
    case parts of
        [ athlete, position, time ] ->
            let
                isAthleteMissing : Bool
                isAthleteMissing =
                    athlete == ""

                hasInvalidAthlete : Bool
                hasInvalidAthlete =
                    not isAthleteMissing && not (Regex.contains athleteRegex athlete)

                isPositionMissing : Bool
                isPositionMissing =
                    position == ""

                hasInvalidPosition : Bool
                hasInvalidPosition =
                    not isPositionMissing && not (Regex.contains positionRegex position)

                positionNumber : Maybe Int
                positionNumber =
                    if hasInvalidPosition then
                        Nothing

                    else
                        String.toInt (String.dropLeft 1 position)
            in
            if hasInvalidAthlete then
                Error "INVALID_ATHLETE_RECORD" ("Invalid athlete record '" ++ athlete ++ "' found in barcode scanner file")
                    |> Err

            else if hasInvalidPosition then
                Error "INVALID_POSITION_RECORD" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")
                    |> Err

            else if isPositionMissing && isAthleteMissing then
                Error "ATHLETE_AND_FINISH_TOKEN_MISSING"
                    ("Barcode scanner file contains line '"
                        ++ line
                        ++ "' with neither athlete nor finish token"
                    )
                    |> Err

            else if isPositionMissing then
                Ok (AthleteOnly athlete time)

            else
                case positionNumber of
                    Just 0 ->
                        Error "INVALID_POSITION_ZERO" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")
                            |> Err

                    Just pos ->
                        if isAthleteMissing then
                            Ok (FinishTokenOnly pos time)

                        else
                            Ok (Successful athlete pos time)

                    Nothing ->
                        Error "NON_NUMERIC_POSITION" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")
                            |> Err

        _ ->
            Error "NOT_THREE_PARTS" ("Line " ++ line ++ " does not contain the expected three comma-separated parts")
                |> Err


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


mergeEntries : List BarcodeScannerEntry -> BarcodeScannerData
mergeEntries scannerEntries =
    List.foldr mergeEntry (BarcodeScannerData Dict.empty [] []) scannerEntries


failIfNoResults : List a -> Result Error (List a)
failIfNoResults results =
    if List.isEmpty results then
        Error "NO_RESULTS" "Barcode scanner data contained no results"
            |> Err

    else
        Ok results


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


mergeScannerData : BarcodeScannerData -> BarcodeScannerData -> BarcodeScannerData
mergeScannerData data1 data2 =
    BarcodeScannerData
        (mergeScannerDicts data1.scannedBarcodes data2.scannedBarcodes)
        (data1.athleteBarcodesOnly ++ data2.athleteBarcodesOnly)
        (data1.finishTokensOnly ++ data2.finishTokensOnly)


readBarcodeScannerData : String -> Result Error BarcodeScannerData
readBarcodeScannerData text =
    if isPossibleBinary text then
        Error "BINARY_FILE" "File appears to be a binary file"
            |> Err

    else
        text
            |> splitLines
            |> List.filter (not << String.isEmpty)
            |> List.map readLine
            |> Result.Extra.combine
            |> Result.andThen failIfNoResults
            |> Result.map mergeEntries


maxFinishToken : BarcodeScannerData -> Maybe Int
maxFinishToken barcodeScannerData =
    Dict.keys barcodeScannerData.scannedBarcodes
        |> List.maximum
