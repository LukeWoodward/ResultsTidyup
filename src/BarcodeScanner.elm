module BarcodeScanner exposing (BarcodeScannerData, empty, isEmpty, maxFinishToken, mergeScannerData, readBarcodeScannerData)

import Dict exposing (Dict)
import Error exposing (Error)
import FileHandling exposing (isPossibleBinary, splitLines)
import Regex exposing (Regex)
import Result.Extra


type alias BarcodeScannerData =
    { scannedBarcodes : Dict Int (List String)
    , athleteBarcodesOnly : List String
    , finishTokensOnly : List Int
    }


type BarcodeScannerEntry
    = Successful String Int
    | AthleteOnly String
    | FinishTokenOnly Int


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
        [ athlete, position, _ ] ->
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
                Ok (AthleteOnly athlete)

            else
                case positionNumber of
                    Just 0 ->
                        Error "INVALID_POSITION_ZERO" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")
                            |> Err

                    Just pos ->
                        if isAthleteMissing then
                            Ok (FinishTokenOnly pos)

                        else
                            Ok (Successful athlete pos)

                    Nothing ->
                        Error "NON_NUMERIC_POSITION" ("Invalid position record '" ++ position ++ "' found in barcode scanner file")
                            |> Err

        _ ->
            Error "NOT_THREE_PARTS" ("Line " ++ line ++ " does not contain the expected three comma-separated parts")
                |> Err


mergeEntry : BarcodeScannerEntry -> BarcodeScannerData -> BarcodeScannerData
mergeEntry entry barcodeData =
    case entry of
        Successful athlete pos ->
            let
                updater : Maybe (List String) -> Maybe (List String)
                updater currentValue =
                    currentValue
                        |> Maybe.withDefault []
                        |> List.append [ athlete ]
                        |> Just
            in
            { barcodeData | scannedBarcodes = Dict.update pos updater barcodeData.scannedBarcodes }

        AthleteOnly athlete ->
            { barcodeData | athleteBarcodesOnly = List.append barcodeData.athleteBarcodesOnly [ athlete ] }

        FinishTokenOnly finishToken ->
            { barcodeData | finishTokensOnly = List.append barcodeData.finishTokensOnly [ finishToken ] }


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


mergeScannerDictEntry : ( Int, List String ) -> Dict Int (List String) -> Dict Int (List String)
mergeScannerDictEntry ( pos, athletes ) dict =
    let
        updater : Maybe (List String) -> Maybe (List String)
        updater currentAthletes =
            currentAthletes
                |> Maybe.withDefault []
                |> (\x -> List.append x athletes)
                |> Just
    in
    Dict.update pos updater dict


mergeScannerDicts : Dict Int (List String) -> Dict Int (List String) -> Dict Int (List String)
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
