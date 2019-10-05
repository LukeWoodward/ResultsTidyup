module TokenOperations exposing
    ( RangeEntry
    , TokenOperationChangeType(..)
    , TokenOperationEditDetails
    , TokenOperationOption(..)
    , TokenOperationValidationError(..)
    , TokenRange
    , TokenRangeField(..)
    , applyTokenOperationToBarcodeScannerData
    , emptyEditDetails
    , isInsertTokenRangeFieldInvalid
    , isRemoveTokenRangeFieldInvalid
    , isSwapTokenRange1FieldInvalid
    , isSwapTokenRange2FieldInvalid
    , parseRange
    , rangeToString
    , updateEditDetails
    , validateEditDetails
    )

import BarcodeScanner exposing (AthleteAndTimePair, BarcodeScannerData, PositionAndTimePair)
import Dict exposing (Dict)
import Set exposing (Set)


type alias TokenRange =
    { start : Int
    , end : Int
    }


type alias RangeEntry =
    { enteredText : String
    , range : Maybe TokenRange
    }


type TokenOperationOption
    = NoOptionSelected
    | InsertTokensOption
    | RemoveTokensOption
    | SwapTokenRangeOption


type TokenRangeField
    = InsertTokenRangeField
    | RemoveTokenRangeField
    | SwapTokenRangeField1
    | SwapTokenRangeField2


type TokenOperationValidationError
    = NoValidationError
    | TokenOperationNotSelected
    | InvalidRange TokenRangeField
    | EmptyRange TokenRangeField
    | InsertRangeOffEndOfTokens Int TokenRange
    | RangeOffEndOfTokens Int TokenRange TokenRangeField
    | RemovingExistingTokens (List Int) TokenRange
    | SwapTokenRangesOfDifferentSizes
    | SwapTokenRangesOverlap


type alias TokenOperationEditDetails =
    { operation : TokenOperationOption
    , insertTokenRange : RangeEntry
    , removeTokenRange : RangeEntry
    , swapTokenRange1 : RangeEntry
    , swapTokenRange2 : RangeEntry
    , validationError : TokenOperationValidationError
    }


type TokenOperationChangeType
    = ChangeOperation TokenOperationOption
    | RangeEdited TokenRangeField String


emptyRange : RangeEntry
emptyRange =
    RangeEntry "" Nothing


emptyEditDetails : TokenOperationEditDetails
emptyEditDetails =
    TokenOperationEditDetails NoOptionSelected emptyRange emptyRange emptyRange emptyRange NoValidationError


trimToInt : String -> Maybe Int
trimToInt string =
    String.toInt (String.trim string)


parseRange : String -> Maybe TokenRange
parseRange text =
    case String.split "-" text of
        [ singleString ] ->
            trimToInt singleString
                |> Maybe.map (\num -> TokenRange num num)

        [ firstString, secondString ] ->
            Maybe.map2 TokenRange (trimToInt firstString) (trimToInt secondString)

        _ ->
            Nothing


rangeToString : TokenRange -> String
rangeToString range =
    if range.start == range.end then
        String.fromInt range.start

    else
        String.fromInt range.start ++ "-" ++ String.fromInt range.end


commonTokenRangeValidation : TokenRangeField -> RangeEntry -> Maybe TokenOperationValidationError
commonTokenRangeValidation field entry =
    case entry.range of
        Just someRange ->
            if someRange.start > someRange.end then
                Just (EmptyRange field)

            else
                Nothing

        Nothing ->
            Just (InvalidRange field)


insertTokenRangeEndOffTokens : Int -> RangeEntry -> Maybe TokenOperationValidationError
insertTokenRangeEndOffTokens lastToken entry =
    case entry.range of
        Just someRange ->
            if lastToken < someRange.start && someRange.start <= someRange.end then
                Just (InsertRangeOffEndOfTokens lastToken someRange)

            else
                Nothing

        Nothing ->
            Nothing


tokenRangeEndOffTokens : Int -> TokenRangeField -> RangeEntry -> Maybe TokenOperationValidationError
tokenRangeEndOffTokens lastToken field entry =
    case entry.range of
        Just someRange ->
            if someRange.start <= someRange.end && someRange.end > lastToken then
                Just (RangeOffEndOfTokens lastToken someRange field)

            else
                Nothing

        Nothing ->
            Nothing


removingExistingTokens : Set Int -> RangeEntry -> Maybe TokenOperationValidationError
removingExistingTokens allTokens entry =
    case entry.range of
        Just someRange ->
            let
                removedExistingTokens : List Int
                removedExistingTokens =
                    List.range someRange.start someRange.end
                        |> Set.fromList
                        |> Set.intersect allTokens
                        |> Set.toList
                        |> List.sort
            in
            if List.isEmpty removedExistingTokens then
                Nothing

            else
                Just (RemovingExistingTokens removedExistingTokens someRange)

        Nothing ->
            Nothing


rangesOfDifferentSizeValidation : RangeEntry -> RangeEntry -> Maybe TokenOperationValidationError
rangesOfDifferentSizeValidation swapTokenEntry1 swapTokenEntry2 =
    case ( swapTokenEntry1.range, swapTokenEntry2.range ) of
        ( Just range1, Just range2 ) ->
            if range1.end - range1.start == range2.end - range2.start then
                Nothing

            else
                Just SwapTokenRangesOfDifferentSizes

        _ ->
            Nothing


rangeTokenOverlapValidation : RangeEntry -> RangeEntry -> Maybe TokenOperationValidationError
rangeTokenOverlapValidation swapTokenEntry1 swapTokenEntry2 =
    case ( swapTokenEntry1.range, swapTokenEntry2.range ) of
        ( Just range1, Just range2 ) ->
            if
                -- Two ranges overlap if each starts are less than or equal to each end.
                -- (The overlap range is the greater start to the lesser end.)
                (range1.start <= range1.end && range2.start <= range2.end)
                    && (range1.start <= range2.end && range2.start <= range1.end)
            then
                Just SwapTokenRangesOverlap

            else
                Nothing

        _ ->
            Nothing


validateEditDetails : Set Int -> TokenOperationEditDetails -> TokenOperationValidationError
validateEditDetails allTokens editDetails =
    let
        lastToken : Int
        lastToken =
            Set.toList allTokens
                |> List.maximum
                |> Maybe.withDefault 0

        allErrors : List (Maybe TokenOperationValidationError)
        allErrors =
            case editDetails.operation of
                NoOptionSelected ->
                    [ Just TokenOperationNotSelected ]

                InsertTokensOption ->
                    [ commonTokenRangeValidation InsertTokenRangeField editDetails.insertTokenRange
                    , insertTokenRangeEndOffTokens lastToken editDetails.insertTokenRange
                    ]

                RemoveTokensOption ->
                    [ commonTokenRangeValidation RemoveTokenRangeField editDetails.removeTokenRange
                    , tokenRangeEndOffTokens lastToken RemoveTokenRangeField editDetails.removeTokenRange
                    , removingExistingTokens allTokens editDetails.removeTokenRange
                    ]

                SwapTokenRangeOption ->
                    [ commonTokenRangeValidation SwapTokenRangeField1 editDetails.swapTokenRange1
                    , tokenRangeEndOffTokens lastToken SwapTokenRangeField1 editDetails.swapTokenRange1
                    , commonTokenRangeValidation SwapTokenRangeField2 editDetails.swapTokenRange2
                    , tokenRangeEndOffTokens lastToken SwapTokenRangeField2 editDetails.swapTokenRange2
                    , rangesOfDifferentSizeValidation editDetails.swapTokenRange1 editDetails.swapTokenRange2
                    , rangeTokenOverlapValidation editDetails.swapTokenRange1 editDetails.swapTokenRange2
                    ]
    in
    List.filterMap identity allErrors
        |> List.head
        |> Maybe.withDefault NoValidationError


rangeEntryFromString : String -> RangeEntry
rangeEntryFromString rangeString =
    { enteredText = rangeString, range = parseRange rangeString }


updateEditDetails : Set Int -> TokenOperationChangeType -> TokenOperationEditDetails -> TokenOperationEditDetails
updateEditDetails allTokens change editDetails =
    let
        updatedDetails : TokenOperationEditDetails
        updatedDetails =
            case change of
                ChangeOperation newOperation ->
                    { editDetails | operation = newOperation }

                RangeEdited InsertTokenRangeField newValue ->
                    { editDetails | insertTokenRange = rangeEntryFromString newValue }

                RangeEdited RemoveTokenRangeField newValue ->
                    { editDetails | removeTokenRange = rangeEntryFromString newValue }

                RangeEdited SwapTokenRangeField1 newValue ->
                    { editDetails | swapTokenRange1 = rangeEntryFromString newValue }

                RangeEdited SwapTokenRangeField2 newValue ->
                    { editDetails | swapTokenRange2 = rangeEntryFromString newValue }
    in
    { updatedDetails | validationError = validateEditDetails allTokens updatedDetails }


isTokenRangeFieldInvalid : TokenRangeField -> TokenOperationEditDetails -> Bool
isTokenRangeFieldInvalid field tokenOperationEditDetails =
    case tokenOperationEditDetails.validationError of
        NoValidationError ->
            False

        TokenOperationNotSelected ->
            False

        InvalidRange errorField ->
            errorField == field

        EmptyRange errorField ->
            errorField == field

        InsertRangeOffEndOfTokens _ _ ->
            field == InsertTokenRangeField

        RangeOffEndOfTokens _ _ errorField ->
            errorField == field

        RemovingExistingTokens _ _ ->
            field == RemoveTokenRangeField

        SwapTokenRangesOfDifferentSizes ->
            field == SwapTokenRangeField1 || field == SwapTokenRangeField2

        SwapTokenRangesOverlap ->
            field == SwapTokenRangeField1 || field == SwapTokenRangeField2


isInsertTokenRangeFieldInvalid : TokenOperationEditDetails -> Bool
isInsertTokenRangeFieldInvalid tokenOperationEditDetails =
    isTokenRangeFieldInvalid InsertTokenRangeField tokenOperationEditDetails


isRemoveTokenRangeFieldInvalid : TokenOperationEditDetails -> Bool
isRemoveTokenRangeFieldInvalid tokenOperationEditDetails =
    isTokenRangeFieldInvalid RemoveTokenRangeField tokenOperationEditDetails


isSwapTokenRange1FieldInvalid : TokenOperationEditDetails -> Bool
isSwapTokenRange1FieldInvalid tokenOperationEditDetails =
    isTokenRangeFieldInvalid SwapTokenRangeField1 tokenOperationEditDetails


isSwapTokenRange2FieldInvalid : TokenOperationEditDetails -> Bool
isSwapTokenRange2FieldInvalid tokenOperationEditDetails =
    isTokenRangeFieldInvalid SwapTokenRangeField2 tokenOperationEditDetails


insertTokensIntoBarcodeScannerData : TokenRange -> BarcodeScannerData -> BarcodeScannerData
insertTokensIntoBarcodeScannerData range barcodeScannerData =
    let
        offset : Int
        offset =
            range.end - range.start + 1

        mapFinishTokenOnly : PositionAndTimePair -> PositionAndTimePair
        mapFinishTokenOnly pair =
            if pair.position < range.start then
                pair

            else
                { pair | position = pair.position + offset }

        mapScannedBarcodeEntry : ( Int, List AthleteAndTimePair ) -> ( Int, List AthleteAndTimePair )
        mapScannedBarcodeEntry ( token, athletes ) =
            if token < range.start then
                ( token, athletes )

            else
                ( token + offset, athletes )
    in
    { barcodeScannerData
        | scannedBarcodes =
            barcodeScannerData.scannedBarcodes
                |> Dict.toList
                |> List.map mapScannedBarcodeEntry
                |> Dict.fromList
        , finishTokensOnly = List.map mapFinishTokenOnly barcodeScannerData.finishTokensOnly
    }


removeTokensFromBarcodeScannerData : TokenRange -> BarcodeScannerData -> BarcodeScannerData
removeTokensFromBarcodeScannerData range barcodeScannerData =
    let
        offset : Int
        offset =
            range.end - range.start + 1

        filterPosition : Int -> Bool
        filterPosition position =
            not (range.start <= position && position <= range.end)

        filterFinishTokenOnly : PositionAndTimePair -> Bool
        filterFinishTokenOnly pair =
            filterPosition pair.position

        mapFinishTokenOnly : PositionAndTimePair -> PositionAndTimePair
        mapFinishTokenOnly pair =
            if pair.position <= range.end then
                pair

            else
                { pair | position = pair.position - offset }

        filterScannedBarcodeEntry : ( Int, List AthleteAndTimePair ) -> Bool
        filterScannedBarcodeEntry ( token, _ ) =
            filterPosition token

        mapScannedBarcodeEntry : ( Int, List AthleteAndTimePair ) -> ( Int, List AthleteAndTimePair )
        mapScannedBarcodeEntry ( token, athletes ) =
            if token <= range.end then
                ( token, athletes )

            else
                ( token - offset, athletes )
    in
    { barcodeScannerData
        | scannedBarcodes =
            barcodeScannerData.scannedBarcodes
                |> Dict.toList
                |> List.filter filterScannedBarcodeEntry
                |> List.map mapScannedBarcodeEntry
                |> Dict.fromList
        , finishTokensOnly =
            barcodeScannerData.finishTokensOnly
                |> List.filter filterFinishTokenOnly
                |> List.map mapFinishTokenOnly
    }


swapTokensInBarcodeScannerData : TokenRange -> Int -> BarcodeScannerData -> BarcodeScannerData
swapTokensInBarcodeScannerData range1 range2Start barcodeScannerData =
    let
        range2End : Int
        range2End =
            range2Start + range1.end - range1.start

        oneToTwoOffset : Int
        oneToTwoOffset =
            range2Start - range1.start

        twoToOneOffset : Int
        twoToOneOffset =
            -oneToTwoOffset

        mapFinishTokenOnly : PositionAndTimePair -> PositionAndTimePair
        mapFinishTokenOnly pair =
            if range1.start <= pair.position && pair.position <= range1.end then
                { pair | position = pair.position + oneToTwoOffset }

            else if range2Start <= pair.position && pair.position <= range2End then
                { pair | position = pair.position + twoToOneOffset }

            else
                pair

        mapScannedBarcodeEntry : ( Int, List AthleteAndTimePair ) -> ( Int, List AthleteAndTimePair )
        mapScannedBarcodeEntry ( token, athletes ) =
            if range1.start <= token && token <= range1.end then
                ( token + oneToTwoOffset, athletes )

            else if range2Start <= token && token <= range2End then
                ( token + twoToOneOffset, athletes )

            else
                ( token, athletes )
    in
    { barcodeScannerData
        | scannedBarcodes =
            barcodeScannerData.scannedBarcodes
                |> Dict.toList
                |> List.map mapScannedBarcodeEntry
                |> Dict.fromList
        , finishTokensOnly =
            List.map mapFinishTokenOnly barcodeScannerData.finishTokensOnly
                |> List.sortBy .position
    }


applyTokenOperationToBarcodeScannerData : TokenOperationEditDetails -> BarcodeScannerData -> BarcodeScannerData
applyTokenOperationToBarcodeScannerData tokenOperationEditDetails barcodeScannerData =
    if tokenOperationEditDetails.validationError == NoValidationError then
        case tokenOperationEditDetails.operation of
            NoOptionSelected ->
                barcodeScannerData

            InsertTokensOption ->
                case tokenOperationEditDetails.insertTokenRange.range of
                    Just range ->
                        insertTokensIntoBarcodeScannerData range barcodeScannerData

                    Nothing ->
                        barcodeScannerData

            RemoveTokensOption ->
                case tokenOperationEditDetails.removeTokenRange.range of
                    Just range ->
                        removeTokensFromBarcodeScannerData range barcodeScannerData

                    Nothing ->
                        barcodeScannerData

            SwapTokenRangeOption ->
                case ( tokenOperationEditDetails.swapTokenRange1.range, tokenOperationEditDetails.swapTokenRange2.range ) of
                    ( Just range1, Just range2 ) ->
                        if range2.end - range2.start == range1.end - range1.start then
                            swapTokensInBarcodeScannerData range1 range2.start barcodeScannerData

                        else
                            -- Validation on range sizes being equal should have been done by
                            -- the time we get here, but if not, let's just do nothing.
                            barcodeScannerData

                    _ ->
                        barcodeScannerData

    else
        barcodeScannerData
