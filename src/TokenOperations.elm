module TokenOperations exposing
    ( RangeEntry
    , TokenOperationChangeType(..)
    , TokenOperationEditDetails
    , TokenOperationOption(..)
    , TokenOperationValidationError(..)
    , TokenRange
    , TokenRangeField(..)
    , emptyEditDetails
    , isInsertTokenRangeFieldInvalid
    , isRemoveTokenRangeFieldInvalid
    , isReverseTokenRangeFieldInvalid
    , isSwapTokenRange1FieldInvalid
    , isSwapTokenRange2FieldInvalid
    , parseRange
    , rangeToString
    , tryApplyTokenOperationToBarcodeScannerData
    , updateEditDetails
    , validateEditDetails
    )

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        , PositionAndTimePair
        , allTokensUsed
        , regenerate
        )
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
    | ReverseTokenRangeOption


type TokenRangeField
    = InsertTokenRangeField
    | RemoveTokenRangeField
    | SwapTokenRangeField1
    | SwapTokenRangeField2
    | ReverseTokenRangeField


type TokenOperationValidationError
    = NoValidationError
    | TokenOperationNotSelected
    | InvalidRange TokenRangeField
    | EmptyRange TokenRangeField
    | ZeroInRange TokenRangeField
    | InsertRangeOffEndOfTokens Int TokenRange
    | RangeOffEndOfTokens Int TokenRange TokenRangeField
    | RemovingExistingTokens (List Int) TokenRange
    | SwapTokenRangesOfDifferentSizes
    | SwapTokenRangesOverlap
    | ReverseTokenRangeSingleToken


type alias TokenOperationEditDetails =
    { operation : TokenOperationOption
    , insertTokenRange : RangeEntry
    , removeTokenRange : RangeEntry
    , swapTokenRange1 : RangeEntry
    , swapTokenRange2 : RangeEntry
    , reverseTokenRange : RangeEntry
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
    TokenOperationEditDetails NoOptionSelected emptyRange emptyRange emptyRange emptyRange emptyRange NoValidationError


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
            if someRange.start == 0 || someRange.end == 0 then
                Just (ZeroInRange field)

            else if someRange.start > someRange.end then
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


reverseSingleTokenValidation : RangeEntry -> Maybe TokenOperationValidationError
reverseSingleTokenValidation reverseTokenEntry =
    case reverseTokenEntry.range of
        Just someRange ->
            if someRange.start == someRange.end then
                Just ReverseTokenRangeSingleToken

            else
                Nothing

        Nothing ->
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

                ReverseTokenRangeOption ->
                    [ commonTokenRangeValidation ReverseTokenRangeField editDetails.reverseTokenRange
                    , tokenRangeEndOffTokens lastToken ReverseTokenRangeField editDetails.reverseTokenRange
                    , reverseSingleTokenValidation editDetails.reverseTokenRange
                    ]
    in
    List.filterMap identity allErrors
        |> List.head
        |> Maybe.withDefault NoValidationError


rangeEntryFromString : String -> RangeEntry
rangeEntryFromString rangeString =
    { enteredText = rangeString, range = parseRange rangeString }


updateEditDetails : TokenOperationChangeType -> TokenOperationEditDetails -> TokenOperationEditDetails
updateEditDetails change editDetails =
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

                RangeEdited ReverseTokenRangeField newValue ->
                    { editDetails | reverseTokenRange = rangeEntryFromString newValue }
    in
    { updatedDetails | validationError = NoValidationError }


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

        ZeroInRange errorField ->
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

        ReverseTokenRangeSingleToken ->
            field == ReverseTokenRangeField


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


isReverseTokenRangeFieldInvalid : TokenOperationEditDetails -> Bool
isReverseTokenRangeFieldInvalid tokenOperationEditDetails =
    isTokenRangeFieldInvalid ReverseTokenRangeField tokenOperationEditDetails


insertTokensIntoBarcodeScannerData : TokenRange -> BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine
insertTokensIntoBarcodeScannerData range line =
    let
        offset : Int
        offset =
            range.end - range.start + 1
    in
    case line.contents of
        Ordinary athlete (Just token) ->
            if token < range.start then
                Just line

            else
                Just { line | contents = Ordinary athlete (Just (token + offset)) }

        Ordinary _ Nothing ->
            Just line

        MisScan _ ->
            Just line


removeTokensFromBarcodeScannerData : TokenRange -> BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine
removeTokensFromBarcodeScannerData range line =
    let
        offset : Int
        offset =
            range.end - range.start + 1
    in
    case line.contents of
        Ordinary athlete (Just token) ->
            if token < range.start then
                Just line

            else if token <= range.end then
                Nothing

            else
                Just { line | contents = Ordinary athlete (Just (token - offset)) }

        Ordinary _ Nothing ->
            Just line

        MisScan _ ->
            Just line


swapTokensInBarcodeScannerData : TokenRange -> Int -> BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine
swapTokensInBarcodeScannerData range1 range2Start line =
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
    in
    case line.contents of
        Ordinary athlete (Just token) ->
            if range1.start <= token && token <= range1.end then
                Just { line | contents = Ordinary athlete (Just (token + oneToTwoOffset)) }

            else if range2Start <= token && token <= range2End then
                Just { line | contents = Ordinary athlete (Just (token + twoToOneOffset)) }

            else
                Just line

        Ordinary _ Nothing ->
            Just line

        MisScan _ ->
            Just line


reverseTokenRangeInBarcodeScannerData : TokenRange -> BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine
reverseTokenRangeInBarcodeScannerData range line =
    case line.contents of
        Ordinary athlete (Just token) ->
            if range.start <= token && token <= range.end then
                Just { line | contents = Ordinary athlete (Just (range.start + range.end - token)) }

            else
                Just line

        Ordinary _ Nothing ->
            Just line

        MisScan _ ->
            Just line


applyOperation : (BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine) -> BarcodeScannerData -> BarcodeScannerData
applyOperation operation barcodeScannerData =
    let
        applyOperationToLine : BarcodeScannerFileLine -> BarcodeScannerFileLine
        applyOperationToLine line =
            case operation line of
                Just newLine ->
                    newLine

                Nothing ->
                    { line | deletionStatus = Deleted DeletedByUser }

        applyOperationToFile : BarcodeScannerFile -> BarcodeScannerFile
        applyOperationToFile file =
            { file | lines = List.map applyOperationToLine file.lines }
    in
    regenerate { barcodeScannerData | files = List.map applyOperationToFile barcodeScannerData.files }


tryApplyTokenOperationToBarcodeScannerData : TokenOperationEditDetails -> BarcodeScannerData -> Result TokenOperationValidationError BarcodeScannerData
tryApplyTokenOperationToBarcodeScannerData tokenOperationEditDetails barcodeScannerData =
    let
        validationError : TokenOperationValidationError
        validationError =
            validateEditDetails (allTokensUsed barcodeScannerData) tokenOperationEditDetails

        apply : (BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine) -> Result TokenOperationValidationError BarcodeScannerData
        apply operation =
            Ok (applyOperation operation barcodeScannerData)
    in
    if validationError == NoValidationError then
        case tokenOperationEditDetails.operation of
            NoOptionSelected ->
                Ok barcodeScannerData

            InsertTokensOption ->
                case tokenOperationEditDetails.insertTokenRange.range of
                    Just range ->
                        apply (insertTokensIntoBarcodeScannerData range)

                    Nothing ->
                        Ok barcodeScannerData

            RemoveTokensOption ->
                case tokenOperationEditDetails.removeTokenRange.range of
                    Just range ->
                        apply (removeTokensFromBarcodeScannerData range)

                    Nothing ->
                        Ok barcodeScannerData

            SwapTokenRangeOption ->
                case ( tokenOperationEditDetails.swapTokenRange1.range, tokenOperationEditDetails.swapTokenRange2.range ) of
                    ( Just range1, Just range2 ) ->
                        if range2.end - range2.start == range1.end - range1.start then
                            apply (swapTokensInBarcodeScannerData range1 range2.start)

                        else
                            -- Validation on range sizes being equal should have been done by
                            -- the time we get here, but if not, let's just do nothing.
                            Ok barcodeScannerData

                    _ ->
                        Ok barcodeScannerData

            ReverseTokenRangeOption ->
                case tokenOperationEditDetails.reverseTokenRange.range of
                    Just range ->
                        apply (reverseTokenRangeInBarcodeScannerData range)

                    Nothing ->
                        Ok barcodeScannerData

    else
        Err validationError
