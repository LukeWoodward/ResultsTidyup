module TokenOperations exposing
    ( TokenOperationChangeType(..)
    , TokenOperationEditDetails
    , TokenOperationOption(..)
    , TokenOperationValidationError(..)
    , TokenRangeField(..)
    , emptyEditDetails
    , isInsertTokenRangeFieldInvalid
    , isRemoveTokenRangeFieldInvalid
    , isReverseTokenRangeFieldInvalid
    , isSwapTokenRange1FieldInvalid
    , isSwapTokenRange2FieldInvalid
    , tryApplyTokenOperationToBarcodeScannerData
    , updateEditDetails
    , validateEditDetails
    )

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents
        , allTokensUsed
        , regenerate
        )
import DataEntry exposing (Range, RangeEntry, emptyEntry, rangeEntryFromString)
import Set exposing (Set)


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
    | TokenOffEndOfTokens Int Int TokenRangeField
    | InsertRangeOffEndOfTokens Int Range
    | RangeOffEndOfTokens Int Range TokenRangeField
    | RemovingExistingToken Int
    | RemovingExistingTokens (List Int) Range
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


emptyEditDetails : TokenOperationEditDetails
emptyEditDetails =
    TokenOperationEditDetails NoOptionSelected emptyEntry emptyEntry emptyEntry emptyEntry emptyEntry NoValidationError


commonTokenRangeValidation : TokenRangeField -> RangeEntry -> Maybe TokenOperationValidationError
commonTokenRangeValidation field entry =
    case entry.parsedValue of
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
    entry.parsedValue
        |> Maybe.andThen
            (\someRange ->
                if lastToken < someRange.start && someRange.start <= someRange.end then
                    if someRange.start < someRange.end then
                        Just (InsertRangeOffEndOfTokens lastToken someRange)

                    else
                        Just (TokenOffEndOfTokens lastToken someRange.start InsertTokenRangeField)

                else
                    Nothing
            )


tokenRangeEndOffTokens : Int -> TokenRangeField -> RangeEntry -> Maybe TokenOperationValidationError
tokenRangeEndOffTokens lastToken field entry =
    entry.parsedValue
        |> Maybe.andThen
            (\someRange ->
                if someRange.start <= someRange.end && someRange.end > lastToken then
                    if someRange.start < someRange.end then
                        Just (RangeOffEndOfTokens lastToken someRange field)

                    else
                        Just (TokenOffEndOfTokens lastToken someRange.start field)

                else
                    Nothing
            )


removingExistingTokens : Set Int -> RangeEntry -> Maybe TokenOperationValidationError
removingExistingTokens allTokens entry =
    entry.parsedValue
        |> Maybe.andThen
            (\someRange ->
                let
                    removedExistingTokens : List Int
                    removedExistingTokens =
                        List.range someRange.start someRange.end
                            |> Set.fromList
                            |> Set.intersect allTokens
                            |> Set.toList
                            |> List.sort
                in
                case removedExistingTokens of
                    [] ->
                        Nothing

                    [ singleToken ] ->
                        Just (RemovingExistingToken singleToken)

                    _ ->
                        Just (RemovingExistingTokens removedExistingTokens someRange)
            )


rangesOfDifferentSizeValidation : RangeEntry -> RangeEntry -> Maybe TokenOperationValidationError
rangesOfDifferentSizeValidation swapTokenEntry1 swapTokenEntry2 =
    case ( swapTokenEntry1.parsedValue, swapTokenEntry2.parsedValue ) of
        ( Just range1, Just range2 ) ->
            if range1.end - range1.start == range2.end - range2.start then
                Nothing

            else
                Just SwapTokenRangesOfDifferentSizes

        _ ->
            Nothing


rangeTokenOverlapValidation : RangeEntry -> RangeEntry -> Maybe TokenOperationValidationError
rangeTokenOverlapValidation swapTokenEntry1 swapTokenEntry2 =
    case ( swapTokenEntry1.parsedValue, swapTokenEntry2.parsedValue ) of
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
    reverseTokenEntry.parsedValue
        |> Maybe.andThen
            (\someRange ->
                if someRange.start == someRange.end then
                    Just ReverseTokenRangeSingleToken

                else
                    Nothing
            )


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

        TokenOffEndOfTokens _ _ errorField ->
            errorField == field

        InsertRangeOffEndOfTokens _ _ ->
            field == InsertTokenRangeField

        RangeOffEndOfTokens _ _ errorField ->
            errorField == field

        RemovingExistingToken _ ->
            field == RemoveTokenRangeField

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


insertTokensIntoBarcodeScannerData : Range -> BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine
insertTokensIntoBarcodeScannerData range line =
    let
        offset : Int
        offset =
            range.end - range.start + 1
    in
    case line.contents.token of
        Just token ->
            if token < range.start then
                Just line

            else
                Just { line | contents = LineContents line.contents.athlete (Just (token + offset)) }

        Nothing ->
            Just line


removeTokensFromBarcodeScannerData : Range -> BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine
removeTokensFromBarcodeScannerData range line =
    let
        offset : Int
        offset =
            range.end - range.start + 1
    in
    case line.contents.token of
        Just token ->
            if token < range.start then
                Just line

            else if token <= range.end then
                Nothing

            else
                Just { line | contents = LineContents line.contents.athlete (Just (token - offset)) }

        Nothing ->
            Just line


swapTokensInBarcodeScannerData : Range -> Int -> BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine
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
    case line.contents.token of
        Just token ->
            if range1.start <= token && token <= range1.end then
                Just { line | contents = LineContents line.contents.athlete (Just (token + oneToTwoOffset)) }

            else if range2Start <= token && token <= range2End then
                Just { line | contents = LineContents line.contents.athlete (Just (token + twoToOneOffset)) }

            else
                Just line

        Nothing ->
            Just line


reverseTokenRangeInBarcodeScannerData : Range -> BarcodeScannerFileLine -> Maybe BarcodeScannerFileLine
reverseTokenRangeInBarcodeScannerData range line =
    case line.contents.token of
        Just token ->
            if range.start <= token && token <= range.end then
                Just { line | contents = LineContents line.contents.athlete (Just (range.start + range.end - token)) }

            else
                Just line

        Nothing ->
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
                case tokenOperationEditDetails.insertTokenRange.parsedValue of
                    Just range ->
                        apply (insertTokensIntoBarcodeScannerData range)

                    Nothing ->
                        Ok barcodeScannerData

            RemoveTokensOption ->
                case tokenOperationEditDetails.removeTokenRange.parsedValue of
                    Just range ->
                        apply (removeTokensFromBarcodeScannerData range)

                    Nothing ->
                        Ok barcodeScannerData

            SwapTokenRangeOption ->
                case ( tokenOperationEditDetails.swapTokenRange1.parsedValue, tokenOperationEditDetails.swapTokenRange2.parsedValue ) of
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
                case tokenOperationEditDetails.reverseTokenRange.parsedValue of
                    Just range ->
                        apply (reverseTokenRangeInBarcodeScannerData range)

                    Nothing ->
                        Ok barcodeScannerData

    else
        Err validationError
