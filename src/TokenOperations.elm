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
    , isSwapTokenRange1FieldInvalid
    , isSwapTokenRange2FieldInvalid
    , parseRange
    , rangeToString
    , updateEditDetails
    , validateEditDetails
    )

import Set exposing (Set)


type alias TokenRange =
    { start : Int
    , end : Int
    }


type alias RangeEntry =
    { enteredText : String
    , range : Maybe TokenRange
    }


type TokenOperation
    = InsertTokens TokenRange
    | RemoveTokens TokenRange
    | SwapTokenRange TokenRange TokenRange


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
    = TokenOperationNotSelected
    | InvalidRange TokenRangeField
    | EmptyRange TokenRangeField
    | InsertRangeOffEndOfTokens Int TokenRange
    | RangeOffEndOfTokens Int TokenRange TokenRangeField
    | RemovingExistingTokens (List Int) TokenRange
    | SwapTokenRangesOverlap


type alias TokenOperationEditDetails =
    { operation : TokenOperationOption
    , insertTokenRange : RangeEntry
    , removeTokenRange : RangeEntry
    , swapTokenRange1 : RangeEntry
    , swapTokenRange2 : RangeEntry
    , validationError : Maybe TokenOperationValidationError
    }


type TokenOperationChangeType
    = ChangeOperation TokenOperationOption
    | RangeEdited TokenRangeField String


emptyRange : RangeEntry
emptyRange =
    RangeEntry "" Nothing


emptyEditDetails : TokenOperationEditDetails
emptyEditDetails =
    TokenOperationEditDetails NoOptionSelected emptyRange emptyRange emptyRange emptyRange Nothing


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


validateEditDetails : Set Int -> TokenOperationEditDetails -> Maybe TokenOperationValidationError
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
                    , rangeTokenOverlapValidation editDetails.swapTokenRange1 editDetails.swapTokenRange2
                    ]
    in
    List.filterMap identity allErrors
        |> List.head


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
        Just TokenOperationNotSelected ->
            False

        Just (InvalidRange errorField) ->
            errorField == field

        Just (EmptyRange errorField) ->
            errorField == field

        Just (InsertRangeOffEndOfTokens _ _) ->
            field == InsertTokenRangeField

        Just (RangeOffEndOfTokens _ _ errorField) ->
            errorField == field

        Just (RemovingExistingTokens _ _) ->
            field == RemoveTokenRangeField

        Just SwapTokenRangesOverlap ->
            field == SwapTokenRangeField1 || field == SwapTokenRangeField2

        Nothing ->
            False


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
