module TokenOperations exposing
    ( RangeEntry
    , TokenOperationEditDetails
    , TokenOperationOption(..)
    , TokenOperationValidationError(..)
    , TokenRange
    , TokenRangeField(..)
    , emptyEditDetails
    , parseRange
    , rangeToString
    , validateEditDetails
    )


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
    | RangeOffEndOfTokens Int TokenRange TokenRangeField
    | SwapTokenRangesOverlap


type alias TokenOperationEditDetails =
    { operation : TokenOperationOption
    , insertTokenRange : RangeEntry
    , removeTokenRange : RangeEntry
    , swapTokenRange1 : RangeEntry
    , swapTokenRange2 : RangeEntry
    , validationError : Maybe TokenOperationValidationError
    }


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


commonTokenValidation : TokenRangeField -> Int -> (TokenRange -> Int) -> RangeEntry -> Maybe TokenOperationValidationError
commonTokenValidation field lastToken rangeFieldGetter entry =
    case entry.range of
        Just someRange ->
            if someRange.start > someRange.end then
                Just (EmptyRange field)

            else if rangeFieldGetter someRange > lastToken then
                Just (RangeOffEndOfTokens lastToken someRange field)

            else
                Nothing

        Nothing ->
            Just (InvalidRange field)


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


validateEditDetails : Int -> TokenOperationEditDetails -> Maybe TokenOperationValidationError
validateEditDetails lastToken editDetails =
    case editDetails.operation of
        NoOptionSelected ->
            Just TokenOperationNotSelected

        InsertTokensOption ->
            commonTokenValidation InsertTokenRangeField lastToken .start editDetails.insertTokenRange

        RemoveTokensOption ->
            commonTokenValidation RemoveTokenRangeField lastToken .end editDetails.removeTokenRange

        SwapTokenRangeOption ->
            [ commonTokenValidation SwapTokenRangeField1 lastToken .end editDetails.swapTokenRange1
            , commonTokenValidation SwapTokenRangeField2 lastToken .end editDetails.swapTokenRange2
            , rangeTokenOverlapValidation editDetails.swapTokenRange1 editDetails.swapTokenRange2
            ]
                |> List.filterMap identity
                |> List.head
