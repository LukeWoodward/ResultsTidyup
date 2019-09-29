module TokenOperations exposing
    ( RangeEntry
    , TokenOperationEditState
    , TokenOperationOptions(..)
    , TokenOperationValidationError(..)
    , TokenRange
    , TokenRangeField(..)
    , emptyEditState
    , parseRange
    , validateEditState
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


type TokenOperationOptions
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
    | RangeOffEndOfTokens TokenRangeField
    | SwapTokenRangesOverlap


type alias TokenOperationEditState =
    { operation : TokenOperationOptions
    , insertTokenRange : RangeEntry
    , removeTokenRange : RangeEntry
    , swapTokenRange1 : RangeEntry
    , swapTokenRange2 : RangeEntry
    , validationError : Maybe TokenOperationValidationError
    }


emptyRange : RangeEntry
emptyRange =
    RangeEntry "" Nothing


emptyEditState : TokenOperationEditState
emptyEditState =
    TokenOperationEditState NoOptionSelected emptyRange emptyRange emptyRange emptyRange Nothing


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


commonTokenValidation : TokenRangeField -> Int -> RangeEntry -> Maybe TokenOperationValidationError
commonTokenValidation field lastToken entry =
    case entry.range of
        Just someRange ->
            if someRange.start > someRange.end then
                Just (EmptyRange field)

            else if someRange.end > lastToken then
                Just (RangeOffEndOfTokens field)

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


validateEditState : Int -> TokenOperationEditState -> Maybe TokenOperationValidationError
validateEditState lastToken editState =
    case editState.operation of
        NoOptionSelected ->
            Just TokenOperationNotSelected

        InsertTokensOption ->
            commonTokenValidation InsertTokenRangeField lastToken editState.insertTokenRange

        RemoveTokensOption ->
            commonTokenValidation RemoveTokenRangeField lastToken editState.removeTokenRange

        SwapTokenRangeOption ->
            [ commonTokenValidation SwapTokenRangeField1 lastToken editState.swapTokenRange1
            , commonTokenValidation SwapTokenRangeField2 lastToken editState.swapTokenRange2
            , rangeTokenOverlapValidation editState.swapTokenRange1 editState.swapTokenRange2
            ]
                |> List.filterMap identity
                |> List.head
