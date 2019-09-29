module TokenOperationsEditing exposing
    ( TokenOperationChangeType(..)
    , isInsertTokenRangeFieldInvalid
    , isRemoveTokenRangeFieldInvalid
    , isSwapTokenRange1FieldInvalid
    , isSwapTokenRange2FieldInvalid
    , updateEditDetails
    )

import TokenOperations
    exposing
        ( RangeEntry
        , TokenOperationEditDetails
        , TokenOperationOption(..)
        , TokenOperationValidationError(..)
        , TokenRangeField(..)
        , parseRange
        , validateEditDetails
        )


type TokenOperationChangeType
    = ChangeOperation TokenOperationOption
    | RangeEdited TokenRangeField String


rangeEntryFromString : String -> RangeEntry
rangeEntryFromString rangeString =
    { enteredText = rangeString, range = parseRange rangeString }


updateEditDetails : Int -> TokenOperationChangeType -> TokenOperationEditDetails -> TokenOperationEditDetails
updateEditDetails lastToken change editDetails =
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
    { updatedDetails | validationError = validateEditDetails lastToken updatedDetails }


isTokenRangeFieldInvalid : TokenRangeField -> TokenOperationEditDetails -> Bool
isTokenRangeFieldInvalid field tokenOperationEditDetails =
    case tokenOperationEditDetails.validationError of
        Just TokenOperationNotSelected ->
            False

        Just (InvalidRange errorField) ->
            errorField == field

        Just (EmptyRange errorField) ->
            errorField == field

        Just (RangeOffEndOfTokens _ _ errorField) ->
            errorField == field

        Just SwapTokenRangesOverlap ->
            False

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
    isTokenRangeFieldInvalid SwapTokenRangeField1 tokenOperationEditDetails || tokenOperationEditDetails.validationError == Just SwapTokenRangesOverlap


isSwapTokenRange2FieldInvalid : TokenOperationEditDetails -> Bool
isSwapTokenRange2FieldInvalid tokenOperationEditDetails =
    isTokenRangeFieldInvalid SwapTokenRangeField2 tokenOperationEditDetails || tokenOperationEditDetails.validationError == Just SwapTokenRangesOverlap
