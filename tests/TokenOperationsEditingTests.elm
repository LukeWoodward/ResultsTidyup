module TokenOperationsEditingTests exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import TokenOperations exposing (RangeEntry, TokenOperationEditDetails, TokenOperationOption(..), TokenOperationValidationError(..), TokenRange, TokenRangeField(..), emptyEditDetails)
import TokenOperationsEditing
    exposing
        ( TokenOperationChangeType(..)
        , isInsertTokenRangeFieldInvalid
        , isRemoveTokenRangeFieldInvalid
        , isSwapTokenRange1FieldInvalid
        , isSwapTokenRange2FieldInvalid
        , updateEditDetails
        )
import TokenOperationsTests exposing (makeEntry)


suite : Test
suite =
    describe "TokenOperationsEditing tests"
        [ describe "updateEditDetails tests"
            [ test "ChangeOperation changes the operation and runs validation" <|
                \() ->
                    updateEditDetails 50 (ChangeOperation InsertTokensOption) emptyEditDetails
                        |> Expect.equal { emptyEditDetails | operation = InsertTokensOption, validationError = Just (InvalidRange InsertTokenRangeField) }
            , test "RangeEdited updates the insert-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = RangeEntry "" Nothing, validationError = Just (InvalidRange InsertTokenRangeField) }
                    in
                    updateEditDetails 50 (RangeEdited InsertTokenRangeField "40-50") initialDetails
                        |> Expect.equal { initialDetails | insertTokenRange = makeEntry 40 50, validationError = Nothing }
            , test "RangeEdited updates the insert-tokens range and runs validation with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = RangeEntry "" Nothing, validationError = Just (InvalidRange InsertTokenRangeField) }
                    in
                    updateEditDetails 50 (RangeEdited InsertTokenRangeField "50-40") initialDetails
                        |> Expect.equal { initialDetails | insertTokenRange = makeEntry 50 40, validationError = Just (EmptyRange InsertTokenRangeField) }
            , test "RangeEdited updates the remove-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = RangeEntry "" Nothing, validationError = Just (InvalidRange RemoveTokenRangeField) }
                    in
                    updateEditDetails 50 (RangeEdited RemoveTokenRangeField "40-50") initialDetails
                        |> Expect.equal { initialDetails | removeTokenRange = makeEntry 40 50, validationError = Nothing }
            , test "RangeEdited updates the remove-tokens range and runs validation with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = RangeEntry "" Nothing, validationError = Just (InvalidRange RemoveTokenRangeField) }
                    in
                    updateEditDetails 50 (RangeEdited RemoveTokenRangeField "50-40") initialDetails
                        |> Expect.equal { initialDetails | removeTokenRange = makeEntry 50 40, validationError = Just (EmptyRange RemoveTokenRangeField) }
            , test "RangeEdited updates the first swap-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = RangeEntry "" Nothing, swapTokenRange2 = makeEntry 20 30, validationError = Just (InvalidRange SwapTokenRangeField1) }
                    in
                    updateEditDetails 50 (RangeEdited SwapTokenRangeField1 "40-50") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange1 = makeEntry 40 50, validationError = Nothing }
            , test "RangeEdited updates the first swap-tokens range and runs validation with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = RangeEntry "" Nothing, swapTokenRange2 = makeEntry 20 30, validationError = Just (InvalidRange SwapTokenRangeField1) }
                    in
                    updateEditDetails 50 (RangeEdited SwapTokenRangeField1 "50-40") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange1 = makeEntry 50 40, validationError = Just (EmptyRange SwapTokenRangeField1) }
            , test "RangeEdited updates the second swap-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 20 30, swapTokenRange2 = RangeEntry "" Nothing, validationError = Just (InvalidRange SwapTokenRangeField2) }
                    in
                    updateEditDetails 50 (RangeEdited SwapTokenRangeField2 "40-50") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange2 = makeEntry 40 50, validationError = Nothing }
            , test "RangeEdited updates the second swap-tokens range and runs validation with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 20 30, swapTokenRange2 = RangeEntry "" Nothing, validationError = Just (InvalidRange SwapTokenRangeField2) }
                    in
                    updateEditDetails 50 (RangeEdited SwapTokenRangeField2 "50-40") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange2 = makeEntry 50 40, validationError = Just (EmptyRange SwapTokenRangeField2) }
            ]
        , describe "isInsertTokenRangeFieldInvalid tests"
            [ test "insert token range field not invalid if no validation error" <|
                \() ->
                    isInsertTokenRangeFieldInvalid emptyEditDetails
                        |> Expect.equal False
            , test "insert token range field not invalid if validation error is no option selected" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = Just TokenOperationNotSelected }
                        |> Expect.equal False
            , test "insert token range field invalid if validation error is insert range invalid" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (InvalidRange InsertTokenRangeField) }
                        |> Expect.equal True
            , test "insert token range field not invalid if validation error is another range invalid" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (InvalidRange RemoveTokenRangeField) }
                        |> Expect.equal False
            , test "insert token range field invalid if validation error is insert range empty" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (EmptyRange InsertTokenRangeField) }
                        |> Expect.equal True
            , test "insert token range field not invalid if validation error is another range empty" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (EmptyRange SwapTokenRangeField1) }
                        |> Expect.equal False
            , test "insert token range field invalid if validation error is insert range off end of tokens" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (RangeOffEndOfTokens 50 (TokenRange 60 70) InsertTokenRangeField) }
                        |> Expect.equal True
            , test "insert token range field not invalid if validation error is another range off end of tokens" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (RangeOffEndOfTokens 50 (TokenRange 60 70) SwapTokenRangeField2) }
                        |> Expect.equal False
            , test "insert token range field not invalid if validation error is token ranges overlap" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = Just SwapTokenRangesOverlap }
                        |> Expect.equal False
            ]
        , describe "isRemoveTokenRangeFieldInvalid tests"
            [ test "remove token range field not invalid if no validation error" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid emptyEditDetails
                        |> Expect.equal False
            , test "remove token range field not invalid if validation error is no option selected" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = Just TokenOperationNotSelected }
                        |> Expect.equal False
            , test "remove token range field invalid if validation error is remove range invalid" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (InvalidRange RemoveTokenRangeField) }
                        |> Expect.equal True
            , test "remove token range field not invalid if validation error is another range invalid" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (InvalidRange SwapTokenRangeField1) }
                        |> Expect.equal False
            , test "remove token range field invalid if validation error is remove range empty" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (EmptyRange RemoveTokenRangeField) }
                        |> Expect.equal True
            , test "remove token range field not invalid if validation error is another range empty" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (EmptyRange SwapTokenRangeField2) }
                        |> Expect.equal False
            , test "remove token range field invalid if validation error is remove range off end of tokens" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (RangeOffEndOfTokens 50 (TokenRange 60 70) RemoveTokenRangeField) }
                        |> Expect.equal True
            , test "remove token range field not invalid if validation error is another range off end of tokens" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = Just (RangeOffEndOfTokens 50 (TokenRange 60 70) InsertTokenRangeField) }
                        |> Expect.equal False
            , test "remove token range field not invalid if validation error is token ranges overlap" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = Just SwapTokenRangesOverlap }
                        |> Expect.equal False
            ]
        , describe "isSwapTokenRangeField1Invalid tests"
            [ test "first swap token range field not invalid if no validation error" <|
                \() ->
                    isSwapTokenRange1FieldInvalid emptyEditDetails
                        |> Expect.equal False
            , test "first swap token range field not invalid if validation error is no option selected" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = Just TokenOperationNotSelected }
                        |> Expect.equal False
            , test "first swap token range field invalid if validation error is first swap token range invalid" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = Just (InvalidRange SwapTokenRangeField1) }
                        |> Expect.equal True
            , test "first swap token range field not invalid if validation error is another range invalid" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = Just (InvalidRange SwapTokenRangeField2) }
                        |> Expect.equal False
            , test "first swap token range field invalid if validation error is first swap token range empty" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = Just (EmptyRange SwapTokenRangeField1) }
                        |> Expect.equal True
            , test "first swap token range field not invalid if validation error is another range empty" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = Just (EmptyRange InsertTokenRangeField) }
                        |> Expect.equal False
            , test "first swap token range field invalid if validation error is first swap token range off end of tokens" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = Just (RangeOffEndOfTokens 50 (TokenRange 60 70) SwapTokenRangeField1) }
                        |> Expect.equal True
            , test "first swap token range field not invalid if validation error is another range off end of tokens" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = Just (RangeOffEndOfTokens 50 (TokenRange 60 70) RemoveTokenRangeField) }
                        |> Expect.equal False
            , test "first swap token range field invalid if validation error is token ranges overlap" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = Just SwapTokenRangesOverlap }
                        |> Expect.equal True
            ]
        , describe "isSwapTokenRangeField2Invalid tests"
            [ test "second swap token range field not invalid if no validation error" <|
                \() ->
                    isSwapTokenRange2FieldInvalid emptyEditDetails
                        |> Expect.equal False
            , test "second swap token range field not invalid if validation error is no option selected" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = Just TokenOperationNotSelected }
                        |> Expect.equal False
            , test "second swap token range field invalid if validation error is second swap token range invalid" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = Just (InvalidRange SwapTokenRangeField2) }
                        |> Expect.equal True
            , test "second swap token range field not invalid if validation error is another range invalid" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = Just (InvalidRange InsertTokenRangeField) }
                        |> Expect.equal False
            , test "second swap token range field invalid if validation error is second swap token range empty" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = Just (EmptyRange SwapTokenRangeField2) }
                        |> Expect.equal True
            , test "second swap token range field not invalid if validation error is another range empty" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = Just (EmptyRange RemoveTokenRangeField) }
                        |> Expect.equal False
            , test "second swap token range field invalid if validation error is second swap token range off end of tokens" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = Just (RangeOffEndOfTokens 50 (TokenRange 60 70) SwapTokenRangeField2) }
                        |> Expect.equal True
            , test "second swap token range field not invalid if validation error is another range off end of tokens" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = Just (RangeOffEndOfTokens 50 (TokenRange 60 70) SwapTokenRangeField1) }
                        |> Expect.equal False
            , test "second swap token range field invalid if validation error is token ranges overlap" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = Just SwapTokenRangesOverlap }
                        |> Expect.equal True
            ]
        ]
