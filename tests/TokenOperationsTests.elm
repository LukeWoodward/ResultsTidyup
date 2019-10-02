module TokenOperationsTests exposing (makeEntry, suite)

import Expect
import Set exposing (Set)
import Test exposing (Test, describe, test)
import TokenOperations
    exposing
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


makeEntry : Int -> Int -> RangeEntry
makeEntry start end =
    RangeEntry
        (String.fromInt start ++ "-" ++ String.fromInt end)
        (Just (TokenRange start end))


invalidEntry : RangeEntry
invalidEntry =
    RangeEntry "invalid" Nothing


tokenSet : Set Int
tokenSet =
    Set.fromList [ 1, 2, 8, 14, 17, 23, 26, 27, 28, 41, 48, 50 ]


suite : Test
suite =
    describe "TokenOperations tests"
        [ describe "parseRange tests"
            [ test "parseRange of an empty string is not valid" <|
                \() ->
                    parseRange ""
                        |> Expect.equal Nothing
            , test "parseRange of a string containing an invalid number is not valid" <|
                \() ->
                    parseRange "this is not valid"
                        |> Expect.equal Nothing
            , test "parseRange of a string containing a single valid number is valid" <|
                \() ->
                    parseRange "37"
                        |> Expect.equal (Just (TokenRange 37 37))
            , test "parseRange of a string containing a single valid number with whitespace is valid" <|
                \() ->
                    parseRange "    37      "
                        |> Expect.equal (Just (TokenRange 37 37))
            , test "parseRange of a string containing two valid numbers is valid" <|
                \() ->
                    parseRange "22-34"
                        |> Expect.equal (Just (TokenRange 22 34))
            , test "parseRange of a string containing two valid numbers with whitespace is valid" <|
                \() ->
                    parseRange "     22  -   34   "
                        |> Expect.equal (Just (TokenRange 22 34))
            , test "parseRange of a string with a missing end number is invalid" <|
                \() ->
                    parseRange "22-"
                        |> Expect.equal Nothing
            , test "parseRange of a string with a missing start number is invalid" <|
                \() ->
                    parseRange "-34"
                        |> Expect.equal Nothing
            , test "parseRange of a string containing three valid numbers is invalid" <|
                \() ->
                    parseRange "22-34-56"
                        |> Expect.equal Nothing
            , test "parseRange of a string containing the same valid number twice is valid" <|
                \() ->
                    parseRange "34-34"
                        |> Expect.equal (Just (TokenRange 34 34))
            , test "parseRange of a string containing two valid numbers the wrong way around is valid" <|
                \() ->
                    parseRange "34-22"
                        |> Expect.equal (Just (TokenRange 34 22))
            ]
        , describe "rangeToString tests"
            [ test "formatting a single-value range returns a single number" <|
                \() ->
                    rangeToString (TokenRange 59 59)
                        |> Expect.equal "59"
            , test "formatting a multi-value range returns two hyphen-separated numbers" <|
                \() ->
                    rangeToString (TokenRange 47 52)
                        |> Expect.equal "47-52"
            ]
        , describe "validateEditDetails tests"
            [ test "validating the default details returns the nothing-selected options" <|
                \() ->
                    validateEditDetails tokenSet emptyEditDetails
                        |> Expect.equal TokenOperationNotSelected
            , test "validating a valid insert operation returns no error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 10 20 }
                        |> Expect.equal NoValidationError
            , test "validating an insert operation with an invalid range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = invalidEntry }
                        |> Expect.equal (InvalidRange InsertTokenRangeField)
            , test "validating an insert operation with an empty range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 20 10 }
                        |> Expect.equal (EmptyRange InsertTokenRangeField)
            , test "validating an insert operation with the start of the range off the end of the tokens returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 55 65 }
                        |> Expect.equal (InsertRangeOffEndOfTokens 50 (TokenRange 55 65))
            , test "validating an insert operation with only the end of the range off the end of the tokens is valid" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 45 55 }
                        |> Expect.equal NoValidationError
            , test "validating a valid remove operation returns no error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 30 40 }
                        |> Expect.equal NoValidationError
            , test "validating a remove operation with an invalid range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = invalidEntry }
                        |> Expect.equal (InvalidRange RemoveTokenRangeField)
            , test "validating a remove operation with an empty range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 20 10 }
                        |> Expect.equal (EmptyRange RemoveTokenRangeField)
            , test "validating a remove operation that attempts to remove unused tokens returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 40 50 }
                        |> Expect.equal (RemovingExistingTokens [ 41, 48, 50 ] (TokenRange 40 50))
            , test "validating a remove operation with a range off the end returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 45 55 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (TokenRange 45 55) RemoveTokenRangeField)
            , test "validating a valid swap operation returns no error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal NoValidationError
            , test "validating a swap operation with an invalid first range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = invalidEntry, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (InvalidRange SwapTokenRangeField1)
            , test "validating a swap operation with an empty first range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 20 10, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (EmptyRange SwapTokenRangeField1)
            , test "validating a swap operation with a first range off the end returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 45 55, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (TokenRange 45 55) SwapTokenRangeField1)
            , test "validating a swap operation with an invalid second range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = invalidEntry }
                        |> Expect.equal (InvalidRange SwapTokenRangeField2)
            , test "validating a swap operation with an empty second range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 40 30 }
                        |> Expect.equal (EmptyRange SwapTokenRangeField2)
            , test "validating a swap operation with a second range off the end returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 45 55 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (TokenRange 45 55) SwapTokenRangeField2)
            , test "validating a swap operation with overlapping ranges returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 15 25 }
                        |> Expect.equal SwapTokenRangesOverlap
            , test "validating a swap operation with ranges overlapping by a single value returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 20 30 }
                        |> Expect.equal SwapTokenRangesOverlap
            , test "validating a swap operation with both ranges invalid returns an error with the first range" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = invalidEntry, swapTokenRange2 = invalidEntry }
                        |> Expect.equal (InvalidRange SwapTokenRangeField1)
            , test "validating a swap operation with the first range off the end and overlapping the second returns the off-the-end error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 35 55, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (TokenRange 35 55) SwapTokenRangeField1)
            , test "validating a swap operation with the second range off the end and overlapping the first returns the off-the-end error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 30 40, swapTokenRange2 = makeEntry 35 55 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (TokenRange 35 55) SwapTokenRangeField2)
            ]
        , describe "updateEditDetails tests"
            [ test "ChangeOperation changes the operation and runs validation" <|
                \() ->
                    updateEditDetails tokenSet (ChangeOperation InsertTokensOption) emptyEditDetails
                        |> Expect.equal { emptyEditDetails | operation = InsertTokensOption, validationError = InvalidRange InsertTokenRangeField }
            , test "RangeEdited updates the insert-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = RangeEntry "" Nothing, validationError = InvalidRange InsertTokenRangeField }
                    in
                    updateEditDetails tokenSet (RangeEdited InsertTokenRangeField "30-40") initialDetails
                        |> Expect.equal { initialDetails | insertTokenRange = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the insert-tokens range and runs validation with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = RangeEntry "" Nothing, validationError = InvalidRange InsertTokenRangeField }
                    in
                    updateEditDetails tokenSet (RangeEdited InsertTokenRangeField "40-30") initialDetails
                        |> Expect.equal { initialDetails | insertTokenRange = makeEntry 40 30, validationError = EmptyRange InsertTokenRangeField }
            , test "RangeEdited updates the remove-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = RangeEntry "" Nothing, validationError = InvalidRange RemoveTokenRangeField }
                    in
                    updateEditDetails tokenSet (RangeEdited RemoveTokenRangeField "30-40") initialDetails
                        |> Expect.equal { initialDetails | removeTokenRange = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the remove-tokens range and runs validation with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = RangeEntry "" Nothing, validationError = InvalidRange RemoveTokenRangeField }
                    in
                    updateEditDetails tokenSet (RangeEdited RemoveTokenRangeField "40-30") initialDetails
                        |> Expect.equal { initialDetails | removeTokenRange = makeEntry 40 30, validationError = EmptyRange RemoveTokenRangeField }
            , test "RangeEdited updates the first swap-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = RangeEntry "" Nothing, swapTokenRange2 = makeEntry 15 25, validationError = InvalidRange SwapTokenRangeField1 }
                    in
                    updateEditDetails tokenSet (RangeEdited SwapTokenRangeField1 "30-40") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange1 = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the first swap-tokens range and runs validation with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = RangeEntry "" Nothing, swapTokenRange2 = makeEntry 15 25, validationError = InvalidRange SwapTokenRangeField1 }
                    in
                    updateEditDetails tokenSet (RangeEdited SwapTokenRangeField1 "40-30") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange1 = makeEntry 40 30, validationError = EmptyRange SwapTokenRangeField1 }
            , test "RangeEdited updates the second swap-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 15 25, swapTokenRange2 = RangeEntry "" Nothing, validationError = InvalidRange SwapTokenRangeField2 }
                    in
                    updateEditDetails tokenSet (RangeEdited SwapTokenRangeField2 "30-40") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange2 = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the second swap-tokens range and runs validation with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 15 25, swapTokenRange2 = RangeEntry "" Nothing, validationError = InvalidRange SwapTokenRangeField2 }
                    in
                    updateEditDetails tokenSet (RangeEdited SwapTokenRangeField2 "40-30") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange2 = makeEntry 40 30, validationError = EmptyRange SwapTokenRangeField2 }
            ]
        , describe "isInsertTokenRangeFieldInvalid tests"
            [ test "insert token range field not invalid if no validation error" <|
                \() ->
                    isInsertTokenRangeFieldInvalid emptyEditDetails
                        |> Expect.equal False
            , test "insert token range field not invalid if validation error is no option selected" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = TokenOperationNotSelected }
                        |> Expect.equal False
            , test "insert token range field invalid if validation error is insert range invalid" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = InvalidRange InsertTokenRangeField }
                        |> Expect.equal True
            , test "insert token range field not invalid if validation error is another range invalid" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = InvalidRange RemoveTokenRangeField }
                        |> Expect.equal False
            , test "insert token range field invalid if validation error is insert range empty" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = EmptyRange InsertTokenRangeField }
                        |> Expect.equal True
            , test "insert token range field not invalid if validation error is another range empty" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = EmptyRange SwapTokenRangeField1 }
                        |> Expect.equal False
            , test "insert token range field invalid if validation error is insert range off end of tokens" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = RangeOffEndOfTokens 50 (TokenRange 60 70) InsertTokenRangeField }
                        |> Expect.equal True
            , test "insert token range field not invalid if validation error is another range off end of tokens" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = RangeOffEndOfTokens 50 (TokenRange 60 70) SwapTokenRangeField2 }
                        |> Expect.equal False
            , test "insert token range field not invalid if validation error is token ranges overlap" <|
                \() ->
                    isInsertTokenRangeFieldInvalid { emptyEditDetails | validationError = SwapTokenRangesOverlap }
                        |> Expect.equal False
            ]
        , describe "isRemoveTokenRangeFieldInvalid tests"
            [ test "remove token range field not invalid if no validation error" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid emptyEditDetails
                        |> Expect.equal False
            , test "remove token range field not invalid if validation error is no option selected" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = TokenOperationNotSelected }
                        |> Expect.equal False
            , test "remove token range field invalid if validation error is remove range invalid" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = InvalidRange RemoveTokenRangeField }
                        |> Expect.equal True
            , test "remove token range field not invalid if validation error is another range invalid" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = InvalidRange SwapTokenRangeField1 }
                        |> Expect.equal False
            , test "remove token range field invalid if validation error is remove range empty" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = EmptyRange RemoveTokenRangeField }
                        |> Expect.equal True
            , test "remove token range field not invalid if validation error is another range empty" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = EmptyRange SwapTokenRangeField2 }
                        |> Expect.equal False
            , test "remove token range field invalid if validation error is remove range off end of tokens" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = RangeOffEndOfTokens 50 (TokenRange 60 70) RemoveTokenRangeField }
                        |> Expect.equal True
            , test "remove token range field not invalid if validation error is another range off end of tokens" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = RangeOffEndOfTokens 50 (TokenRange 60 70) InsertTokenRangeField }
                        |> Expect.equal False
            , test "remove token range field not invalid if validation error is token ranges overlap" <|
                \() ->
                    isRemoveTokenRangeFieldInvalid { emptyEditDetails | validationError = SwapTokenRangesOverlap }
                        |> Expect.equal False
            ]
        , describe "isSwapTokenRangeField1Invalid tests"
            [ test "first swap token range field not invalid if no validation error" <|
                \() ->
                    isSwapTokenRange1FieldInvalid emptyEditDetails
                        |> Expect.equal False
            , test "first swap token range field not invalid if validation error is no option selected" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = TokenOperationNotSelected }
                        |> Expect.equal False
            , test "first swap token range field invalid if validation error is first swap token range invalid" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = InvalidRange SwapTokenRangeField1 }
                        |> Expect.equal True
            , test "first swap token range field not invalid if validation error is another range invalid" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = InvalidRange SwapTokenRangeField2 }
                        |> Expect.equal False
            , test "first swap token range field invalid if validation error is first swap token range empty" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = EmptyRange SwapTokenRangeField1 }
                        |> Expect.equal True
            , test "first swap token range field not invalid if validation error is another range empty" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = EmptyRange InsertTokenRangeField }
                        |> Expect.equal False
            , test "first swap token range field invalid if validation error is first swap token range off end of tokens" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = RangeOffEndOfTokens 50 (TokenRange 60 70) SwapTokenRangeField1 }
                        |> Expect.equal True
            , test "first swap token range field not invalid if validation error is another range off end of tokens" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = RangeOffEndOfTokens 50 (TokenRange 60 70) RemoveTokenRangeField }
                        |> Expect.equal False
            , test "first swap token range field invalid if validation error is token ranges overlap" <|
                \() ->
                    isSwapTokenRange1FieldInvalid { emptyEditDetails | validationError = SwapTokenRangesOverlap }
                        |> Expect.equal True
            ]
        , describe "isSwapTokenRangeField2Invalid tests"
            [ test "second swap token range field not invalid if no validation error" <|
                \() ->
                    isSwapTokenRange2FieldInvalid emptyEditDetails
                        |> Expect.equal False
            , test "second swap token range field not invalid if validation error is no option selected" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = TokenOperationNotSelected }
                        |> Expect.equal False
            , test "second swap token range field invalid if validation error is second swap token range invalid" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = InvalidRange SwapTokenRangeField2 }
                        |> Expect.equal True
            , test "second swap token range field not invalid if validation error is another range invalid" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = InvalidRange InsertTokenRangeField }
                        |> Expect.equal False
            , test "second swap token range field invalid if validation error is second swap token range empty" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = EmptyRange SwapTokenRangeField2 }
                        |> Expect.equal True
            , test "second swap token range field not invalid if validation error is another range empty" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = EmptyRange RemoveTokenRangeField }
                        |> Expect.equal False
            , test "second swap token range field invalid if validation error is second swap token range off end of tokens" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = RangeOffEndOfTokens 50 (TokenRange 60 70) SwapTokenRangeField2 }
                        |> Expect.equal True
            , test "second swap token range field not invalid if validation error is another range off end of tokens" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = RangeOffEndOfTokens 50 (TokenRange 60 70) SwapTokenRangeField1 }
                        |> Expect.equal False
            , test "second swap token range field invalid if validation error is token ranges overlap" <|
                \() ->
                    isSwapTokenRange2FieldInvalid { emptyEditDetails | validationError = SwapTokenRangesOverlap }
                        |> Expect.equal True
            ]
        ]
