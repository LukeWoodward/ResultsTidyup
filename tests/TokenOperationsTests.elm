module TokenOperationsTests exposing (makeEntry, suite)

import Expect
import Test exposing (Test, describe, test)
import TokenOperations
    exposing
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


makeEntry : Int -> Int -> RangeEntry
makeEntry start end =
    RangeEntry
        (String.fromInt start ++ "-" ++ String.fromInt end)
        (Just (TokenRange start end))


invalidEntry : RangeEntry
invalidEntry =
    RangeEntry "invalid" Nothing


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
                    validateEditDetails 50 emptyEditDetails
                        |> Expect.equal (Just TokenOperationNotSelected)
            , test "validating a valid insert operation returns no error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 10 20 }
                        |> Expect.equal Nothing
            , test "validating an insert operation with an invalid range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = invalidEntry }
                        |> Expect.equal (Just (InvalidRange InsertTokenRangeField))
            , test "validating an insert operation with an empty range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 20 10 }
                        |> Expect.equal (Just (EmptyRange InsertTokenRangeField))
            , test "validating an insert operation with the start of the range off the end of the tokens returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 55 65 }
                        |> Expect.equal (Just (RangeOffEndOfTokens 50 (TokenRange 55 65) InsertTokenRangeField))
            , test "validating an insert operation with only the end of the range off the end of the tokens is valid" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 45 55 }
                        |> Expect.equal Nothing
            , test "validating a valid remove operation returns no error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 10 20 }
                        |> Expect.equal Nothing
            , test "validating a remove operation with an invalid range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = invalidEntry }
                        |> Expect.equal (Just (InvalidRange RemoveTokenRangeField))
            , test "validating a remove operation with an empty range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 20 10 }
                        |> Expect.equal (Just (EmptyRange RemoveTokenRangeField))
            , test "validating a remove operation with a range off the end returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 45 55 }
                        |> Expect.equal (Just (RangeOffEndOfTokens 50 (TokenRange 45 55) RemoveTokenRangeField))
            , test "validating a valid swap operation returns no error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal Nothing
            , test "validating a swap operation with an invalid first range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = invalidEntry, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (Just (InvalidRange SwapTokenRangeField1))
            , test "validating a swap operation with an empty first range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 20 10, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (Just (EmptyRange SwapTokenRangeField1))
            , test "validating a swap operation with a first range off the end returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 45 55, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (Just (RangeOffEndOfTokens 50 (TokenRange 45 55) SwapTokenRangeField1))
            , test "validating a swap operation with an invalid second range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = invalidEntry }
                        |> Expect.equal (Just (InvalidRange SwapTokenRangeField2))
            , test "validating a swap operation with an empty second range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 40 30 }
                        |> Expect.equal (Just (EmptyRange SwapTokenRangeField2))
            , test "validating a swap operation with a second range off the end returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 45 55 }
                        |> Expect.equal (Just (RangeOffEndOfTokens 50 (TokenRange 45 55) SwapTokenRangeField2))
            , test "validating a swap operation with overlapping ranges returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 15 25 }
                        |> Expect.equal (Just SwapTokenRangesOverlap)
            , test "validating a swap operation with ranges overlapping by a single value returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 20 30 }
                        |> Expect.equal (Just SwapTokenRangesOverlap)
            , test "validating a swap operation with both ranges invalid returns an error with the first range" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = invalidEntry, swapTokenRange2 = invalidEntry }
                        |> Expect.equal (Just (InvalidRange SwapTokenRangeField1))
            , test "validating a swap operation with the first range off the end and overlapping the second returns the off-the-end error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 35 55, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (Just (RangeOffEndOfTokens 50 (TokenRange 35 55) SwapTokenRangeField1))
            , test "validating a swap operation with the second range off the end and overlapping the first returns the off-the-end error" <|
                \() ->
                    validateEditDetails 50 { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 30 40, swapTokenRange2 = makeEntry 35 55 }
                        |> Expect.equal (Just (RangeOffEndOfTokens 50 (TokenRange 35 55) SwapTokenRangeField2))
            ]
        ]
