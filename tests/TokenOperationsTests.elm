module TokenOperationsTests exposing (suite)

import Errors exposing (expectError)
import Expect
import Stopwatch exposing (..)
import Test exposing (Test, describe, test)
import TokenOperations
    exposing
        ( RangeEntry
        , TokenOperationEditDetails
        , TokenOperationOptions(..)
        , TokenOperationValidationError(..)
        , TokenRange
        , TokenRangeField(..)
        , emptyEditState
        , parseRange
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
        , describe "validateEditDetails tests"
            [ test "validating the default state returns the nothing-selected options" <|
                \() ->
                    validateEditDetails 50 emptyEditState
                        |> Expect.equal (Just TokenOperationNotSelected)
            , test "validating a valid insert operation returns no error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = InsertTokensOption, insertTokenRange = makeEntry 10 20 }
                        |> Expect.equal Nothing
            , test "validating an insert operation with an invalid range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = InsertTokensOption, insertTokenRange = invalidEntry }
                        |> Expect.equal (Just (InvalidRange InsertTokenRangeField))
            , test "validating an insert operation with an empty range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = InsertTokensOption, insertTokenRange = makeEntry 20 10 }
                        |> Expect.equal (Just (EmptyRange InsertTokenRangeField))
            , test "validating an insert operation with a range off the end returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = InsertTokensOption, insertTokenRange = makeEntry 45 55 }
                        |> Expect.equal (Just (RangeOffEndOfTokens InsertTokenRangeField))
            , test "validating a valid remove operation returns no error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = RemoveTokensOption, removeTokenRange = makeEntry 10 20 }
                        |> Expect.equal Nothing
            , test "validating a remove operation with an invalid range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = RemoveTokensOption, removeTokenRange = invalidEntry }
                        |> Expect.equal (Just (InvalidRange RemoveTokenRangeField))
            , test "validating a remove operation with an empty range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = RemoveTokensOption, removeTokenRange = makeEntry 20 10 }
                        |> Expect.equal (Just (EmptyRange RemoveTokenRangeField))
            , test "validating a remove operation with a range off the end returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = RemoveTokensOption, removeTokenRange = makeEntry 45 55 }
                        |> Expect.equal (Just (RangeOffEndOfTokens RemoveTokenRangeField))
            , test "validating a valid swap operation returns no error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal Nothing
            , test "validating a swap operation with an invalid first range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = invalidEntry, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (Just (InvalidRange SwapTokenRangeField1))
            , test "validating a swap operation with an empty first range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 20 10, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (Just (EmptyRange SwapTokenRangeField1))
            , test "validating a swap operation with a first range off the end returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 45 55, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (Just (RangeOffEndOfTokens SwapTokenRangeField1))
            , test "validating a swap operation with an invalid second range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = invalidEntry }
                        |> Expect.equal (Just (InvalidRange SwapTokenRangeField2))
            , test "validating a swap operation with an empty second range returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 40 30 }
                        |> Expect.equal (Just (EmptyRange SwapTokenRangeField2))
            , test "validating a swap operation with a second range off the end returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 45 55 }
                        |> Expect.equal (Just (RangeOffEndOfTokens SwapTokenRangeField2))
            , test "validating a swap operation with overlapping ranges returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 15 25 }
                        |> Expect.equal (Just SwapTokenRangesOverlap)
            , test "validating a swap operation with ranges overlapping by a single value returns an error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 20 30 }
                        |> Expect.equal (Just SwapTokenRangesOverlap)
            , test "validating a swap operation with both ranges invalid returns an error with the first range" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = invalidEntry, swapTokenRange2 = invalidEntry }
                        |> Expect.equal (Just (InvalidRange SwapTokenRangeField1))
            , test "validating a swap operation with the first range off the end and overlapping the second returns the off-the-end error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 35 55, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (Just (RangeOffEndOfTokens SwapTokenRangeField1))
            , test "validating a swap operation with the second range off the end and overlapping the first returns the off-the-end error" <|
                \() ->
                    validateEditDetails 50 { emptyEditState | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 30 40, swapTokenRange2 = makeEntry 35 55 }
                        |> Expect.equal (Just (RangeOffEndOfTokens SwapTokenRangeField2))
            ]
        ]
