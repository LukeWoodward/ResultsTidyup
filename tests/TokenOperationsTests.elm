module TokenOperationsTests exposing (suite)

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionStatus(..)
        , LineContents(..)
        , PositionAndTimePair
        , empty
        , regenerate
        )
import DataEntry exposing (Range, RangeEntry)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Set exposing (Set)
import Test exposing (Test, describe, test)
import TokenOperations
    exposing
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


makeEntry : Int -> Int -> RangeEntry
makeEntry start end =
    RangeEntry
        (String.fromInt start ++ "-" ++ String.fromInt end)
        (Just (Range start end))


invalidEntry : RangeEntry
invalidEntry =
    RangeEntry "invalid" Nothing


tokenSet : Set Int
tokenSet =
    Set.fromList [ 1, 2, 8, 14, 17, 23, 26, 27, 28, 41, 48, 50 ]


makeBarcodeScannerData : List ( Int, String ) -> BarcodeScannerData
makeBarcodeScannerData items =
    let
        lines : List BarcodeScannerFileLine
        lines =
            List.indexedMap (\index ( position, athlete ) -> { lineNumber = index + 1, contents = Ordinary athlete (Just position), scanDateTime = "scanDateTime", deletionStatus = NotDeleted }) items

        files : List BarcodeScannerFile
        files =
            [ { name = "file1.txt", lines = lines, maxScanDateTime = Nothing } ]
    in
    regenerate { empty | files = files }


barcodeScannerDataForTokenOperationsTesting : BarcodeScannerData
barcodeScannerDataForTokenOperationsTesting =
    makeBarcodeScannerData [ ( 1, "A48223" ), ( 2, "A37192" ), ( 3, "A60804" ), ( 4, "A53779" ), ( 5, "A84311" ), ( 6, "" ), ( 7, "A29046" ), ( 8, "A76535" ), ( 9, "" ), ( 10, "A12680" ) ]


allValidationErrors : TokenRangeField -> List ( String, TokenOperationValidationError )
allValidationErrors field =
    let
        differentField : TokenRangeField
        differentField =
            case field of
                InsertTokenRangeField ->
                    RemoveTokenRangeField

                RemoveTokenRangeField ->
                    SwapTokenRangeField1

                SwapTokenRangeField1 ->
                    SwapTokenRangeField2

                SwapTokenRangeField2 ->
                    ReverseTokenRangeField

                ReverseTokenRangeField ->
                    InsertTokenRangeField

        rangeOffEndOfTokensError : TokenRangeField -> TokenOperationValidationError
        rangeOffEndOfTokensError errorField =
            if errorField == InsertTokenRangeField then
                InsertRangeOffEndOfTokens 50 (Range 60 70)

            else
                RangeOffEndOfTokens 65 (Range 60 70) errorField
    in
    [ ( "noValidationError", NoValidationError )
    , ( "operationNotSelected", TokenOperationNotSelected )
    , ( "fieldHasInvalidRange", InvalidRange field )
    , ( "differentFieldHasInvalidRange", InvalidRange differentField )
    , ( "fieldHasEmptyRange", EmptyRange field )
    , ( "differentFieldHasEmptyRange", EmptyRange differentField )
    , ( "fieldHasRangeOffEndOfTokens", rangeOffEndOfTokensError field )
    , ( "differentFieldHasRangeOffEndOfTokens", rangeOffEndOfTokensError differentField )
    , ( "removingExistingTokens", RemovingExistingTokens [ 65 ] (Range 60 70) )
    , ( "swapTokenRangesOverlap", SwapTokenRangesOverlap )
    , ( "reverseSingleTokenOnly", ReverseTokenRangeSingleToken )
    ]


runFieldValidationTest : (TokenOperationEditDetails -> Bool) -> TokenRangeField -> List String -> Expectation
runFieldValidationTest validationFunction field expectedFields =
    allValidationErrors field
        |> List.filter (\( name, error ) -> validationFunction { emptyEditDetails | validationError = error })
        |> List.map Tuple.first
        |> Expect.equal expectedFields


suite : Test
suite =
    describe "TokenOperations tests"
        [ describe "validateEditDetails tests"
            [ test "validating the default details returns the nothing-selected error" <|
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
            , test "validating an insert operation with the range starting at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 0 10 }
                        |> Expect.equal (ZeroInRange InsertTokenRangeField)
            , test "validating an insert operation with the range ending at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry -10 0 }
                        |> Expect.equal (ZeroInRange InsertTokenRangeField)
            , test "validating an insert operation with the start of the range off the end of the tokens returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = makeEntry 55 65 }
                        |> Expect.equal (InsertRangeOffEndOfTokens 50 (Range 55 65))
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
            , test "validating a remove operation with the range starting at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 0 10 }
                        |> Expect.equal (ZeroInRange RemoveTokenRangeField)
            , test "validating a remove operation with the range ending at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry -10 0 }
                        |> Expect.equal (ZeroInRange RemoveTokenRangeField)
            , test "validating a remove operation that attempts to remove unused tokens returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 40 50 }
                        |> Expect.equal (RemovingExistingTokens [ 41, 48, 50 ] (Range 40 50))
            , test "validating a remove operation with a range off the end returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = makeEntry 45 55 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (Range 45 55) RemoveTokenRangeField)
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
            , test "validating a swap operation with the first range starting at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 0 10, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (ZeroInRange SwapTokenRangeField1)
            , test "validating a swap operation with the first range ending at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry -10 0, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (ZeroInRange SwapTokenRangeField1)
            , test "validating a swap operation with a first range off the end returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 45 55, swapTokenRange2 = makeEntry 30 40 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (Range 45 55) SwapTokenRangeField1)
            , test "validating a swap operation with an invalid second range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = invalidEntry }
                        |> Expect.equal (InvalidRange SwapTokenRangeField2)
            , test "validating a swap operation with an empty second range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 40 30 }
                        |> Expect.equal (EmptyRange SwapTokenRangeField2)
            , test "validating a swap operation with the second range starting at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 30 40, swapTokenRange2 = makeEntry 0 10 }
                        |> Expect.equal (ZeroInRange SwapTokenRangeField2)
            , test "validating a swap operation with the second range ending at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 30 40, swapTokenRange2 = makeEntry -10 0 }
                        |> Expect.equal (ZeroInRange SwapTokenRangeField2)
            , test "validating a swap operation with a second range off the end returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 45 55 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (Range 45 55) SwapTokenRangeField2)
            , test "validating a swap operation with ranges of different sizes returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 30 35 }
                        |> Expect.equal SwapTokenRangesOfDifferentSizes
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
                        |> Expect.equal (RangeOffEndOfTokens 50 (Range 35 55) SwapTokenRangeField1)
            , test "validating a swap operation with the second range off the end and overlapping the first returns the off-the-end error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 30 40, swapTokenRange2 = makeEntry 35 55 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (Range 35 55) SwapTokenRangeField2)
            , test "validating a swap operation with ranges of different sizes and which overlap returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 10 20, swapTokenRange2 = makeEntry 18 22 }
                        |> Expect.equal SwapTokenRangesOfDifferentSizes
            , test "validating a valid reverse operation returns no error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = makeEntry 30 40 }
                        |> Expect.equal NoValidationError
            , test "validating a reverse operation with an invalid range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = invalidEntry }
                        |> Expect.equal (InvalidRange ReverseTokenRangeField)
            , test "validating a reverse operation with an empty range returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = makeEntry 20 10 }
                        |> Expect.equal (EmptyRange ReverseTokenRangeField)
            , test "validating a reverse operation with the range starting at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = makeEntry 0 10 }
                        |> Expect.equal (ZeroInRange ReverseTokenRangeField)
            , test "validating a reverse operation with the range ending at zero returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = makeEntry -10 0 }
                        |> Expect.equal (ZeroInRange ReverseTokenRangeField)
            , test "validating a reverse operation that attempts to reverse a single token returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = makeEntry 40 40 }
                        |> Expect.equal ReverseTokenRangeSingleToken
            , test "validating a reverse operation with a range off the end returns an error" <|
                \() ->
                    validateEditDetails tokenSet { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = makeEntry 45 55 }
                        |> Expect.equal (RangeOffEndOfTokens 50 (Range 45 55) ReverseTokenRangeField)
            ]
        , describe "updateEditDetails tests"
            [ test "ChangeOperation changes the operation and clears validation" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = RangeEntry "" Nothing, validationError = TokenOperationNotSelected }
                    in
                    updateEditDetails (ChangeOperation InsertTokensOption) initialDetails
                        |> Expect.equal { emptyEditDetails | operation = InsertTokensOption, validationError = NoValidationError }
            , test "RangeEdited updates the insert-tokens range and clears validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = RangeEntry "" Nothing, validationError = InvalidRange InsertTokenRangeField }
                    in
                    updateEditDetails (RangeEdited InsertTokenRangeField "30-40") initialDetails
                        |> Expect.equal { initialDetails | insertTokenRange = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the insert-tokens range and clears validation even with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = InsertTokensOption, insertTokenRange = RangeEntry "" Nothing, validationError = InvalidRange InsertTokenRangeField }
                    in
                    updateEditDetails (RangeEdited InsertTokenRangeField "40-30") initialDetails
                        |> Expect.equal { initialDetails | insertTokenRange = makeEntry 40 30, validationError = NoValidationError }
            , test "RangeEdited updates the remove-tokens range and clears validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = RangeEntry "" Nothing, validationError = InvalidRange RemoveTokenRangeField }
                    in
                    updateEditDetails (RangeEdited RemoveTokenRangeField "30-40") initialDetails
                        |> Expect.equal { initialDetails | removeTokenRange = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the remove-tokens range and clears validation even with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = RemoveTokensOption, removeTokenRange = RangeEntry "" Nothing, validationError = InvalidRange RemoveTokenRangeField }
                    in
                    updateEditDetails (RangeEdited RemoveTokenRangeField "40-30") initialDetails
                        |> Expect.equal { initialDetails | removeTokenRange = makeEntry 40 30, validationError = NoValidationError }
            , test "RangeEdited updates the first swap-tokens range and clears validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = RangeEntry "" Nothing, swapTokenRange2 = makeEntry 15 25, validationError = InvalidRange SwapTokenRangeField1 }
                    in
                    updateEditDetails (RangeEdited SwapTokenRangeField1 "30-40") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange1 = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the first swap-tokens range and clears validation even with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = RangeEntry "" Nothing, swapTokenRange2 = makeEntry 15 25, validationError = InvalidRange SwapTokenRangeField1 }
                    in
                    updateEditDetails (RangeEdited SwapTokenRangeField1 "40-30") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange1 = makeEntry 40 30, validationError = NoValidationError }
            , test "RangeEdited updates the second swap-tokens range and runs validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 15 25, swapTokenRange2 = RangeEntry "" Nothing, validationError = InvalidRange SwapTokenRangeField2 }
                    in
                    updateEditDetails (RangeEdited SwapTokenRangeField2 "30-40") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange2 = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the second swap-tokens range and clears validation even with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = SwapTokenRangeOption, swapTokenRange1 = makeEntry 15 25, swapTokenRange2 = RangeEntry "" Nothing, validationError = InvalidRange SwapTokenRangeField2 }
                    in
                    updateEditDetails (RangeEdited SwapTokenRangeField2 "40-30") initialDetails
                        |> Expect.equal { initialDetails | swapTokenRange2 = makeEntry 40 30, validationError = NoValidationError }
            , test "RangeEdited updates the reverse-tokens range and clears validation with a valid range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = RangeEntry "" Nothing, validationError = InvalidRange ReverseTokenRangeField }
                    in
                    updateEditDetails (RangeEdited ReverseTokenRangeField "30-40") initialDetails
                        |> Expect.equal { initialDetails | reverseTokenRange = makeEntry 30 40, validationError = NoValidationError }
            , test "RangeEdited updates the reverse-tokens range and clears validation even with an empty range" <|
                \() ->
                    let
                        initialDetails : TokenOperationEditDetails
                        initialDetails =
                            { emptyEditDetails | operation = ReverseTokenRangeOption, reverseTokenRange = RangeEntry "" Nothing, validationError = InvalidRange ReverseTokenRangeField }
                    in
                    updateEditDetails (RangeEdited ReverseTokenRangeField "40-30") initialDetails
                        |> Expect.equal { initialDetails | reverseTokenRange = makeEntry 40 30, validationError = NoValidationError }
            ]
        , describe "Field validation function tests"
            [ test "isInsertTokenRangeFieldInvalid returns correct results" <|
                \() ->
                    runFieldValidationTest isInsertTokenRangeFieldInvalid InsertTokenRangeField [ "fieldHasInvalidRange", "fieldHasEmptyRange", "fieldHasRangeOffEndOfTokens" ]
            , test "isRemoveTokenRangeFieldInvalid returns correct results" <|
                \() ->
                    runFieldValidationTest isRemoveTokenRangeFieldInvalid RemoveTokenRangeField [ "fieldHasInvalidRange", "fieldHasEmptyRange", "fieldHasRangeOffEndOfTokens", "removingExistingTokens" ]
            , test "isSwapTokenRange1FieldInvalid returns correct results" <|
                \() ->
                    runFieldValidationTest isSwapTokenRange1FieldInvalid SwapTokenRangeField1 [ "fieldHasInvalidRange", "fieldHasEmptyRange", "fieldHasRangeOffEndOfTokens", "swapTokenRangesOverlap" ]
            , test "isSwapTokenRange2FieldInvalid returns correct results" <|
                \() ->
                    runFieldValidationTest isSwapTokenRange2FieldInvalid SwapTokenRangeField2 [ "fieldHasInvalidRange", "fieldHasEmptyRange", "fieldHasRangeOffEndOfTokens", "swapTokenRangesOverlap" ]
            , test "isReverseTokenRangeFieldInvalid returns correct results" <|
                \() ->
                    runFieldValidationTest isReverseTokenRangeFieldInvalid ReverseTokenRangeField [ "fieldHasInvalidRange", "fieldHasEmptyRange", "fieldHasRangeOffEndOfTokens", "reverseSingleTokenOnly" ]
            ]
        , describe "tryApplyTokenOperationToBarcodeScannerData tests"
            [ describe "general tests"
                [ test "applying the operation with no operation selected does nothing" <|
                    \() ->
                        tryApplyTokenOperationToBarcodeScannerData emptyEditDetails barcodeScannerDataForTokenOperationsTesting
                            |> Expect.equal (Err TokenOperationNotSelected)
                ]
            , describe "insert operation tests"
                [ test "applying an insert operation with an invalid range does nothing" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = InsertTokensOption
                                    , insertTokenRange = RangeEntry "Invalid range" Nothing
                                }
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails barcodeScannerDataForTokenOperationsTesting
                            |> Expect.equal (Err (InvalidRange InsertTokenRangeField))
                , test "applying an insert operation has the expected effect" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = InsertTokensOption
                                    , insertTokenRange = RangeEntry "8-10" (Just (Range 8 10))
                                }

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                makeBarcodeScannerData
                                    [ ( 1, "A48223" ), ( 2, "A37192" ), ( 3, "A60804" ), ( 4, "A53779" ), ( 5, "A84311" ), ( 6, "" ), ( 7, "A29046" ), ( 11, "A76535" ), ( 12, "" ), ( 13, "A12680" ) ]
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails barcodeScannerDataForTokenOperationsTesting
                            |> Expect.equal (Ok expectedBarcodeScannerData)
                ]
            , describe "remove operation tests"
                [ test "applying a remove operation with an invalid range returns an error" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = RemoveTokensOption
                                    , removeTokenRange = RangeEntry "Invalid range" Nothing
                                }
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails barcodeScannerDataForTokenOperationsTesting
                            |> Expect.equal (Err (InvalidRange RemoveTokenRangeField))
                , test "applying a remove operation has the expected effect" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = RemoveTokensOption
                                    , removeTokenRange = RangeEntry "8-10" (Just (Range 8 10))
                                }

                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                makeBarcodeScannerData
                                    [ ( 1, "A48223" ), ( 2, "A37192" ), ( 3, "A60804" ), ( 4, "A53779" ), ( 5, "A84311" ), ( 6, "" ), ( 7, "A29046" ), ( 11, "A76535" ), ( 12, "" ), ( 13, "A12680" ) ]
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails initialBarcodeScannerData
                            |> Expect.equal (Ok barcodeScannerDataForTokenOperationsTesting)
                ]
            , describe "swap operation tests"
                [ test "applying a swap operation with an invalid first range returns an error" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = SwapTokenRangeOption
                                    , swapTokenRange1 = RangeEntry "Invalid range" Nothing
                                    , swapTokenRange2 = RangeEntry "8-10" (Just (Range 8 10))
                                }
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails barcodeScannerDataForTokenOperationsTesting
                            |> Expect.equal (Err (InvalidRange SwapTokenRangeField1))
                , test "applying a swap operation with an invalid second range returns an error" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = SwapTokenRangeOption
                                    , swapTokenRange1 = RangeEntry "8-10" (Just (Range 8 10))
                                    , swapTokenRange2 = RangeEntry "Invalid range" Nothing
                                }
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails barcodeScannerDataForTokenOperationsTesting
                            |> Expect.equal (Err (InvalidRange SwapTokenRangeField2))
                , test "applying a swap operation has the expected effect" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = SwapTokenRangeOption
                                    , swapTokenRange1 = RangeEntry "4-6" (Just (Range 4 6))
                                    , swapTokenRange2 = RangeEntry "8-10" (Just (Range 8 10))
                                }

                            expectedBarcodeScannerData : BarcodeScannerData
                            expectedBarcodeScannerData =
                                makeBarcodeScannerData [ ( 1, "A48223" ), ( 2, "A37192" ), ( 3, "A60804" ), ( 8, "A53779" ), ( 9, "A84311" ), ( 10, "" ), ( 7, "A29046" ), ( 4, "A76535" ), ( 5, "" ), ( 6, "A12680" ) ]
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails barcodeScannerDataForTokenOperationsTesting
                            |> Expect.equal (Ok expectedBarcodeScannerData)
                ]
            , describe "reverse operation tests"
                [ test "applying a reverse operation with an invalid range returns an error" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = ReverseTokenRangeOption
                                    , reverseTokenRange = RangeEntry "Invalid range" Nothing
                                }
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails barcodeScannerDataForTokenOperationsTesting
                            |> Expect.equal (Err (InvalidRange ReverseTokenRangeField))
                , test "applying a reverse operation has the expected effect" <|
                    \() ->
                        let
                            editDetails : TokenOperationEditDetails
                            editDetails =
                                { emptyEditDetails
                                    | operation = ReverseTokenRangeOption
                                    , reverseTokenRange = RangeEntry "5-8" (Just (Range 5 8))
                                }

                            initialBarcodeScannerData : BarcodeScannerData
                            initialBarcodeScannerData =
                                makeBarcodeScannerData
                                    [ ( 1, "A48223" ), ( 2, "A37192" ), ( 3, "A60804" ), ( 4, "A53779" ), ( 8, "A84311" ), ( 7, "" ), ( 6, "A29046" ), ( 5, "A76535" ), ( 9, "" ), ( 10, "A12680" ) ]
                        in
                        tryApplyTokenOperationToBarcodeScannerData editDetails initialBarcodeScannerData
                            |> Expect.equal (Ok barcodeScannerDataForTokenOperationsTesting)
                ]
            ]
        ]
