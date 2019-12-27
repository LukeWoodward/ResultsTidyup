module NumericEntryTests exposing (suite)

import Expect
import NumericEntry
    exposing
        ( IntegerEntry
        , emptyIntegerEntry
        , integerEntryFromAthleteNumber
        , integerEntryFromInt
        , integerEntryFromMaybeInt
        , integerEntryFromString
        , isValidEntry
        )
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "IntegerEntry tests"
        [ describe "integerEntryFromInt tests"
            [ test "Can create a numeric entry from an int" <|
                \() ->
                    integerEntryFromInt 149846
                        |> Expect.equal (IntegerEntry "149846" (Just 149846))
            ]
        , describe "integerEntryFromMaybeInt tests"
            [ test "Can create a numeric entry from a Maybe Int with a value" <|
                \() ->
                    integerEntryFromMaybeInt (Just 72094)
                        |> Expect.equal (IntegerEntry "72094" (Just 72094))
            , test "Can create a numeric entry from Nothing" <|
                \() ->
                    integerEntryFromMaybeInt Nothing
                        |> Expect.equal (IntegerEntry "" Nothing)
            ]
        , describe "integerEntryFromString tests"
            [ test "Can create a numeric entry from a string containing a valid int value" <|
                \() ->
                    integerEntryFromString "50554"
                        |> Expect.equal (IntegerEntry "50554" (Just 50554))
            , test "Can create a numeric entry from a string containing an invalid int value" <|
                \() ->
                    integerEntryFromString "This is not valid"
                        |> Expect.equal (IntegerEntry "This is not valid" Nothing)
            ]
        , describe "integerEntryFromAthleteNumber tests"
            [ test "Can create a numeric entry from an athlete number containing an A and a valid int value" <|
                \() ->
                    integerEntryFromAthleteNumber "A450442"
                        |> Expect.equal (IntegerEntry "A450442" (Just 450442))
            , test "Can create a numeric entry from an athlete number containing an invalid value starting with an A" <|
                \() ->
                    integerEntryFromAthleteNumber "Absolutely not valid"
                        |> Expect.equal (IntegerEntry "Absolutely not valid" Nothing)
            , test "Can create a numeric entry from an athlete number containing a valid int value" <|
                \() ->
                    integerEntryFromAthleteNumber "499702"
                        |> Expect.equal (IntegerEntry "499702" (Just 499702))
            , test "Can create a numeric entry from a string containing an invalid value not beginning with A" <|
                \() ->
                    integerEntryFromAthleteNumber "This is not valid"
                        |> Expect.equal (IntegerEntry "This is not valid" Nothing)
            ]
        , describe "isValidEntry tests"
            [ test "An empty value is valid" <|
                \() ->
                    isValidEntry emptyIntegerEntry
                        |> Expect.true "Empty value should be valid"
            , test "An valid value is valid" <|
                \() ->
                    isValidEntry (IntegerEntry "44092" (Just 44092))
                        |> Expect.true "Valid positive value should be valid"
            , test "A negative value is invalid" <|
                \() ->
                    isValidEntry (IntegerEntry "-442" (Just -442))
                        |> Expect.false "Negative value should not be valid"
            , test "A zero value is invalid" <|
                \() ->
                    isValidEntry (IntegerEntry "0" (Just 0))
                        |> Expect.false "Zero value should not be valid"
            , test "An invalid value is invalid" <|
                \() ->
                    isValidEntry (IntegerEntry "This is not valid" Nothing)
                        |> Expect.false "Invalid value should not be valid"
            ]
        ]
