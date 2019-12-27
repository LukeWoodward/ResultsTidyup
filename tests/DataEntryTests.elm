module DataEntryTests exposing (suite)

import DataEntry
    exposing
        ( FloatEntry
        , IntegerEntry
        , floatEntryFromString
        , integerEntryFromAthleteNumber
        , integerEntryFromInt
        , integerEntryFromMaybeInt
        , integerEntryFromString
        , isValidEntry
        )
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "IntegerEntry tests"
        [ describe "integerEntryFromInt tests"
            [ test "Can create an integer entry from an int" <|
                \() ->
                    integerEntryFromInt 149846
                        |> Expect.equal (IntegerEntry "149846" (Just 149846))
            ]
        , describe "integerEntryFromMaybeInt tests"
            [ test "Can create an integer entry from a Maybe Int with a value" <|
                \() ->
                    integerEntryFromMaybeInt (Just 72094)
                        |> Expect.equal (IntegerEntry "72094" (Just 72094))
            , test "Can create an integer entry from Nothing" <|
                \() ->
                    integerEntryFromMaybeInt Nothing
                        |> Expect.equal (IntegerEntry "" Nothing)
            ]
        , describe "integerEntryFromString tests"
            [ test "Can create an integer entry from a string containing a valid int value" <|
                \() ->
                    integerEntryFromString "50554"
                        |> Expect.equal (IntegerEntry "50554" (Just 50554))
            , test "Can create an integer entry from a string containing an invalid int value" <|
                \() ->
                    integerEntryFromString "This is not valid"
                        |> Expect.equal (IntegerEntry "This is not valid" Nothing)
            ]
        , describe "floatEntryFromString tests"
            [ test "Can create a float entry from a string containing a valid float value" <|
                \() ->
                    floatEntryFromString "654.32"
                        |> Expect.equal (FloatEntry "654.32" (Just 654.32))
            , test "Can create a float entry from a string containing an invalid float value" <|
                \() ->
                    floatEntryFromString "This is not valid"
                        |> Expect.equal (FloatEntry "This is not valid" Nothing)
            ]
        , describe "integerEntryFromAthleteNumber tests"
            [ test "Can create an integer entry from an athlete number containing an A and a valid int value" <|
                \() ->
                    integerEntryFromAthleteNumber "A450442"
                        |> Expect.equal (IntegerEntry "A450442" (Just 450442))
            , test "Can create an integer entry from an athlete number containing an invalid value starting with an A" <|
                \() ->
                    integerEntryFromAthleteNumber "Absolutely not valid"
                        |> Expect.equal (IntegerEntry "Absolutely not valid" Nothing)
            , test "Can create an integer entry from an athlete number containing a valid int value" <|
                \() ->
                    integerEntryFromAthleteNumber "499702"
                        |> Expect.equal (IntegerEntry "499702" (Just 499702))
            , test "Can create an integer entry from a string containing an invalid value not beginning with A" <|
                \() ->
                    integerEntryFromAthleteNumber "This is not valid"
                        |> Expect.equal (IntegerEntry "This is not valid" Nothing)
            ]
        , describe "isValidEntry tests"
            [ test "An empty value is valid" <|
                \() ->
                    isValidEntry (IntegerEntry "" Nothing)
                        |> Expect.true "Empty integer value should be valid"
            , test "An empty float value is valid" <|
                \() ->
                    isValidEntry (FloatEntry "" Nothing)
                        |> Expect.true "Empty float value should be valid"
            , test "A valid integer value is valid" <|
                \() ->
                    isValidEntry (IntegerEntry "44092" (Just 44092))
                        |> Expect.true "Valid positive integer value should be valid"
            , test "A valid float value is valid" <|
                \() ->
                    isValidEntry (FloatEntry "123.45" (Just 123.45))
                        |> Expect.true "Valid positive integer value should be valid"
            , test "A negative integer value is invalid" <|
                \() ->
                    isValidEntry (IntegerEntry "-442" (Just -442))
                        |> Expect.false "Negative integer value should not be valid"
            , test "A negative float value is invalid" <|
                \() ->
                    isValidEntry (FloatEntry "-123.45" (Just -123.45))
                        |> Expect.false "Negative integer value should not be valid"
            , test "A zero integer value is invalid" <|
                \() ->
                    isValidEntry (IntegerEntry "0" (Just 0))
                        |> Expect.false "Zero integer value should not be valid"
            , test "A zero float value is invalid" <|
                \() ->
                    isValidEntry (FloatEntry "0.0" (Just 0.0))
                        |> Expect.false "Zero float value should not be valid"
            , test "An invalid integer value is invalid" <|
                \() ->
                    isValidEntry (IntegerEntry "This is not valid" Nothing)
                        |> Expect.false "Invalid integer value should not be valid"
            , test "An invalid float value is invalid" <|
                \() ->
                    isValidEntry (FloatEntry "This is not valid" Nothing)
                        |> Expect.false "Invalid float value should not be valid"
            ]
        ]
