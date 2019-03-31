module NumericEntryTests exposing (suite)

import Expect
import NumericEntry
    exposing
        ( NumericEntry
        , emptyNumericEntry
        , isValidEntry
        , numericEntryFromAthleteNumber
        , numericEntryFromInt
        , numericEntryFromMaybeInt
        , numericEntryFromString
        )
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "NumericEntry tests"
        [ describe "numericEntryFromInt tests"
            [ test "Can create a numeric entry from an int" <|
                \() ->
                    numericEntryFromInt 149846
                        |> Expect.equal (NumericEntry "149846" (Just 149846))
            ]
        , describe "numericEntryFromMaybeInt tests"
            [ test "Can create a numeric entry from a Maybe Int with a value" <|
                \() ->
                    numericEntryFromMaybeInt (Just 72094)
                        |> Expect.equal (NumericEntry "72094" (Just 72094))
            , test "Can create a numeric entry from Nothing" <|
                \() ->
                    numericEntryFromMaybeInt Nothing
                        |> Expect.equal (NumericEntry "" Nothing)
            ]
        , describe "numericEntryFromString tests"
            [ test "Can create a numeric entry from a string containing a valid int value" <|
                \() ->
                    numericEntryFromString "50554"
                        |> Expect.equal (NumericEntry "50554" (Just 50554))
            , test "Can create a numeric entry from a string containing an invalid int value" <|
                \() ->
                    numericEntryFromString "This is not valid"
                        |> Expect.equal (NumericEntry "This is not valid" Nothing)
            ]
        , describe "numericEntryFromAthleteNumber tests"
            [ test "Can create a numeric entry from an athlete number containing an A and a valid int value" <|
                \() ->
                    numericEntryFromAthleteNumber "A450442"
                        |> Expect.equal (NumericEntry "A450442" (Just 450442))
            , test "Can create a numeric entry from an athlete number containing an invalid value starting with an A" <|
                \() ->
                    numericEntryFromAthleteNumber "Absolutely not valid"
                        |> Expect.equal (NumericEntry "Absolutely not valid" Nothing)
            , test "Can create a numeric entry from an athlete number containing a valid int value" <|
                \() ->
                    numericEntryFromAthleteNumber "499702"
                        |> Expect.equal (NumericEntry "499702" (Just 499702))
            , test "Can create a numeric entry from a string containing an invalid value not beginning with A" <|
                \() ->
                    numericEntryFromAthleteNumber "This is not valid"
                        |> Expect.equal (NumericEntry "This is not valid" Nothing)
            ]
        , describe "isValidEntry tests"
            [ test "An empty value is valid" <|
                \() ->
                    isValidEntry emptyNumericEntry
                        |> Expect.true "Empty value should be valid"
            , test "An valid value is valid" <|
                \() ->
                    isValidEntry (NumericEntry "44092" (Just 44092))
                        |> Expect.true "Valid positive value should be valid"
            , test "A negative value is invalid" <|
                \() ->
                    isValidEntry (NumericEntry "-442" (Just -442))
                        |> Expect.false "Negative value should not be valid"
            , test "A zero value is invalid" <|
                \() ->
                    isValidEntry (NumericEntry "0" (Just 0))
                        |> Expect.false "Zero value should not be valid"
            , test "An invalid value is invalid" <|
                \() ->
                    isValidEntry (NumericEntry "This is not valid" Nothing)
                        |> Expect.false "Invalid value should not be valid"
            ]
        ]
