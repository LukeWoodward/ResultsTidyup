module DataEntryTests exposing (suite)

import DataEntry
    exposing
        ( FloatEntry
        , IntegerEntry
        , Range
        , RangeEntry
        , emptyEntry
        , floatEntryFromString
        , integerEntryFromAthleteNumber
        , integerEntryFromHoursAndMinutes
        , integerEntryFromInt
        , integerEntryFromMaybeInt
        , integerEntryFromString
        , integerEntryFromTime
        , isValidEntry
        , rangeEntryFromString
        , rangeToString
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
                        |> Expect.equal emptyEntry
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
        , describe "integerEntryFromHoursAndMinutes tests"
            [ test "Can create an integer entry from valid hours and minutes values " <|
                \() ->
                    integerEntryFromHoursAndMinutes "12:35"
                        |> Expect.equal (IntegerEntry "12:35" (Just (12 * 60 + 35)))
            , test "Can create an integer entry from invalid hours and minutes values " <|
                \() ->
                    integerEntryFromHoursAndMinutes "This is not valid"
                        |> Expect.equal (IntegerEntry "This is not valid" Nothing)
            ]
        , describe "integerEntryFromTime tests"
            [ test "Can create an integer entry from a valid time value without hours" <|
                \() ->
                    integerEntryFromTime "29:44"
                        |> Expect.equal (IntegerEntry "29:44" (Just (29 * 60 + 44)))
            , test "Can create an integer entry from a valid time value with hours" <|
                \() ->
                    integerEntryFromTime "02:29:44"
                        |> Expect.equal (IntegerEntry "02:29:44" (Just (2 * 60 * 60 + 29 * 60 + 44)))
            , test "Can create an integer entry from an invalid time " <|
                \() ->
                    integerEntryFromTime "This is not valid"
                        |> Expect.equal (IntegerEntry "This is not valid" Nothing)
            ]
        , describe "rangeEntryFromString tests"
            [ test "rangeEntryFromString of an empty string is not valid" <|
                \() ->
                    rangeEntryFromString ""
                        |> Expect.equal emptyEntry
            , test "rangeEntryFromString of a string containing an invalid number is not valid" <|
                \() ->
                    rangeEntryFromString "this is not valid"
                        |> Expect.equal (RangeEntry "this is not valid" Nothing)
            , test "rangeEntryFromString of a string containing a single valid number is valid" <|
                \() ->
                    rangeEntryFromString "37"
                        |> Expect.equal (RangeEntry "37" (Just (Range 37 37)))
            , test "rangeEntryFromString of a string containing a single valid number with whitespace is valid" <|
                \() ->
                    rangeEntryFromString "    37      "
                        |> Expect.equal (RangeEntry "    37      " (Just (Range 37 37)))
            , test "rangeEntryFromString of a string containing two valid numbers is valid" <|
                \() ->
                    rangeEntryFromString "22-34"
                        |> Expect.equal (RangeEntry "22-34" (Just (Range 22 34)))
            , test "rangeEntryFromString of a string containing two valid numbers with whitespace is valid" <|
                \() ->
                    rangeEntryFromString "     22  -   34   "
                        |> Expect.equal (RangeEntry "     22  -   34   " (Just (Range 22 34)))
            , test "rangeEntryFromString of a string with a missing end number is invalid" <|
                \() ->
                    rangeEntryFromString "22-"
                        |> Expect.equal (RangeEntry "22-" Nothing)
            , test "rangeEntryFromString of a string with a missing start number is invalid" <|
                \() ->
                    rangeEntryFromString "-34"
                        |> Expect.equal (RangeEntry "-34" Nothing)
            , test "rangeEntryFromString of a string containing three valid numbers is invalid" <|
                \() ->
                    rangeEntryFromString "22-34-56"
                        |> Expect.equal (RangeEntry "22-34-56" Nothing)
            , test "rangeEntryFromString of a string containing the same valid number twice is valid" <|
                \() ->
                    rangeEntryFromString "34-34"
                        |> Expect.equal (RangeEntry "34-34" (Just (Range 34 34)))
            , test "rangeEntryFromString of a string containing two valid numbers the wrong way around is valid" <|
                \() ->
                    rangeEntryFromString "34-22"
                        |> Expect.equal (RangeEntry "34-22" (Just (Range 34 22)))
            ]
        , describe "rangeToString tests"
            [ test "formatting a single-value range returns a single number" <|
                \() ->
                    rangeToString (Range 59 59)
                        |> Expect.equal "59"
            , test "formatting a multi-value range returns two hyphen-separated numbers" <|
                \() ->
                    rangeToString (Range 47 52)
                        |> Expect.equal "47-52"
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
