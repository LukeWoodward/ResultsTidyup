module DataEntry exposing
    ( DateEntry
    , FloatEntry
    , IntegerEntry
    , emptyEntry
    , floatEntryFromString
    , integerEntryFromAthleteNumber
    , integerEntryFromInt
    , integerEntryFromMaybeInt
    , integerEntryFromString
    , isValidEntry
    )

import Time exposing (Posix)


type alias Entry a =
    { enteredValue : String
    , parsedValue : Maybe a
    }


type alias IntegerEntry =
    { enteredValue : String
    , parsedValue : Maybe Int
    }


type alias FloatEntry =
    { enteredValue : String
    , parsedValue : Maybe Float
    }


type alias DateEntry =
    { enteredValue : String
    , parsedValue : Maybe Posix
    }


emptyEntry : Entry a
emptyEntry =
    Entry "" Nothing


integerEntryFromString : String -> IntegerEntry
integerEntryFromString stringValue =
    IntegerEntry stringValue (String.toInt stringValue)


floatEntryFromString : String -> FloatEntry
floatEntryFromString stringValue =
    FloatEntry stringValue (String.toFloat stringValue)


integerEntryFromAthleteNumber : String -> IntegerEntry
integerEntryFromAthleteNumber athleteNumber =
    if String.startsWith "A" athleteNumber || String.startsWith "a" athleteNumber then
        IntegerEntry athleteNumber (String.toInt (String.dropLeft 1 athleteNumber))

    else
        integerEntryFromString athleteNumber


integerEntryFromInt : Int -> IntegerEntry
integerEntryFromInt intValue =
    IntegerEntry (String.fromInt intValue) (Just intValue)


integerEntryFromMaybeInt : Maybe Int -> IntegerEntry
integerEntryFromMaybeInt maybeIntValue =
    let
        stringValue : String
        stringValue =
            Maybe.map String.fromInt maybeIntValue
                |> Maybe.withDefault ""
    in
    IntegerEntry stringValue maybeIntValue


isPositive : Entry number -> Bool
isPositive entry =
    case entry.parsedValue of
        Just someInt ->
            someInt > 0

        Nothing ->
            False


isValidEntry : Entry number -> Bool
isValidEntry entry =
    entry.enteredValue == "" || isPositive entry
