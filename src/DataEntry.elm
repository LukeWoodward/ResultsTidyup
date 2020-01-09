module DataEntry exposing
    ( DateEntry
    , Entry
    , FloatEntry
    , IntegerEntry
    , Range
    , RangeEntry
    , emptyEntry
    , floatEntryFromFloat
    , floatEntryFromString
    , integerEntryFromAthleteNumber
    , integerEntryFromHoursAndMinutes
    , integerEntryFromInt
    , integerEntryFromMaybeInt
    , integerEntryFromString
    , integerEntryFromTime
    , isPositive
    , isValidEntry
    , rangeEntryFromString
    , rangeToString
    )

import Time exposing (Posix)
import TimeHandling exposing (parseHoursAndMinutes, parseTime)


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


type alias Range =
    { start : Int
    , end : Int
    }


type alias RangeEntry =
    { enteredValue : String
    , parsedValue : Maybe Range
    }


emptyEntry : Entry a
emptyEntry =
    Entry "" Nothing


integerEntryFromString : String -> IntegerEntry
integerEntryFromString stringValue =
    IntegerEntry stringValue (String.toInt stringValue)


requireFiniteFloat : Float -> Maybe Float
requireFiniteFloat floatValue =
    -- Strictly speaking the isNaN check in the line below is redundant if
    -- passed a value from String.toFloat: at the time of writing,
    -- String.toFloat "NaN" returns Nothing rather than Just NaN.  However,
    -- we'll check this case anyway in case things change in future.
    if isNaN floatValue || isInfinite floatValue then
        Nothing

    else
        Just floatValue


floatEntryFromString : String -> FloatEntry
floatEntryFromString stringValue =
    String.toFloat stringValue
        |> Maybe.andThen requireFiniteFloat
        |> FloatEntry stringValue


rangeEntryFromString : String -> RangeEntry
rangeEntryFromString rangeString =
    RangeEntry rangeString (parseRange rangeString)


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


integerEntryFromHoursAndMinutes : String -> IntegerEntry
integerEntryFromHoursAndMinutes hoursAndMinutes =
    parseHoursAndMinutes hoursAndMinutes
        |> Result.toMaybe
        |> IntegerEntry hoursAndMinutes


integerEntryFromTime : String -> IntegerEntry
integerEntryFromTime time =
    parseTime time
        |> Result.toMaybe
        |> IntegerEntry time


floatEntryFromFloat : Float -> FloatEntry
floatEntryFromFloat floatValue =
    FloatEntry (String.fromFloat floatValue) (Just floatValue)


trimToInt : String -> Maybe Int
trimToInt string =
    String.toInt (String.trim string)


parseRange : String -> Maybe Range
parseRange text =
    case String.split "-" text of
        [ singleString ] ->
            trimToInt singleString
                |> Maybe.map (\num -> Range num num)

        [ firstString, secondString ] ->
            Maybe.map2 Range (trimToInt firstString) (trimToInt secondString)

        _ ->
            Nothing


rangeToString : Range -> String
rangeToString range =
    if range.start == range.end then
        String.fromInt range.start

    else
        String.fromInt range.start ++ "-" ++ String.fromInt range.end


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
