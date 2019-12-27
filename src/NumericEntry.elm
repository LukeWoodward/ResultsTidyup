module NumericEntry exposing
    ( IntegerEntry
    , emptyIntegerEntry
    , integerEntryFromAthleteNumber
    , integerEntryFromInt
    , integerEntryFromMaybeInt
    , integerEntryFromString
    , isValidEntry
    )


type alias IntegerEntry =
    { enteredValue : String
    , parsedValue : Maybe Int
    }


emptyIntegerEntry : IntegerEntry
emptyIntegerEntry =
    IntegerEntry "" Nothing


integerEntryFromString : String -> IntegerEntry
integerEntryFromString stringValue =
    IntegerEntry stringValue (String.toInt stringValue)


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


isPositive : IntegerEntry -> Bool
isPositive entry =
    case entry.parsedValue of
        Just someInt ->
            someInt > 0

        Nothing ->
            False


isValidEntry : IntegerEntry -> Bool
isValidEntry entry =
    entry.enteredValue == "" || isPositive entry
