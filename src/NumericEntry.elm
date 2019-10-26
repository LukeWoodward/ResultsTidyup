module NumericEntry exposing
    ( NumericEntry
    , emptyNumericEntry
    , isValidEntry
    , numericEntryFromAthleteNumber
    , numericEntryFromInt
    , numericEntryFromMaybeInt
    , numericEntryFromString
    )


type alias NumericEntry =
    { enteredValue : String
    , parsedValue : Maybe Int
    }


emptyNumericEntry : NumericEntry
emptyNumericEntry =
    NumericEntry "" Nothing


numericEntryFromString : String -> NumericEntry
numericEntryFromString stringValue =
    NumericEntry stringValue (String.toInt stringValue)


numericEntryFromAthleteNumber : String -> NumericEntry
numericEntryFromAthleteNumber athleteNumber =
    if String.startsWith "A" athleteNumber || String.startsWith "a" athleteNumber then
        NumericEntry athleteNumber (String.toInt (String.dropLeft 1 athleteNumber))

    else
        numericEntryFromString athleteNumber


numericEntryFromInt : Int -> NumericEntry
numericEntryFromInt intValue =
    NumericEntry (String.fromInt intValue) (Just intValue)


numericEntryFromMaybeInt : Maybe Int -> NumericEntry
numericEntryFromMaybeInt maybeIntValue =
    let
        stringValue : String
        stringValue =
            Maybe.map String.fromInt maybeIntValue
                |> Maybe.withDefault ""
    in
    NumericEntry stringValue maybeIntValue


isPositive : NumericEntry -> Bool
isPositive entry =
    case entry.parsedValue of
        Just someInt ->
            someInt > 0

        Nothing ->
            False


isValidEntry : NumericEntry -> Bool
isValidEntry entry =
    entry.enteredValue == "" || isPositive entry
