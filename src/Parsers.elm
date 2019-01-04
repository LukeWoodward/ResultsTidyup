module Parsers exposing (digits, digitsRange)

import Parser exposing (Parser, andThen, chompWhile, getChompedString, problem, succeed)


checkDigits : Int -> String -> Parser ()
checkDigits expectedLength string =
    if String.length string == expectedLength then
        succeed ()

    else
        problem ("Expected " ++ String.fromInt expectedLength ++ " digits, found " ++ String.fromInt (String.length string))


checkDigitsRange : Int -> Int -> String -> Parser Int
checkDigitsRange minLength maxLength string =
    let
        actualLength : Int
        actualLength =
            String.length string
    in
    if minLength <= actualLength && actualLength <= maxLength then
        String.toInt string
            |> Maybe.map succeed
            |> Maybe.withDefault (problem ("Invalid integer: '" ++ string ++ "'"))

    else
        problem ("Expected from " ++ String.fromInt minLength ++ " to " ++ String.fromInt maxLength ++ " digits, found " ++ String.fromInt actualLength)


digits : Int -> Parser ()
digits numDigits =
    getChompedString (chompWhile Char.isDigit)
        |> andThen (checkDigits numDigits)


digitsRange : Int -> Int -> Parser Int
digitsRange minDigits maxDigits =
    getChompedString (chompWhile Char.isDigit)
        |> andThen (checkDigitsRange minDigits maxDigits)
