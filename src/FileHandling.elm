module FileHandling exposing (AddedFile, InteropFile, crlf, deduceNameFromFilename, isPossibleBinary, splitLines)

import Regex exposing (Match, Regex)


type alias InteropFile =
    { fileName : String
    , fileText : String
    }


type alias AddedFile =
    { fileName : String
    , name : String
    , fileText : String
    }


crlf : String
crlf =
    "\u{000D}\n"


binaryRegex : Regex
binaryRegex =
    Regex.fromString "[\u{0000}-\u{0008}\u{000B}\u{000C}\u{000E}-\u{001F}\u{007F}]"
        |> Maybe.withDefault Regex.never


isPossibleBinary : String -> Bool
isPossibleBinary fileText =
    Regex.contains binaryRegex fileText


lineSplitRegex : Regex
lineSplitRegex =
    Regex.fromString "[\\r\\n]+"
        |> Maybe.withDefault Regex.never


splitLines : String -> List String
splitLines text =
    Regex.split lineSplitRegex text


deduceNameFromFilename : String -> String
deduceNameFromFilename fileName =
    let
        addSpace : Match -> String
        addSpace match =
            case match.submatches of
                [ Just _, Just _ ] ->
                    String.left 1 match.match ++ " " ++ String.dropLeft 1 match.match

                _ ->
                    fileName

        replaceTrailingNumberIfNotEntireString : String -> String
        replaceTrailingNumberIfNotEntireString value =
            let
                reversedChars : List Char
                reversedChars =
                    String.toList value |> List.reverse

                trimLeadingDigits : List Char -> List Char
                trimLeadingDigits chars =
                    case chars of
                        [] ->
                            []

                        char :: rest ->
                            if char == '_' || Char.isDigit char then
                                trimLeadingDigits rest

                            else
                                chars

                trimmedLeadingDigits : List Char
                trimmedLeadingDigits =
                    trimLeadingDigits reversedChars
            in
            if List.isEmpty trimmedLeadingDigits then
                value

            else
                List.reverse trimmedLeadingDigits
                    |> String.fromList

        lowerThenUpperLetterRegex : Regex
        lowerThenUpperLetterRegex =
            Regex.fromString "([a-z])([A-Z])"
                |> Maybe.withDefault Regex.never
    in
    fileName
        |> String.replace "vv_Stopwatch_" ""
        |> String.replace "vv_Scanner_" ""
        |> String.replace "parkrun_timer_" ""
        |> String.replace "parkrun_barcode_" ""
        |> String.replace "junsd_stopwatch" ""
        |> String.replace ".txt" ""
        |> String.replace ".csv" ""
        |> Regex.replace lowerThenUpperLetterRegex addSpace
        |> replaceTrailingNumberIfNotEntireString
        |> String.replace "_" " "
        |> String.trim
