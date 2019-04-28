module FileHandling exposing (InteropFile, crlf, isPossibleBinary, splitLines)

import Regex exposing (Regex)


type alias InteropFile =
    { fileName : String
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
