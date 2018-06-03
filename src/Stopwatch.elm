module Stopwatch exposing (Stopwatch(..), readStopwatchData)

import Error exposing (Error)
import Regex exposing (regex, Regex, HowMany(AtMost, All))
import Result.Extra
import TimeHandling exposing (parseTime)


{- Stopwatch data is basically a list of integer numbers of seconds -}


type Stopwatch
    = StopwatchData (List Int)


binaryRegex : Regex
binaryRegex =
    regex "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]"


isPossibleBinary : String -> Bool
isPossibleBinary fileText =
    Regex.contains binaryRegex fileText


lineSplitRegex : Regex
lineSplitRegex =
    regex "[\\r\\n]+"


splitLines : String -> List String
splitLines text =
    Regex.split All lineSplitRegex text


ignorableLinePrefixes : List String
ignorableLinePrefixes =
    [ "STARTOFEVENT"
    , "0,"
    , "ENDOFEVENT"
    ]


filterIgnorableLines : String -> Bool
filterIgnorableLines line =
    List.any (\prefix -> String.startsWith prefix line) ignorableLinePrefixes
        |> not


readLine : String -> Result Error Int
readLine line =
    let
        parts : List String
        parts =
            String.split "," line
    in
        case parts of
            [ _, _, time ] ->
                parseTime time

            _ ->
                Error "NOT_THREE_PARTS" ("Line " ++ line ++ " does not contain the expected three comma-separated parts")
                    |> Err


failIfNoResults : List Int -> Result Error (List Int)
failIfNoResults results =
    if List.isEmpty results then
        Error "NO_RESULTS" "Stopwatch data contained no results"
            |> Err
    else
        Ok results


readStopwatchData : String -> Result Error Stopwatch
readStopwatchData text =
    if isPossibleBinary text then
        Error "BINARY_FILE" "File appears to be a binary file"
            |> Err
    else
        text
            |> splitLines
            |> List.filter (not << String.isEmpty)
            |> List.filter filterIgnorableLines
            |> List.map readLine
            |> Result.Extra.combine
            |> Result.andThen failIfNoResults
            |> Result.map StopwatchData
