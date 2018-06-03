module Stopwatch exposing (Stopwatch(..), readStopwatchData)

import Error exposing (Error)
import Regex exposing (regex, Regex, HowMany(AtMost, All))
import Result.Extra


{- Stopwatch data is basically a list of integer numbers of seconds -}


type Stopwatch
    = StopwatchData (List Int)


lineSplitRegex : Regex
lineSplitRegex =
    regex "[\\r\\n]+"


timeRegex : Regex
timeRegex =
    regex "^(?:(\\d+):)?(\\d{1,2}):(\\d{1,2})$"


splitLines : String -> List String
splitLines text =
    Regex.split All lineSplitRegex text


ignorableLinePrefixes : List String
ignorableLinePrefixes =
    [ "STARTOFEVENT"
    , "0,"
    , "ENDOFEVENT"
    ]


parseTime : String -> Result Error Int
parseTime timeString =
    case Regex.find (AtMost 1) timeRegex timeString of
        [ match ] ->
            case match.submatches of
                [ hoursMatch, Just minutesStr, Just secondsStr ] ->
                    let
                        hoursStr : String
                        hoursStr =
                            Maybe.withDefault "0" hoursMatch

                        hoursResult : Result String Int
                        hoursResult =
                            String.toInt hoursStr

                        minutesResult : Result String Int
                        minutesResult =
                            String.toInt minutesStr

                        secondsResult : Result String Int
                        secondsResult =
                            String.toInt secondsStr
                    in
                        case ( hoursResult, minutesResult, secondsResult ) of
                            ( Ok hours, Ok minutes, Ok seconds ) ->
                                if seconds >= 60 then
                                    Error "SECONDS_TOO_LARGE" ("Seconds value " ++ secondsStr ++ " is too large")
                                        |> Err
                                else if minutes >= 60 then
                                    Error "MINUTES_TOO_LARGE" ("Minutes value " ++ secondsStr ++ " is too large")
                                        |> Err
                                else
                                    Ok (hours * 3600 + minutes * 60 + seconds)

                            _ ->
                                -- Unexpected: one of the number values failed to parseTime
                                Error "INTERNAL_TIME_PARSING_FAILURE_INT" ("Unexpected failure to parse time '" ++ timeString ++ "'")
                                    |> Err

                _ ->
                    -- Unexpected: not three matches from the regex.
                    Error "INTERNAL_TIME_PARSING_FAILURE_REGEX" ("Unexpected failure to parse time '" ++ timeString ++ "'")
                        |> Err

        _ ->
            Error "UNRECOGNISED_TIME" ("Time '" ++ timeString ++ "' was not recognised")
                |> Err


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
    text
        |> splitLines
        |> List.filter (not << String.isEmpty)
        |> List.filter filterIgnorableLines
        |> List.map readLine
        |> Result.Extra.combine
        |> Result.andThen failIfNoResults
        |> Result.map StopwatchData
