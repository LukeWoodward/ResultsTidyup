module TimeHandling exposing (parseTime, formatTime, formatTimeWithHours)

import Regex exposing (Regex, fromString)
import Error exposing (Error)


timeRegex : Regex
timeRegex =
    Regex.fromString "^(?:(\\d+):)?(\\d{1,2}):(\\d{1,2})$"
        |> Maybe.withDefault Regex.never


parseTime : String -> Result Error Int
parseTime timeString =
    case Regex.findAtMost 1 timeRegex timeString of
        [ match ] ->
            case match.submatches of
                [ hoursMatch, Just minutesStr, Just secondsStr ] ->
                    let
                        hoursStr : String
                        hoursStr =
                            Maybe.withDefault "0" hoursMatch

                        hoursResult : Maybe Int
                        hoursResult =
                            String.toInt hoursStr

                        minutesResult : Maybe Int
                        minutesResult =
                            String.toInt minutesStr

                        secondsResult : Maybe Int
                        secondsResult =
                            String.toInt secondsStr
                    in
                        case ( hoursResult, minutesResult, secondsResult ) of
                            ( Just hours, Just minutes, Just seconds ) ->
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


formatToAtLeastTwoChars : Int -> String
formatToAtLeastTwoChars number =
    if number < 10 then
        "0" ++ (String.fromInt number)
    else
        String.fromInt number


formatTimeInternal : Bool -> Int -> String
formatTimeInternal mustIncludeHours timeInSeconds =
    if timeInSeconds < 0 then
        "-" ++ formatTime -timeInSeconds
    else
        let
            seconds : Int
            seconds =
                modBy 60 timeInSeconds

            minutes : Int
            minutes =
                modBy 60 (timeInSeconds // 60)

            hours : Int
            hours =
                timeInSeconds // 3600

            minsAndSecs : String
            minsAndSecs =
                (formatToAtLeastTwoChars minutes) ++ ":" ++ (formatToAtLeastTwoChars seconds)
        in
            if hours == 0 && not mustIncludeHours then
                minsAndSecs
            else
                (formatToAtLeastTwoChars hours) ++ ":" ++ minsAndSecs


formatTime : Int -> String
formatTime timeInSeconds =
    formatTimeInternal False timeInSeconds


formatTimeWithHours : Int -> String
formatTimeWithHours timeInSeconds =
    formatTimeInternal True timeInSeconds
