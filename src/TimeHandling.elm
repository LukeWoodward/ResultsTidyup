module TimeHandling exposing (parseTime, formatTime, formatTimeWithHours)

import Regex exposing (Regex, regex, HowMany(..))
import Error exposing (Error)


timeRegex : Regex
timeRegex =
    regex "^(?:(\\d+):)?(\\d{1,2}):(\\d{1,2})$"


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


formatToAtLeastTwoChars : Int -> String
formatToAtLeastTwoChars number =
    if number < 10 then
        "0" ++ (toString number)
    else
        toString number


formatTimeInternal : Bool -> Int -> String
formatTimeInternal mustIncludeHours timeInSeconds =
    if timeInSeconds < 0 then
        "-" ++ formatTime -timeInSeconds
    else
        let
            seconds : Int
            seconds =
                timeInSeconds % 60

            minutes : Int
            minutes =
                (timeInSeconds // 60) % 60

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
