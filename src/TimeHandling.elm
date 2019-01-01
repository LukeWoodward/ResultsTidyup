module TimeHandling exposing (formatTime, formatTimeWithHours, parseHoursAndMinutes, parseTime)

import Error exposing (Error)
import Regex exposing (Regex, fromString)


hoursAndMinutesRegex : Regex
hoursAndMinutesRegex =
    Regex.fromString "^(\\d{1,2}):(\\d{1,2})$"
        |> Maybe.withDefault Regex.never


timeRegex : Regex
timeRegex =
    Regex.fromString "^(?:(\\d+):)?(\\d{1,2}):(\\d{1,2})$"
        |> Maybe.withDefault Regex.never


{-| Parse a string containing a time in the form MM:SS or HH:MM:SS to a time,
as a number of seconds.

    parseTime "00:00" = Ok 0
    parseTime "00:00:00" = Ok 0
    parseTime "02:03" = Ok 123   -- 123 = 2 * 60 + 3
    parseTime "01:02:03" = Ok 3723   -- 3723 = 1 * 3600 + 2 * 60 + 3
    parseTime "nonsense" = Err ...

-}
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

                        hoursMaybe : Maybe Int
                        hoursMaybe =
                            String.toInt hoursStr

                        minutesMaybe : Maybe Int
                        minutesMaybe =
                            String.toInt minutesStr

                        secondsMaybe : Maybe Int
                        secondsMaybe =
                            String.toInt secondsStr
                    in
                    case ( hoursMaybe, minutesMaybe, secondsMaybe ) of
                        ( Just hours, Just minutes, Just seconds ) ->
                            if seconds >= 60 then
                                Error "SECONDS_TOO_LARGE" ("Seconds value " ++ secondsStr ++ " is too large")
                                    |> Err

                            else if minutes >= 60 then
                                Error "MINUTES_TOO_LARGE" ("Minutes value " ++ minutesStr ++ " is too large")
                                    |> Err

                            else
                                Ok (hours * 3600 + minutes * 60 + seconds)

                        _ ->
                            -- Unexpected: one of the number values failed to parse.
                            Error "INTERNAL_TIME_PARSING_FAILURE_INT" ("Unexpected failure to parse time '" ++ timeString ++ "'")
                                |> Err

                _ ->
                    -- Unexpected: not three matches from the regex.
                    Error "INTERNAL_TIME_PARSING_FAILURE_REGEX" ("Unexpected failure to parse time '" ++ timeString ++ "'")
                        |> Err

        _ ->
            Error "UNRECOGNISED_TIME" ("Time '" ++ timeString ++ "' was not recognised")
                |> Err


{-| Parse a string containing a time in the form HH:MM to a time,
as a number of minutes.

    parseHoursAndMinutes "00:00" = Ok 0
    parseHoursAndMinutes "09:30" = Ok 570   -- 570 = 9 * 60 + 30
    parseHoursAndMinutes "junk" = Err ...

-}
parseHoursAndMinutes : String -> Result Error Int
parseHoursAndMinutes timeString =
    case Regex.findAtMost 1 hoursAndMinutesRegex timeString of
        [ match ] ->
            case match.submatches of
                [ Just hoursStr, Just minutesStr ] ->
                    let
                        hoursMaybe : Maybe Int
                        hoursMaybe =
                            String.toInt hoursStr

                        minutesMaybe : Maybe Int
                        minutesMaybe =
                            String.toInt minutesStr
                    in
                    case ( hoursMaybe, minutesMaybe ) of
                        ( Just hours, Just minutes ) ->
                            if hours >= 24 then
                                Error "HOURS_TOO_LARGE" ("Hours value " ++ hoursStr ++ " is too large")
                                    |> Err

                            else if minutes >= 60 then
                                Error "MINUTES_TOO_LARGE" ("Minutes value " ++ minutesStr ++ " is too large")
                                    |> Err

                            else
                                Ok (hours * 60 + minutes)

                        _ ->
                            -- Unexpected: one of the number values failed to parse.
                            Error "INTERNAL_TIME_PARSING_FAILURE_INT" ("Unexpected failure to parse time '" ++ timeString ++ "'")
                                |> Err

                _ ->
                    -- Unexpected: not two matches from the regex.
                    Error "INTERNAL_TIME_PARSING_FAILURE_REGEX" ("Unexpected failure to parse time '" ++ timeString ++ "'")
                        |> Err

        _ ->
            Error "UNRECOGNISED_TIME" ("Time '" ++ timeString ++ "' was not recognised")
                |> Err


formatToAtLeastTwoChars : Int -> String
formatToAtLeastTwoChars number =
    if number < 10 then
        "0" ++ String.fromInt number

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
                formatToAtLeastTwoChars minutes ++ ":" ++ formatToAtLeastTwoChars seconds
        in
        if hours == 0 && not mustIncludeHours then
            minsAndSecs

        else
            formatToAtLeastTwoChars hours ++ ":" ++ minsAndSecs


formatTime : Int -> String
formatTime timeInSeconds =
    formatTimeInternal False timeInSeconds


formatTimeWithHours : Int -> String
formatTimeWithHours timeInSeconds =
    formatTimeInternal True timeInSeconds
