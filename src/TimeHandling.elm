module TimeHandling exposing (formatTime, formatTimeWithHours, parseTime)

import Error exposing (Error)
import Parser exposing ((|.), (|=), Parser, end, oneOf, run, succeed, symbol)
import Parsers exposing (digitsRange)


timeParser : Parser ( Int, Int, Maybe Int )
timeParser =
    succeed (\first second third -> ( first, second, third ))
        |= digitsRange 1 2
        |. symbol ":"
        |= digitsRange 1 2
        |= oneOf
            [ succeed Just
                |. symbol ":"
                |= digitsRange 1 2
            , succeed Nothing
            ]
        |. oneOf
            [ succeed ()
                |. symbol "."
                |. digitsRange 1 5
            , succeed ()
            ]
        |. end


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
    case run timeParser (String.trim timeString) of
        Ok ( hours, minutes, Just seconds ) ->
            checkTime hours minutes seconds

        Ok ( minutes, seconds, Nothing ) ->
            checkTime 0 minutes seconds

        _ ->
            Error "UNRECOGNISED_TIME" ("Time '" ++ timeString ++ "' was not recognised")
                |> Err


checkTime : Int -> Int -> Int -> Result Error Int
checkTime hours minutes seconds =
    if seconds >= 60 then
        Error "SECONDS_TOO_LARGE" ("Seconds value " ++ String.fromInt seconds ++ " is too large")
            |> Err

    else if minutes >= 60 then
        Error "MINUTES_TOO_LARGE" ("Minutes value " ++ String.fromInt minutes ++ " is too large")
            |> Err

    else
        Ok (hours * 3600 + minutes * 60 + seconds)


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
