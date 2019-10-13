module DateHandling exposing (dateTimeStringToPosix, generateDownloadFilenameDatePart, posixToDateString, posixToDateTimeString)

import Iso8601
import Parser exposing ((|.), (|=), Parser, end, run, symbol)
import Parsers exposing (digits)
import Result.Extra
import Time exposing (Month(..), Posix, Zone)


formatToAtLeastTwoChars : Int -> String
formatToAtLeastTwoChars number =
    if number < 10 then
        "0" ++ String.fromInt number

    else
        String.fromInt number


getMonthNumber : Zone -> Posix -> Int
getMonthNumber zone time =
    case Time.toMonth zone time of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


generateDownloadFilenameDatePart : Zone -> Posix -> String
generateDownloadFilenameDatePart zone time =
    [ Time.toDay zone time
    , getMonthNumber zone time
    , Time.toYear zone time
    , Time.toHour zone time
    , Time.toMinute zone time
    , Time.toSecond zone time
    ]
        |> List.map formatToAtLeastTwoChars
        |> String.join ""


barcodeScannerDateTimeParser : Parser ()
barcodeScannerDateTimeParser =
    digits 2
        |. symbol "/"
        |. digits 2
        |. symbol "/"
        |. digits 4
        |. symbol " "
        |. digits 2
        |. symbol ":"
        |. digits 2
        |. symbol ":"
        |. digits 2
        |. end


dateTimeStringToPosix : String -> Maybe Posix
dateTimeStringToPosix dateTimeString =
    if Result.Extra.isOk (run barcodeScannerDateTimeParser dateTimeString) then
        let
            isoDateTimeString : String
            isoDateTimeString =
                String.slice 6 10 dateTimeString
                    ++ "-"
                    ++ String.slice 3 5 dateTimeString
                    ++ "-"
                    ++ String.left 2 dateTimeString
                    ++ "T"
                    ++ String.right 8 dateTimeString
                    ++ ".000Z"
        in
        case Iso8601.toTime isoDateTimeString of
            Ok time ->
                Just time

            Err _ ->
                Nothing

    else
        Nothing


posixToDateString : Posix -> String
posixToDateString date =
    [ Time.toDay Time.utc date
    , getMonthNumber Time.utc date
    , Time.toYear Time.utc date
    ]
        |> List.map formatToAtLeastTwoChars
        |> String.join "/"


posixToDateTimeString : Posix -> String
posixToDateTimeString dateTime =
    let
        timeString : String
        timeString =
            [ Time.toHour Time.utc dateTime
            , Time.toMinute Time.utc dateTime
            , Time.toSecond Time.utc dateTime
            ]
                |> List.map formatToAtLeastTwoChars
                |> String.join ":"
    in
    posixToDateString dateTime ++ " " ++ timeString
