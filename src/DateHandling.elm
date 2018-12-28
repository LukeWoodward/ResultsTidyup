module DateHandling exposing (dateStringToPosix, generateDownloadFilenameDatePart)

import Iso8601
import Regex exposing (Regex)
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


barcodeScannerDateRegex : Regex
barcodeScannerDateRegex =
    Regex.fromString "^\\d\\d/\\d\\d/\\d\\d\\d\\d \\d\\d:\\d\\d:\\d\\d$"
        |> Maybe.withDefault Regex.never


dateStringToPosix : String -> Maybe Posix
dateStringToPosix dateString =
    if Regex.contains barcodeScannerDateRegex dateString then
        let
            isoDateString : String
            isoDateString =
                String.slice 6 10 dateString
                    ++ "-"
                    ++ String.slice 3 5 dateString
                    ++ "-"
                    ++ String.left 2 dateString
                    ++ "T"
                    ++ String.right 8 dateString
                    ++ ".000Z"
        in
        case Iso8601.toTime isoDateString of
            Ok time ->
                Just time

            Err _ ->
                Nothing

    else
        Nothing
