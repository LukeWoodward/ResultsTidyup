module DateHandling exposing (generateDownloadFilenameDatePart)

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
