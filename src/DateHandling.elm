module DateHandling exposing (generateDownloadFilenameDatePart)

import Date exposing (Date, Month(..))


formatToAtLeastTwoChars : Int -> String
formatToAtLeastTwoChars number =
    if number < 10 then
        "0" ++ (toString number)
    else
        toString number


getMonthNumber : Date -> Int
getMonthNumber date =
    case Date.month date of
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


generateDownloadFilenameDatePart : Date -> String
generateDownloadFilenameDatePart date =
    [ Date.day date
    , getMonthNumber date
    , Date.year date
    , Date.hour date
    , Date.minute date
    , Date.second date
    ]
        |> List.map formatToAtLeastTwoChars
        |> String.join ""
