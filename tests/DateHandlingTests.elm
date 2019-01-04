module DateHandlingTests exposing (suite)

import DateHandling exposing (dateStringToPosix, dateToString, generateDownloadFilenameDatePart)
import Errors exposing (expectError)
import Expect
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "DateHandling tests"
        [ describe "generateDownloadFilenameDatePart tests"
            [ test "Can generate a download filename part" <|
                \() ->
                    generateDownloadFilenameDatePart Time.utc (Time.millisToPosix 1500000000000)
                        |> Expect.equal "14072017024000"
            ]
        , describe "dateStringToPosix tests"
            [ test "Can parse an actual time into a Posix value" <|
                \() ->
                    dateStringToPosix "14/07/2017 02:40:00"
                        |> Maybe.map Time.posixToMillis
                        |> Expect.equal (Just 1500000000000)
            , test "Can parse an invalid time into Nothing" <|
                \() ->
                    dateStringToPosix "29/02/2017 09:57:22"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-short day value into Nothing" <|
                \() ->
                    dateStringToPosix "9/02/2017 02:24:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-long day value into Nothing" <|
                \() ->
                    dateStringToPosix "109/02/2017 02:24:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-short month value into Nothing" <|
                \() ->
                    dateStringToPosix "19/2/2017 02:24:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-long month value into Nothing" <|
                \() ->
                    dateStringToPosix "19/102/2017 02:24:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-short year value into Nothing" <|
                \() ->
                    dateStringToPosix "19/02/207 02:24:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-long year value into Nothing" <|
                \() ->
                    dateStringToPosix "19/02/21017 02:24:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-short hour value into Nothing" <|
                \() ->
                    dateStringToPosix "19/02/2017 2:24:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-long hour value into Nothing" <|
                \() ->
                    dateStringToPosix "19/02/2017 302:24:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-short minute value into Nothing" <|
                \() ->
                    dateStringToPosix "19/02/2017 02:4:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-long minute value into Nothing" <|
                \() ->
                    dateStringToPosix "19/02/2017 02:124:00"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-short second value into Nothing" <|
                \() ->
                    dateStringToPosix "19/02/2017 02:04:0"
                        |> Expect.equal Nothing
            , test "Can parse a string with too-long second value into Nothing" <|
                \() ->
                    dateStringToPosix "19/02/2017 02:24:200"
                        |> Expect.equal Nothing
            , test "Can parse a nonsense string into Nothing" <|
                \() ->
                    dateStringToPosix "This is not a valid date-time"
                        |> Expect.equal Nothing
            ]
        , describe "dateToString tests"
            [ test "Can convert a date into a string" <|
                \() ->
                    dateToString (Time.millisToPosix 1500000000000)
                        |> Expect.equal "14/07/2017"
            ]
        ]
