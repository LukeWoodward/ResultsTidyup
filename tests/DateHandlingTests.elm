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
