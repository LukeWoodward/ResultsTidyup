module BarcodeScannerTests exposing (suite)

import BarcodeScanner exposing (BarcodeScannerData, readBarcodeScannerData)
import Dict exposing (Dict)
import Errors exposing (expectError)
import Expect
import String.Extra
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "BarcodeScanner tests"
        [ describe "readBarcodeScannerData tests"
            [ test "readBarcodeScannerData of a valid single-line string with athlete and finish token is valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,P0047,14/03/2018 09:47:03"
                        |> Expect.equal (Ok (BarcodeScannerData (Dict.singleton 47 [ "A4580442" ]) [] []))
            , test "readBarcodeScannerData of a valid single-line string with athlete only is valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,,14/03/2018 09:47:03"
                        |> Expect.equal (Ok (BarcodeScannerData Dict.empty [ "A4580442" ] []))
            , test "readBarcodeScannerData of a valid single-line string with finish token only is valid" <|
                \() ->
                    readBarcodeScannerData ",P0047,14/03/2018 09:47:03"
                        |> Expect.equal (Ok (BarcodeScannerData Dict.empty [] [ 47 ]))
            , test "readBarcodeScannerData of a valid single-line string with athlete and finish token is valid and blank lines is valid" <|
                \() ->
                    readBarcodeScannerData "\n\n\n\n\nA4580442,P0047,14/03/2018 09:47:03\n\n\n\n"
                        |> Expect.equal (Ok (BarcodeScannerData (Dict.singleton 47 [ "A4580442" ]) [] []))
            , test "readBarcodeScannerData of a valid multiline string with two different finish tokens is valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,P0047,14/03/2018 09:47:03\nA1866207,P0047,14/03/2018 09:48:44"
                        |> Expect.equal (Ok (BarcodeScannerData (Dict.singleton 47 [ "A4580442", "A1866207" ]) [] []))
            , test "readBarcodeScannerData of an empty string is not valid" <|
                \() ->
                    readBarcodeScannerData ""
                        |> expectError "NO_RESULTS"
            , test "readBarcodeScannerData of a string with an invalid athlete barcode is not valid" <|
                \() ->
                    readBarcodeScannerData "Nonsense,P0047,14/03/2018 09:47:03"
                        |> expectError "INVALID_ATHLETE_RECORD"
            , test "readBarcodeScannerData of a string with an invalid finish token barcode is not valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,Nonsense,14/03/2018 09:47:03"
                        |> expectError "INVALID_POSITION_RECORD"
            , test "readBarcodeScannerData of a string with no athlete nor finish token barcode is not valid" <|
                \() ->
                    readBarcodeScannerData ",,14/03/2018 09:47:03"
                        |> expectError "ATHLETE_AND_FINISH_TOKEN_MISSING"
            , test "readBarcodeScannerData of a string with too few parts is not valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,P0047"
                        |> expectError "NOT_THREE_PARTS"
            , test "readBarcodeScannerData of a string with too many parts is not valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,P0047,14/03/2018 09:47:03,someOtherRubbish"
                        |> expectError "NOT_THREE_PARTS"
            , test "readBarcodeScannerData of a string with position zero is not valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,P0000,14/03/2018 09:47:03"
                        |> expectError "INVALID_POSITION_ZERO"
            ]
        ]
