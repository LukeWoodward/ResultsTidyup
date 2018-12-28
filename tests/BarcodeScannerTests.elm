module BarcodeScannerTests exposing (createBarcodeScannerData, suite, toPosix)

import BarcodeScanner exposing (AthleteAndTimePair, BarcodeScannerData, PositionAndTimePair, empty, isEmpty, maxFinishToken, readBarcodeScannerData)
import Dict exposing (Dict)
import Errors exposing (expectError)
import Expect
import Iso8601
import String.Extra
import Test exposing (Test, describe, test)
import Time exposing (Posix)


dummyTime : String
dummyTime =
    "14/03/2018 09:47:53"


toPosix : String -> Maybe Posix
toPosix timeString =
    let
        parsedTime : Maybe Posix
        parsedTime =
            Iso8601.toTime timeString
                |> Result.toMaybe
    in
    case parsedTime of
        Just _ ->
            parsedTime

        Nothing ->
            let
                _ =
                    Debug.log "Warning: time did not parse as ISO-8601 date string" timeString
            in
            Nothing


createBarcodeScannerData : Dict Int (List String) -> List String -> List Int -> BarcodeScannerData
createBarcodeScannerData athleteToPositionsDict athleteBarcodesOnly finishTokensOnly =
    let
        wrapAthlete : String -> AthleteAndTimePair
        wrapAthlete athlete =
            AthleteAndTimePair athlete dummyTime

        athletesToPosition : Dict Int (List AthleteAndTimePair)
        athletesToPosition =
            Dict.map (\position athletes -> List.map wrapAthlete athletes) athleteToPositionsDict
    in
    BarcodeScannerData
        athletesToPosition
        (List.map wrapAthlete athleteBarcodesOnly)
        (List.map (\position -> PositionAndTimePair position dummyTime) finishTokensOnly)
        Nothing


suite : Test
suite =
    describe "BarcodeScanner tests"
        [ describe "emptiness tests"
            [ test "Empty data is empty" <|
                \() ->
                    isEmpty empty
                        |> Expect.true "Empty data is empty"
            , test "Data with non-empty barcodes scanned should not be empty" <|
                \() ->
                    isEmpty (createBarcodeScannerData (Dict.singleton 47 [ "A4580442" ]) [] [])
                        |> Expect.false "Data with non-empty barcodes scanned should not be empty"
            , test "Data with non-empty athletes-only should not be empty" <|
                \() ->
                    isEmpty (createBarcodeScannerData Dict.empty [ "A123456" ] [])
                        |> Expect.false "Data with non-empty athletes-only should not be empty"
            , test "Data with non-empty finish-tokens-only should not be empty" <|
                \() ->
                    isEmpty (createBarcodeScannerData Dict.empty [] [ 47 ])
                        |> Expect.false "Data with non-empty finish-tokens  should not be empty"
            ]
        , describe "readBarcodeScannerData tests"
            [ test "readBarcodeScannerData of a valid single-line string with athlete and finish token is valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,P0047,14/03/2018 09:47:03"
                        |> Expect.equal (Ok (BarcodeScannerData (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ]) [] [] (toPosix "2018-03-14T09:47:03.000Z")))
            , test "readBarcodeScannerData of a valid single-line string with athlete only is valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,,14/03/2018 09:47:03"
                        |> Expect.equal (Ok (BarcodeScannerData Dict.empty [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ] [] (toPosix "2018-03-14T09:47:03.000Z")))
            , test "readBarcodeScannerData of a valid single-line string with finish token only is valid" <|
                \() ->
                    readBarcodeScannerData ",P0047,14/03/2018 09:47:03"
                        |> Expect.equal (Ok (BarcodeScannerData Dict.empty [] [ PositionAndTimePair 47 "14/03/2018 09:47:03" ] (toPosix "2018-03-14T09:47:03.000Z")))
            , test "readBarcodeScannerData of a valid single-line string with athlete and finish token and blank lines is valid" <|
                \() ->
                    readBarcodeScannerData "\n\n\n\n\nA4580442,P0047,14/03/2018 09:47:03\n\n\n\n"
                        |> Expect.equal (Ok (BarcodeScannerData (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ]) [] [] (toPosix "2018-03-14T09:47:03.000Z")))
            , test "readBarcodeScannerData of a valid multiline string with two different finish tokens is valid" <|
                \() ->
                    readBarcodeScannerData "A4580442,P0047,14/03/2018 09:47:03\nA1866207,P0047,14/03/2018 09:48:44"
                        |> Expect.equal
                            (Ok
                                (BarcodeScannerData
                                    (Dict.singleton 47
                                        [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03"
                                        , AthleteAndTimePair "A1866207" "14/03/2018 09:48:44"
                                        ]
                                    )
                                    []
                                    []
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                )
                            )
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
        , describe "maxFinishToken tests"
            [ test "maxFinishToken of an empty set of barcodes returns Nothing" <|
                \() ->
                    maxFinishToken empty
                        |> Expect.equal Nothing
            , test "maxFinishToken of a single scanned barcode returns the finish position" <|
                \() ->
                    maxFinishToken (createBarcodeScannerData (Dict.singleton 47 [ "A4580442" ]) [] [])
                        |> Expect.equal (Just 47)
            , test "maxFinishToken of three scanned barcodse returns the maximum finish position" <|
                \() ->
                    maxFinishToken (createBarcodeScannerData (Dict.fromList [ ( 47, [ "A4580442" ] ), ( 59, [ "A456321" ] ), ( 33, [ "A464631" ] ) ]) [] [])
                        |> Expect.equal (Just 59)
            ]
        ]
