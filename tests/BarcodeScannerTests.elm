module BarcodeScannerTests exposing (createBarcodeScannerData, expectSingleUnrecognisedLine, suite, toPosix)

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , LineContents(..)
        , MisScannedItem
        , ModificationStatus(..)
        , PositionAndTimePair
        , UnrecognisedLine
        , empty
        , generateDownloadText
        , isEmpty
        , maxFinishToken
        , readBarcodeScannerData
        )
import Dict exposing (Dict)
import Error exposing (Error)
import Errors exposing (expectError)
import Expect exposing (Expectation)
import FileHandling exposing (crlf)
import Iso8601
import Test exposing (Test, describe, test)
import Time exposing (Posix)


dummyTime : String
dummyTime =
    "14/03/2018 09:47:03"


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
        []
        athletesToPosition
        (List.map wrapAthlete athleteBarcodesOnly)
        (List.map (\position -> PositionAndTimePair position dummyTime) finishTokensOnly)
        []
        []
        Nothing


expectSingleUnrecognisedLine : String -> String -> Result Error BarcodeScannerData -> Expectation
expectSingleUnrecognisedLine expectedLine expectedCode barcodeScannerDataResult =
    case barcodeScannerDataResult of
        Ok barcodeScannerData ->
            case barcodeScannerData.unrecognisedLines of
                [] ->
                    Expect.fail "No unrecognised lines"

                [ unrecognisedLine ] ->
                    if Dict.isEmpty barcodeScannerData.scannedBarcodes then
                        Expect.all
                            [ \ul -> Expect.equal expectedLine ul.line
                            , \ul -> Expect.equal expectedCode ul.errorCode
                            ]
                            unrecognisedLine

                    else
                        Expect.fail "Unexpected valid lines found"

                first :: second :: rest ->
                    Expect.fail "More than one unrecognised line was found"

        Err error ->
            Expect.fail ("Unexpectedly failed with error '" ++ Debug.toString error ++ "'")


expectSingleUnrecognisedLineFor : String -> String -> Expectation
expectSingleUnrecognisedLineFor line expectedCode =
    readBarcodeScannerData "barcodes.txt" line
        |> expectSingleUnrecognisedLine line expectedCode


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
                    readBarcodeScannerData "barcodes1.txt" "A4580442,P0047,14/03/2018 09:47:03"
                        |> Expect.equal
                            (Ok
                                (BarcodeScannerData
                                    [ BarcodeScannerFile "barcodes1.txt"
                                        [ BarcodeScannerFileLine 1 (Ordinary "A4580442" (Just 47)) "14/03/2018 09:47:03" Unmodified ]
                                    ]
                                    (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ])
                                    []
                                    []
                                    []
                                    []
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                )
                            )
            , test "readBarcodeScannerData of a valid single-line string with athlete only is valid" <|
                \() ->
                    readBarcodeScannerData "barcodes2.txt" "A4580442,,14/03/2018 09:47:03"
                        |> Expect.equal
                            (Ok
                                (BarcodeScannerData
                                    [ BarcodeScannerFile "barcodes2.txt"
                                        [ BarcodeScannerFileLine 1 (Ordinary "A4580442" Nothing) "14/03/2018 09:47:03" Unmodified ]
                                    ]
                                    Dict.empty
                                    [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ]
                                    []
                                    []
                                    []
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                )
                            )
            , test "readBarcodeScannerData of a valid single-line string with finish token only is valid" <|
                \() ->
                    readBarcodeScannerData "barcodes3.txt" ",P0047,14/03/2018 09:47:03"
                        |> Expect.equal
                            (Ok
                                (BarcodeScannerData
                                    [ BarcodeScannerFile "barcodes3.txt"
                                        [ BarcodeScannerFileLine 1 (Ordinary "" (Just 47)) "14/03/2018 09:47:03" Unmodified ]
                                    ]
                                    Dict.empty
                                    []
                                    [ PositionAndTimePair 47 "14/03/2018 09:47:03" ]
                                    []
                                    []
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                )
                            )
            , test "readBarcodeScannerData of a valid single-line string with mis-scanned item only is valid" <|
                \() ->
                    readBarcodeScannerData "barcodes4.txt" "&d084,14/03/2018 09:47:03"
                        |> Expect.equal
                            (Ok
                                (BarcodeScannerData
                                    [ BarcodeScannerFile "barcodes4.txt"
                                        [ BarcodeScannerFileLine 1 (MisScan "&d084") "14/03/2018 09:47:03" Unmodified ]
                                    ]
                                    Dict.empty
                                    []
                                    []
                                    [ MisScannedItem "&d084" "14/03/2018 09:47:03" ]
                                    []
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                )
                            )
            , test "readBarcodeScannerData of a valid single-line string with athlete and finish token and blank lines is valid" <|
                \() ->
                    readBarcodeScannerData "barcodes5.txt" "\n\n\n\n\nA4580442,P0047,14/03/2018 09:47:03\n\n\n\n"
                        |> Expect.equal
                            (Ok
                                (BarcodeScannerData
                                    [ BarcodeScannerFile "barcodes5.txt"
                                        [ BarcodeScannerFileLine 1 (Ordinary "A4580442" (Just 47)) "14/03/2018 09:47:03" Unmodified ]
                                    ]
                                    (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ])
                                    []
                                    []
                                    []
                                    []
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                )
                            )
            , test "readBarcodeScannerData of a valid multiline string with two different finish tokens is valid" <|
                \() ->
                    readBarcodeScannerData "barcodes6.txt" "A4580442,P0047,14/03/2018 09:47:03\nA1866207,P0047,14/03/2018 09:48:44"
                        |> Expect.equal
                            (Ok
                                (BarcodeScannerData
                                    [ BarcodeScannerFile "barcodes6.txt"
                                        [ BarcodeScannerFileLine 1 (Ordinary "A4580442" (Just 47)) "14/03/2018 09:47:03" Unmodified
                                        , BarcodeScannerFileLine 2 (Ordinary "A1866207" (Just 47)) "14/03/2018 09:48:44" Unmodified
                                        ]
                                    ]
                                    (Dict.singleton 47
                                        [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03"
                                        , AthleteAndTimePair "A1866207" "14/03/2018 09:48:44"
                                        ]
                                    )
                                    []
                                    []
                                    []
                                    []
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                )
                            )
            , test "readBarcodeScannerData of an empty string is not valid" <|
                \() ->
                    readBarcodeScannerData "empty.txt" ""
                        |> expectError "NO_RESULTS"
            , test "readBarcodeScannerData of a string with an invalid athlete barcode is not valid" <|
                \() ->
                    expectSingleUnrecognisedLineFor "Nonsense,P0047,14/03/2018 09:47:03" "INVALID_ATHLETE_RECORD"
            , test "readBarcodeScannerData of a string with an invalid finish token barcode is not valid" <|
                \() ->
                    expectSingleUnrecognisedLineFor "A4580442,Nonsense,14/03/2018 09:47:03" "INVALID_POSITION_RECORD"
            , test "readBarcodeScannerData of a string with no athlete nor finish token barcode is not valid" <|
                \() ->
                    expectSingleUnrecognisedLineFor ",,14/03/2018 09:47:03" "ATHLETE_AND_FINISH_TOKEN_MISSING"
            , test "readBarcodeScannerData of a string with one comma-separated item is not valid" <|
                \() ->
                    expectSingleUnrecognisedLineFor "This is not valid" "NOT_TWO_OR_THREE_PARTS"
            , test "readBarcodeScannerData of a string with too many parts is not valid" <|
                \() ->
                    expectSingleUnrecognisedLineFor "A4580442,P0047,14/03/2018 09:47:03,someOtherRubbish" "NOT_TWO_OR_THREE_PARTS"
            , test "readBarcodeScannerData of a string with position zero is not valid" <|
                \() ->
                    expectSingleUnrecognisedLineFor "A4580442,P0000,14/03/2018 09:47:03" "INVALID_POSITION_ZERO"
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
            , test "maxFinishToken of three scanned barcodes returns the maximum finish position" <|
                \() ->
                    maxFinishToken (createBarcodeScannerData (Dict.fromList [ ( 47, [ "A4580442" ] ), ( 59, [ "A456321" ] ), ( 33, [ "A464631" ] ) ]) [] [])
                        |> Expect.equal (Just 59)
            ]
        , describe "generateDownloadText tests"
            [ test "generateDownloadText returns an empty string for empty data" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [])
                        |> Expect.equal ""
            , test "generateDownloadText returns a string for a single scanned barcode" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ BarcodeScannerFileLine 1 (Ordinary "A123456" (Just 47)) "14/03/2018 09:47:03" Unmodified ])
                        |> Expect.equal ("A123456,P0047,14/03/2018 09:47:03" ++ crlf)
            , test "generateDownloadText returns a string for a single athlete with no finish token" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ BarcodeScannerFileLine 1 (Ordinary "A123456" Nothing) "19/09/2018 09:33:37" Unmodified ])
                        |> Expect.equal ("A123456,,19/09/2018 09:33:37" ++ crlf)
            , test "generateDownloadText returns a string for a single finish token with no athlete" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ BarcodeScannerFileLine 1 (Ordinary "" (Just 47)) "19/09/2018 09:40:09" Unmodified ])
                        |> Expect.equal (",P0047,19/09/2018 09:40:09" ++ crlf)
            , test "generateDownloadText returns a string for a single mis-scanned item" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ BarcodeScannerFileLine 1 (MisScan "&d084") "04/07/2018 09:42:22" Unmodified ])
                        |> Expect.equal ("&d084,04/07/2018 09:42:22" ++ crlf)
            , test "generateDownloadText returns an empty string for a deleted record" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ BarcodeScannerFileLine 1 (Ordinary "A123456" (Just 47)) "14/03/2018 08:57:50" (Deleted BeforeEventStart) ])
                        |> Expect.equal ""
            , test "generateDownloadText returns the correct string for multiple items" <|
                \() ->
                    generateDownloadText
                        (BarcodeScannerFile "barcodes.txt"
                            [ BarcodeScannerFileLine 1 (Ordinary "A123456" (Just 47)) "14/03/2018 09:47:03" Unmodified
                            , BarcodeScannerFileLine 2 (Ordinary "A123456" Nothing) "19/09/2018 09:33:37" Unmodified
                            , BarcodeScannerFileLine 3 (Ordinary "" (Just 47)) "19/09/2018 09:40:09" Unmodified
                            , BarcodeScannerFileLine 4 (MisScan "&d084") "04/07/2018 09:42:22" Unmodified
                            , BarcodeScannerFileLine 5 (Ordinary "A123456" (Just 47)) "14/03/2018 08:57:50" (Deleted BeforeEventStart)
                            ]
                        )
                        |> Expect.equal
                            ("A123456,P0047,14/03/2018 09:47:03"
                                ++ crlf
                                ++ "A123456,,19/09/2018 09:33:37"
                                ++ crlf
                                ++ ",P0047,19/09/2018 09:40:09"
                                ++ crlf
                                ++ "&d084,04/07/2018 09:42:22"
                                ++ crlf
                            )
            ]
        ]
