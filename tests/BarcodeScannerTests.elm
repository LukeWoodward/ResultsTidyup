module BarcodeScannerTests exposing (createBarcodeScannerData, expectSingleUnrecognisedLine, suite)

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        , MisScannedItem
        , PositionAndTimePair
        , UnrecognisedLine
        , deleteBarcodeScannerLine
        , empty
        , generateDownloadText
        , isEmpty
        , maxFinishToken
        , readBarcodeScannerData
        , regenerate
        , updateBarcodeScannerLine
        )
import Dict exposing (Dict)
import Error exposing (Error)
import Errors exposing (expectError)
import Expect exposing (Expectation)
import FileHandling exposing (crlf)
import Test exposing (Test, describe, test)
import TestData exposing (createBarcodeScannerDataFromFiles, ordinaryFileLine, toPosix)
import Time exposing (Posix)


dummyTime : String
dummyTime =
    "14/03/2018 09:47:03"


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


deleteByUser : BarcodeScannerFileLine -> BarcodeScannerFileLine
deleteByUser line =
    { line | deletionStatus = Deleted DeletedByUser }


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
                                    [ BarcodeScannerFile
                                        "barcodes1.txt"
                                        [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
                                        (toPosix "2018-03-14T09:47:03.000Z")
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
                                    [ BarcodeScannerFile
                                        "barcodes2.txt"
                                        [ ordinaryFileLine 1 "A4580442" Nothing "14/03/2018 09:47:03" ]
                                        (toPosix "2018-03-14T09:47:03.000Z")
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
                                    [ BarcodeScannerFile
                                        "barcodes3.txt"
                                        [ ordinaryFileLine 1 "" (Just 47) "14/03/2018 09:47:03" ]
                                        (toPosix "2018-03-14T09:47:03.000Z")
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
                                    [ BarcodeScannerFile
                                        "barcodes4.txt"
                                        [ BarcodeScannerFileLine 1 (MisScan "&d084") "14/03/2018 09:47:03" NotDeleted ]
                                        (toPosix "2018-03-14T09:47:03.000Z")
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
                                    [ BarcodeScannerFile
                                        "barcodes5.txt"
                                        [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
                                        (toPosix "2018-03-14T09:47:03.000Z")
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
                                    [ BarcodeScannerFile
                                        "barcodes6.txt"
                                        [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                        , ordinaryFileLine 2 "A1866207" (Just 47) "14/03/2018 09:48:44"
                                        ]
                                        (toPosix "2018-03-14T09:48:44.000Z")
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
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [] Nothing)
                        |> Expect.equal ""
            , test "generateDownloadText returns a string for a single scanned barcode" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ ordinaryFileLine 1 "A123456" (Just 47) "14/03/2018 09:47:03" ] Nothing)
                        |> Expect.equal ("A123456,P0047,14/03/2018 09:47:03" ++ crlf)
            , test "generateDownloadText returns a string for a single athlete with no finish token" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ ordinaryFileLine 1 "A123456" Nothing "19/09/2018 09:33:37" ] Nothing)
                        |> Expect.equal ("A123456,,19/09/2018 09:33:37" ++ crlf)
            , test "generateDownloadText returns a string for a single finish token with no athlete" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ ordinaryFileLine 1 "" (Just 47) "19/09/2018 09:40:09" ] Nothing)
                        |> Expect.equal (",P0047,19/09/2018 09:40:09" ++ crlf)
            , test "generateDownloadText returns a string for a single mis-scanned item" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ BarcodeScannerFileLine 1 (MisScan "&d084") "04/07/2018 09:42:22" NotDeleted ] Nothing)
                        |> Expect.equal ("&d084,04/07/2018 09:42:22" ++ crlf)
            , test "generateDownloadText returns an empty string for a deleted record" <|
                \() ->
                    generateDownloadText (BarcodeScannerFile "barcodes.txt" [ BarcodeScannerFileLine 1 (Ordinary "A123456" (Just 47)) "14/03/2018 08:57:50" (Deleted BeforeEventStart) ] Nothing)
                        |> Expect.equal ""
            , test "generateDownloadText returns the correct string for multiple items" <|
                \() ->
                    generateDownloadText
                        (BarcodeScannerFile
                            "barcodes.txt"
                            [ ordinaryFileLine 1 "A123456" (Just 47) "14/03/2018 09:47:03"
                            , ordinaryFileLine 2 "A123456" Nothing "19/09/2018 09:33:37"
                            , ordinaryFileLine 3 "" (Just 47) "19/09/2018 09:40:09"
                            , BarcodeScannerFileLine 4 (MisScan "&d084") "04/07/2018 09:42:22" NotDeleted
                            , BarcodeScannerFileLine 5 (Ordinary "A123456" (Just 47)) "14/03/2018 08:57:50" (Deleted BeforeEventStart)
                            ]
                            Nothing
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
        , describe "updateBarcodeScannerLine tests"
            [ test "Updating a line in barcode scanner data updates the line" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A2022807" (Just 37) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal expectedBarcodeScannerData (updateBarcodeScannerLine "barcodes6.txt" 1 "A2022807" (Just 37) initialBarcodeScannerData)
            , test "Updating a deleted line in barcode scanner data updates the line and undeletes it" <|
                \() ->
                    let
                        markAsDeleted : BarcodeScannerFileLine -> BarcodeScannerFileLine
                        markAsDeleted line =
                            { line | deletionStatus = Deleted DeletedByUser }

                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ markAsDeleted (ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03")
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A2022807" (Just 37) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal expectedBarcodeScannerData (updateBarcodeScannerLine "barcodes6.txt" 1 "A2022807" (Just 37) initialBarcodeScannerData)
            , test "Updating a line in barcode scanner data with two files updates the line from the correct file" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes1.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                , BarcodeScannerFile
                                    "barcodes2.txt"
                                    [ ordinaryFileLine 1 "A1866207" (Just 58) "14/03/2018 09:48:44" ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes1.txt"
                                    [ ordinaryFileLine 1 "A2022807" (Just 31) "14/03/2018 09:47:03" ]
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                , BarcodeScannerFile
                                    "barcodes2.txt"
                                    [ ordinaryFileLine 1 "A1866207" (Just 58) "14/03/2018 09:48:44" ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal expectedBarcodeScannerData (updateBarcodeScannerLine "barcodes1.txt" 1 "A2022807" (Just 31) initialBarcodeScannerData)
            , test "Updating a line in barcode scanner data with no matching line does nothing" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal initialBarcodeScannerData (updateBarcodeScannerLine "barcodes6.txt" 99 "A2022807" (Just 31) initialBarcodeScannerData)
            , test "Updating a line in barcode scanner data with no matching filename does nothing" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 47) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal initialBarcodeScannerData (updateBarcodeScannerLine "wrong-filename.txt" 1 "A2022807" (Just 31) initialBarcodeScannerData)
            ]
        , describe "deleteBarcodeScannerLine tests"
            [ test "Deleting a line from barcode scanner data deletes that line" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ deleteByUser (ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03")
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal expectedBarcodeScannerData (deleteBarcodeScannerLine "barcodes6.txt" 1 initialBarcodeScannerData)
            , test "Deleting a line from barcode scanner data with two files deletes the line from the correct file" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes1.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                , BarcodeScannerFile
                                    "barcodes2.txt"
                                    [ ordinaryFileLine 1 "A1866207" (Just 58) "14/03/2018 09:48:44" ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes1.txt"
                                    [ deleteByUser (ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03") ]
                                    (toPosix "2018-03-14T09:47:03.000Z")
                                , BarcodeScannerFile
                                    "barcodes2.txt"
                                    [ ordinaryFileLine 1 "A1866207" (Just 58) "14/03/2018 09:48:44" ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal expectedBarcodeScannerData (deleteBarcodeScannerLine "barcodes1.txt" 1 initialBarcodeScannerData)
            , test "Deleting a line from barcode scanner data with no matching line does nothing" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal initialBarcodeScannerData (deleteBarcodeScannerLine "barcodes6.txt" 99 initialBarcodeScannerData)
            , test "Deleting a line from barcode scanner data with no matching filename does nothing" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 47) "14/03/2018 09:48:44"
                                    ]
                                    (toPosix "2018-03-14T09:48:44.000Z")
                                ]
                    in
                    Expect.equal initialBarcodeScannerData (deleteBarcodeScannerLine "wrong-filename.txt" 1 initialBarcodeScannerData)
            ]
        ]
