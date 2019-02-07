module WrongWayAroundTests exposing (suite)

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , LineContents(..)
        , ModificationStatus(..)
        , WrongWayAroundStatus(..)
        )
import Dict
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import WrongWayAround exposing (identifyBarcodesScannedTheWrongWayAround)


createBarcodeScannerDataForWrongWayAroundTests : List LineContents -> BarcodeScannerData
createBarcodeScannerDataForWrongWayAroundTests lineContents =
    let
        createLine : Int -> LineContents -> BarcodeScannerFileLine
        createLine index contents =
            BarcodeScannerFileLine (index + 1) contents "14/03/2018 09:47:03" Unmodified NotWrongWayAround
    in
    BarcodeScannerData
        [ BarcodeScannerFile "barcodes1.txt" (List.indexedMap createLine lineContents) Nothing ]
        Dict.empty
        []
        []
        []
        []
        Nothing


lenStr : List a -> String
lenStr list =
    List.length list
        |> String.fromInt


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith func list1 list2 =
    case ( list1, list2 ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( first1 :: rest1, first2 :: rest2 ) ->
            func first1 first2 :: zipWith func rest1 rest2


expectEqualWithStatuses : List WrongWayAroundStatus -> BarcodeScannerData -> BarcodeScannerData -> Expectation
expectEqualWithStatuses statuses expectedDataWithoutStatuses actualData =
    case expectedDataWithoutStatuses.files of
        [ singleFile ] ->
            if List.length singleFile.lines == List.length statuses then
                let
                    statusSetter : WrongWayAroundStatus -> BarcodeScannerFileLine -> BarcodeScannerFileLine
                    statusSetter newStatus oldLine =
                        { oldLine | wrongWayAroundStatus = newStatus }

                    expectedLinesWithStatuses : List BarcodeScannerFileLine
                    expectedLinesWithStatuses =
                        zipWith statusSetter statuses singleFile.lines

                    expectedData : BarcodeScannerData
                    expectedData =
                        { expectedDataWithoutStatuses | files = [ { singleFile | lines = expectedLinesWithStatuses } ] }
                in
                Expect.equal expectedData actualData

            else
                Expect.fail ("Expected precisely " ++ lenStr statuses ++ " lines, but got " ++ lenStr singleFile.lines ++ " instead.")

        _ ->
            Expect.fail ("Expected precisely one file, but got " ++ lenStr expectedDataWithoutStatuses.files ++ " instead.")


suite : Test
suite =
    describe "WrongWayAround tests"
        [ describe "identifyBarcodesScannedTheWrongWayAround tests"
            [ test "identifyBarcodesScannedTheWrongWayAround returns empty data for empty data" <|
                \() ->
                    identifyBarcodesScannedTheWrongWayAround BarcodeScanner.empty
                        |> Expect.equal BarcodeScanner.empty
            , test "identifyBarcodesScannedTheWrongWayAround returns a single empty file for a single empty file" <|
                \() ->
                    let
                        emptySingleFileBarcodeScannerData : BarcodeScannerData
                        emptySingleFileBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests []
                    in
                    identifyBarcodesScannedTheWrongWayAround emptySingleFileBarcodeScannerData
                        |> Expect.equal emptySingleFileBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a single valid record" <|
                \() ->
                    let
                        singleRecordBarcodeScannerData : BarcodeScannerData
                        singleRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests [ Ordinary "A123456" (Just 47) ]
                    in
                    identifyBarcodesScannedTheWrongWayAround singleRecordBarcodeScannerData
                        |> Expect.equal singleRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a single athlete-only record" <|
                \() ->
                    let
                        singleRecordBarcodeScannerData : BarcodeScannerData
                        singleRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests [ Ordinary "A123456" Nothing ]
                    in
                    identifyBarcodesScannedTheWrongWayAround singleRecordBarcodeScannerData
                        |> Expect.equal singleRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a single finish-token-only record" <|
                \() ->
                    let
                        singleRecordBarcodeScannerData : BarcodeScannerData
                        singleRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests [ Ordinary "" (Just 47) ]
                    in
                    identifyBarcodesScannedTheWrongWayAround singleRecordBarcodeScannerData
                        |> Expect.equal singleRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a single mis-scan record" <|
                \() ->
                    let
                        singleRecordBarcodeScannerData : BarcodeScannerData
                        singleRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests [ BarcodeScanner.MisScan "junk" ]
                    in
                    identifyBarcodesScannedTheWrongWayAround singleRecordBarcodeScannerData
                        |> Expect.equal singleRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a finish-token only record followed by two valid records" <|
                \() ->
                    let
                        tripleRecordBarcodeScannerData : BarcodeScannerData
                        tripleRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests [ Ordinary "" (Just 47), Ordinary "A123456" (Just 59), Ordinary "A654321" (Just 77) ]
                    in
                    identifyBarcodesScannedTheWrongWayAround tripleRecordBarcodeScannerData
                        |> Expect.equal tripleRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns modified data for a finish-token only record followed by an athlete-only record" <|
                \() ->
                    let
                        doubleRecordBarcodeScannerData : BarcodeScannerData
                        doubleRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests [ Ordinary "" (Just 47), Ordinary "A123456" Nothing ]

                        expectedStatuses : List WrongWayAroundStatus
                        expectedStatuses =
                            [ FirstWrongWayAround 1 2, SubsequentWrongWayAround ]
                    in
                    identifyBarcodesScannedTheWrongWayAround doubleRecordBarcodeScannerData
                        |> expectEqualWithStatuses expectedStatuses doubleRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns modified data for a finish-token only record followed by an athlete-only record and a complete record" <|
                \() ->
                    let
                        doubleRecordBarcodeScannerData : BarcodeScannerData
                        doubleRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests [ Ordinary "" (Just 47), Ordinary "A123456" Nothing, Ordinary "A999090" (Just 15) ]

                        expectedStatuses : List WrongWayAroundStatus
                        expectedStatuses =
                            [ FirstWrongWayAround 1 2, SubsequentWrongWayAround, NotWrongWayAround ]
                    in
                    identifyBarcodesScannedTheWrongWayAround doubleRecordBarcodeScannerData
                        |> expectEqualWithStatuses expectedStatuses doubleRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns modified data for a finish-token only record, an athlete-only record and complete records before, between and after" <|
                \() ->
                    let
                        multiRecordBarcodeScannerData : BarcodeScannerData
                        multiRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests
                                [ Ordinary "A1234" (Just 9)
                                , Ordinary "" (Just 47)
                                , Ordinary "A3456" (Just 20)
                                , Ordinary "A123456" Nothing
                                , Ordinary "A999090" (Just 15)
                                ]

                        expectedStatuses : List WrongWayAroundStatus
                        expectedStatuses =
                            [ NotWrongWayAround, FirstWrongWayAround 2 4, SubsequentWrongWayAround, SubsequentWrongWayAround, NotWrongWayAround ]
                    in
                    identifyBarcodesScannedTheWrongWayAround multiRecordBarcodeScannerData
                        |> expectEqualWithStatuses expectedStatuses multiRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns modified data for a finish-token only record, followed by a repeated finish token position" <|
                \() ->
                    let
                        multiRecordBarcodeScannerData : BarcodeScannerData
                        multiRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests
                                [ Ordinary "A1234" (Just 9)
                                , Ordinary "" (Just 47)
                                , Ordinary "A3456" (Just 20)
                                , Ordinary "A123456" (Just 20)
                                , Ordinary "A999090" (Just 15)
                                ]

                        expectedStatuses : List WrongWayAroundStatus
                        expectedStatuses =
                            [ NotWrongWayAround, FirstWrongWayAround 2 3, SubsequentWrongWayAround, NotWrongWayAround, NotWrongWayAround ]
                    in
                    identifyBarcodesScannedTheWrongWayAround multiRecordBarcodeScannerData
                        |> expectEqualWithStatuses expectedStatuses multiRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns modified data for two pairs of wrong-way-around data" <|
                \() ->
                    let
                        multiRecordBarcodeScannerData : BarcodeScannerData
                        multiRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests
                                [ Ordinary "" (Just 9)
                                , Ordinary "A1234" Nothing
                                , Ordinary "A999090" (Just 15)
                                , Ordinary "" (Just 20)
                                , Ordinary "A123456" Nothing
                                ]

                        expectedStatuses : List WrongWayAroundStatus
                        expectedStatuses =
                            [ FirstWrongWayAround 1 2, SubsequentWrongWayAround, NotWrongWayAround, FirstWrongWayAround 4 5, SubsequentWrongWayAround ]
                    in
                    identifyBarcodesScannedTheWrongWayAround multiRecordBarcodeScannerData
                        |> expectEqualWithStatuses expectedStatuses multiRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a finish-token-only record followed by another" <|
                \() ->
                    let
                        multiRecordBarcodeScannerData : BarcodeScannerData
                        multiRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests
                                [ Ordinary "" (Just 9)
                                , Ordinary "" (Just 22)
                                ]
                    in
                    identifyBarcodesScannedTheWrongWayAround multiRecordBarcodeScannerData
                        |> Expect.equal multiRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a finish-token-only record followed by only a complete record" <|
                \() ->
                    let
                        multiRecordBarcodeScannerData : BarcodeScannerData
                        multiRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests
                                [ Ordinary "" (Just 9)
                                , Ordinary "A654321" (Just 22)
                                ]
                    in
                    identifyBarcodesScannedTheWrongWayAround multiRecordBarcodeScannerData
                        |> Expect.equal multiRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a finish-token-only record followed by a mis-scan" <|
                \() ->
                    let
                        multiRecordBarcodeScannerData : BarcodeScannerData
                        multiRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests
                                [ Ordinary "" (Just 9)
                                , BarcodeScanner.MisScan "rubbish"
                                ]
                    in
                    identifyBarcodesScannedTheWrongWayAround multiRecordBarcodeScannerData
                        |> Expect.equal multiRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns unmodified data for a finish-token-only record followed by a complete record with the same finish token" <|
                \() ->
                    let
                        multiRecordBarcodeScannerData : BarcodeScannerData
                        multiRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests
                                [ Ordinary "" (Just 9)
                                , Ordinary "A123456" (Just 9)
                                ]
                    in
                    identifyBarcodesScannedTheWrongWayAround multiRecordBarcodeScannerData
                        |> Expect.equal multiRecordBarcodeScannerData
            , test "identifyBarcodesScannedTheWrongWayAround returns modified data for two finish-token-only records followed by an athlete-only record" <|
                \() ->
                    let
                        multiRecordBarcodeScannerData : BarcodeScannerData
                        multiRecordBarcodeScannerData =
                            createBarcodeScannerDataForWrongWayAroundTests
                                [ Ordinary "" (Just 9)
                                , Ordinary "" (Just 22)
                                , Ordinary "A654321" Nothing
                                ]

                        expectedStatuses : List WrongWayAroundStatus
                        expectedStatuses =
                            [ NotWrongWayAround, FirstWrongWayAround 2 3, SubsequentWrongWayAround ]
                    in
                    identifyBarcodesScannedTheWrongWayAround multiRecordBarcodeScannerData
                        |> expectEqualWithStatuses expectedStatuses multiRecordBarcodeScannerData
            ]
        ]
