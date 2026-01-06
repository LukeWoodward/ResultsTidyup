module TestData exposing
    ( createBarcodeScannerDataFromFiles
    , defaultMatchSummary
    , doubleTimers
    , expectedDownloadedTimerData1
    , expectedDownloadedTimerData2
    , expectedMergedTimerFileContents
    , expectedParsedSampleTimerData
    , flippedDoubleTimers
    , invalidBarcodeScannerData
    , ordinaryFileLine
    , parsedBarcodeScannerData1
    , parsedBarcodeScannerData1And2
    , parsedBarcodeScannerDataWithIncompleteRecordFirst
    , parsedInvalidBarcodeScannerData
    , parsedTimerTimes1
    , parsedTimerTimes2
    , recentTime
    , sampleDownloadedTimerData
    , sampleTimerData
    , sampleTimerData2
    , singleTimer
    , timersForAdjusting
    , validBarcodeScannerData1
    , validBarcodeScannerData2
    , validBarcodeScannerDataWithIncompleteRecordFirst
    )

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionStatus(..)
        , LineContents(..)
        , regenerate
        )
import Dict
import FileHandling exposing (crlf)
import Time
import Timer
    exposing
        ( MergeEntry(..)
        , MergedTableRow
        , Timer(..)
        , TimerFile
        , TimerMatchSummary
        , Timers(..)
        , WhichTimer(..)
        )


sampleTimerData : String
sampleTimerData =
    "STARTOFEVENT,01/01/2001 00:00:00,abcdefghij\n"
        ++ "0,01/01/2001 00:00:00\n"
        ++ "1,01/01/2001 00:03:11,00:03:11\n"
        ++ "2,01/01/2001 00:07:44,00:07:44\n"
        ++ "3,01/01/2001 00:10:03,00:10:03\n"
        ++ "4,01/01/2001 00:12:26,00:12:26\n"
        ++ "5,01/01/2001 00:14:42,00:14:42\n"
        ++ "6,01/01/2001 00:17:09,00:17:09\n"
        ++ "ENDOFEVENT,01/01/2001 00:19:23\n"


sampleDownloadedTimerData : String
sampleDownloadedTimerData =
    "I, CP765, 47\n"
        ++ "S, 001, SPLIT\n"
        ++ "T,1,1,00:03:11.000\n"
        ++ "T,2,1,00:07:44.000\n"
        ++ "T,3,1,00:10:03.000\n"
        ++ "T,4,1,00:12:26.000\n"
        ++ "T,5,1,00:14:42.000\n"
        ++ "T,6,1,00:17:09.000\n"


parsedTimerTimes1 : List Int
parsedTimerTimes1 =
    [ 3 * 60 + 11, 7 * 60 + 44, 10 * 60 + 3, 12 * 60 + 26, 14 * 60 + 42, 17 * 60 + 9 ]


expectedParsedSampleTimerData : Timer
expectedParsedSampleTimerData =
    TimerData parsedTimerTimes1


singleTimer : Timers
singleTimer =
    Single (TimerFile "timer1.txt" "Name1") parsedTimerTimes1


defaultMatchSummary : TimerMatchSummary
defaultMatchSummary =
    TimerMatchSummary 0 0 0 0 0


ordinaryFileLine : Int -> String -> Maybe Int -> String -> BarcodeScannerFileLine
ordinaryFileLine lineNumber athlete finishToken scanTime =
    BarcodeScannerFileLine lineNumber (Ordinary athlete finishToken) scanTime NotDeleted


sampleTimerData2 : String
sampleTimerData2 =
    "STARTOFEVENT,01/01/2001 00:00:00,klmnopqrst\n"
        ++ "0,01/01/2001 00:00:00\n"
        ++ "1,01/01/2001 00:03:11,00:03:11\n"
        ++ "2,01/01/2001 00:07:43,00:07:43\n"
        ++ "3,01/01/2001 00:12:26,00:12:26\n"
        ++ "4,01/01/2001 00:13:11,00:13:11\n"
        ++ "5,01/01/2001 00:14:42,00:14:42\n"
        ++ "ENDOFEVENT,01/01/2001 00:19:23\n"


parsedTimerTimes2 : List Int
parsedTimerTimes2 =
    [ 3 * 60 + 11, 7 * 60 + 43, 12 * 60 + 26, 13 * 60 + 11, 14 * 60 + 42 ]


doubleTimers : Timers
doubleTimers =
    let
        expectedEntries : List MergedTableRow
        expectedEntries =
            [ { index = 0, rowNumber = Just 1, entry = ExactMatch 191, included = True }
            , { index = 1, rowNumber = Just 2, entry = NearMatch 464 463, included = True }
            , { index = 2, rowNumber = Just 3, entry = OneWatchOnly TimerOne 603, included = True }
            , { index = 3, rowNumber = Just 4, entry = ExactMatch 746, included = True }
            , { index = 4, rowNumber = Just 5, entry = OneWatchOnly TimerTwo 791, included = True }
            , { index = 5, rowNumber = Just 6, entry = ExactMatch 882, included = True }
            , { index = 6, rowNumber = Just 7, entry = OneWatchOnly TimerOne 1029, included = True }
            ]

        expectedMatchSummary : TimerMatchSummary
        expectedMatchSummary =
            { exactMatches = 3, nearMatches = 1, notNearMatches = 0, timer1Only = 2, timer2Only = 1 }
    in
    Double
        { times1 = parsedTimerTimes1
        , times2 = parsedTimerTimes2
        , file1 = TimerFile "timer1.txt" "Name1"
        , file2 = TimerFile "timer2.txt" "Name2"
        , mergedTableRows = expectedEntries
        , matchSummary = expectedMatchSummary
        }


flippedDoubleTimers : Timers
flippedDoubleTimers =
    let
        expectedEntries : List MergedTableRow
        expectedEntries =
            [ { index = 0, rowNumber = Just 1, entry = ExactMatch 191, included = True }
            , { index = 1, rowNumber = Just 2, entry = NearMatch 463 464, included = True }
            , { index = 2, rowNumber = Just 3, entry = OneWatchOnly TimerTwo 603, included = True }
            , { index = 3, rowNumber = Just 4, entry = ExactMatch 746, included = True }
            , { index = 4, rowNumber = Just 5, entry = OneWatchOnly TimerOne 791, included = True }
            , { index = 5, rowNumber = Just 6, entry = ExactMatch 882, included = True }
            , { index = 6, rowNumber = Just 7, entry = OneWatchOnly TimerTwo 1029, included = True }
            ]

        expectedMatchSummary : TimerMatchSummary
        expectedMatchSummary =
            { exactMatches = 3, nearMatches = 1, notNearMatches = 0, timer1Only = 1, timer2Only = 2 }
    in
    Double
        { times1 = parsedTimerTimes2
        , times2 = parsedTimerTimes1
        , file1 = TimerFile "timer2.txt" "Name2"
        , file2 = TimerFile "timer1.txt" "Name1"
        , mergedTableRows = expectedEntries
        , matchSummary = expectedMatchSummary
        }


timersForAdjusting : Int -> Int -> Timers
timersForAdjusting timer1Offset timer2Offset =
    Double
        { times1 = List.map (\t -> t + timer1Offset) parsedTimerTimes1
        , times2 = List.map (\t -> t + timer2Offset) parsedTimerTimes2
        , file1 = TimerFile "timer1.txt" "Name1"
        , file2 = TimerFile "timer2.txt" "Name2"
        , mergedTableRows = []
        , matchSummary = defaultMatchSummary
        }


validBarcodeScannerData1 : String
validBarcodeScannerData1 =
    "A4580442,P0047,14/03/2018 09:47:03" ++ crlf


validBarcodeScannerData2 : String
validBarcodeScannerData2 =
    "A2044293,P0059,14/03/2018 09:49:44" ++ crlf


validBarcodeScannerDataWithIncompleteRecordFirst : String
validBarcodeScannerDataWithIncompleteRecordFirst =
    "A2044293,,14/03/2018 09:44:06" ++ crlf ++ validBarcodeScannerData1


invalidBarcodeScannerData : String
invalidBarcodeScannerData =
    "A4580442,P0000,14/03/2018 09:47:03" ++ crlf


parsedBarcodeScannerData1 : BarcodeScannerData
parsedBarcodeScannerData1 =
    BarcodeScannerData
        [ BarcodeScannerFile
            "barcodes1.txt"
            "Name1"
            [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
        ]
        (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ])
        []
        []


parsedBarcodeScannerData1And2 : BarcodeScannerData
parsedBarcodeScannerData1And2 =
    BarcodeScannerData
        [ BarcodeScannerFile
            "barcodes1.txt"
            "Name1"
            [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
        , BarcodeScannerFile
            "barcodes2.txt"
            "Name2"
            [ ordinaryFileLine 1 "A2044293" (Just 59) "14/03/2018 09:49:44" ]
        ]
        (Dict.fromList
            [ ( 47, [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ] )
            , ( 59, [ AthleteAndTimePair "A2044293" "14/03/2018 09:49:44" ] )
            ]
        )
        []
        []


parsedBarcodeScannerDataWithIncompleteRecordFirst : BarcodeScannerData
parsedBarcodeScannerDataWithIncompleteRecordFirst =
    BarcodeScannerData
        [ BarcodeScannerFile
            "barcodes1.txt"
            "Name1"
            [ ordinaryFileLine 1 "A2044293" Nothing "14/03/2018 09:44:06"
            , ordinaryFileLine 2 "A4580442" (Just 47) "14/03/2018 09:47:03"
            ]
        ]
        (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ])
        [ AthleteAndTimePair "A2044293" "14/03/2018 09:44:06" ]
        []


parsedInvalidBarcodeScannerData : BarcodeScannerData
parsedInvalidBarcodeScannerData =
    BarcodeScannerData
        [ BarcodeScannerFile "invalid.txt" "Invalid" [] ]
        Dict.empty
        []
        [ { errorCode = "INVALID_POSITION_ZERO"
          , errorMessage = "Invalid position record 'P0000' found in barcode scanner file"
          , line = "A4580442,P0000,14/03/2018 09:47:03"
          }
        ]


recentTime : Time.Posix
recentTime =
    Time.millisToPosix 1500000000000


expectedMergedTimerFileContents : String
expectedMergedTimerFileContents =
    "STARTOFEVENT,01/01/2001 00:00:00,results_tidyup"
        ++ crlf
        ++ "0,01/01/2001 00:00:00"
        ++ crlf
        ++ "1,01/01/2001 00:03:11,00:03:11"
        ++ crlf
        ++ "2,01/01/2001 00:07:43,00:07:43"
        ++ crlf
        ++ "3,01/01/2001 00:10:03,00:10:03"
        ++ crlf
        ++ "4,01/01/2001 00:12:26,00:12:26"
        ++ crlf
        ++ "5,01/01/2001 00:13:11,00:13:11"
        ++ crlf
        ++ "6,01/01/2001 00:14:42,00:14:42"
        ++ crlf
        ++ "7,01/01/2001 00:17:09,00:17:09"
        ++ crlf
        ++ "ENDOFEVENT,01/01/2001 01:59:59"


expectedDownloadedTimerData1 : String
expectedDownloadedTimerData1 =
    "STARTOFEVENT,01/01/2001 00:00:00,results_tidyup"
        ++ crlf
        ++ "0,01/01/2001 00:00:00"
        ++ crlf
        ++ "1,01/01/2001 00:03:11,00:03:11"
        ++ crlf
        ++ "2,01/01/2001 00:07:44,00:07:44"
        ++ crlf
        ++ "3,01/01/2001 00:10:03,00:10:03"
        ++ crlf
        ++ "4,01/01/2001 00:12:26,00:12:26"
        ++ crlf
        ++ "5,01/01/2001 00:14:42,00:14:42"
        ++ crlf
        ++ "6,01/01/2001 00:17:09,00:17:09"
        ++ crlf
        ++ "ENDOFEVENT,01/01/2001 01:59:59"


expectedDownloadedTimerData2 : String
expectedDownloadedTimerData2 =
    "STARTOFEVENT,01/01/2001 00:00:00,results_tidyup"
        ++ crlf
        ++ "0,01/01/2001 00:00:00"
        ++ crlf
        ++ "1,01/01/2001 00:03:11,00:03:11"
        ++ crlf
        ++ "2,01/01/2001 00:07:43,00:07:43"
        ++ crlf
        ++ "3,01/01/2001 00:12:26,00:12:26"
        ++ crlf
        ++ "4,01/01/2001 00:13:11,00:13:11"
        ++ crlf
        ++ "5,01/01/2001 00:14:42,00:14:42"
        ++ crlf
        ++ "ENDOFEVENT,01/01/2001 01:59:59"


createBarcodeScannerDataFromFiles : List BarcodeScannerFile -> BarcodeScannerData
createBarcodeScannerDataFromFiles files =
    BarcodeScannerData files Dict.empty [] []
        |> regenerate
