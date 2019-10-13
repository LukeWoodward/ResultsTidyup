module TestData exposing
    ( createBarcodeScannerDataFromFiles
    , createNumberCheckerManualEntryRow
    , defaultDateTime
    , defaultMatchSummary
    , doubleStopwatches
    , expectedDownloadedStopwatchData1
    , expectedDownloadedStopwatchData2
    , expectedMergedStopwatchFileContents
    , expectedParsedSampleStopwatchData
    , flippedDoubleStopwatches
    , invalidBarcodeScannerData
    , invalidNumberCheckerData
    , ordinaryFileLine
    , parsedBarcodeScannerData1
    , parsedBarcodeScannerData1And2
    , parsedBarcodeScannerDataWithIncompleteRecordFirst
    , parsedEventDateOnly
    , parsedInvalidBarcodeScannerData
    , parsedNumberCheckerData
    , parsedStopwatchTimes2
    , recentTime
    , sampleNumberCheckerData
    , sampleNumberCheckerDataDecremented
    , sampleNumberCheckerDataIncremented
    , sampleNumberCheckerDataWithSecondItemRemoved
    , sampleStopwatchData
    , sampleStopwatchData2
    , singleStopwatch
    , stopwatchesForAdjusting
    , toPosix
    , validBarcodeScannerData1
    , validBarcodeScannerData2
    , validBarcodeScannerDataWithIncompleteRecordFirst
    , validNumberCheckerData
    )

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionStatus(..)
        , LineContents(..)
        , PositionAndTimePair
        , regenerate
        )
import Dict
import EventDateAndTime exposing (EventDateAndTime)
import FileHandling exposing (crlf)
import Iso8601
import Model exposing (NumberCheckerManualEntryRow)
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import NumericEntry exposing (numericEntryFromInt)
import Stopwatch
    exposing
        ( MergeEntry(..)
        , MergedTableRow
        , Stopwatch(..)
        , StopwatchMatchSummary
        , Stopwatches(..)
        , WhichStopwatch(..)
        , noUnderlines
        )
import Time exposing (Posix)


defaultDateTime : Maybe Time.Posix
defaultDateTime =
    toPosix "2018-03-14T09:47:03.000Z"


sampleStopwatchData : String
sampleStopwatchData =
    "STARTOFEVENT,01/01/2001 00:00:00,abcdefghij\n"
        ++ "0,01/01/2001 00:00:00\n"
        ++ "1,01/01/2001 00:03:11,00:03:11\n"
        ++ "2,01/01/2001 00:07:44,00:07:44\n"
        ++ "3,01/01/2001 00:10:03,00:10:03\n"
        ++ "4,01/01/2001 00:12:26,00:12:26\n"
        ++ "5,01/01/2001 00:14:42,00:14:42\n"
        ++ "6,01/01/2001 00:17:09,00:17:09\n"
        ++ "ENDOFEVENT,01/01/2001 00:19:23\n"


parsedStopwatchTimes1 : List Int
parsedStopwatchTimes1 =
    [ 3 * 60 + 11, 7 * 60 + 44, 10 * 60 + 3, 12 * 60 + 26, 14 * 60 + 42, 17 * 60 + 9 ]


expectedParsedSampleStopwatchData : Stopwatch
expectedParsedSampleStopwatchData =
    StopwatchData parsedStopwatchTimes1


singleStopwatch : Stopwatches
singleStopwatch =
    case expectedParsedSampleStopwatchData of
        StopwatchData times ->
            Single "stopwatch1.txt" times


defaultMatchSummary : StopwatchMatchSummary
defaultMatchSummary =
    StopwatchMatchSummary 0 0 0 0 0


ordinaryFileLine : Int -> String -> Maybe Int -> String -> BarcodeScannerFileLine
ordinaryFileLine lineNumber athlete finishToken scanTime =
    BarcodeScannerFileLine lineNumber (Ordinary athlete finishToken) scanTime NotDeleted


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


sampleStopwatchData2 : String
sampleStopwatchData2 =
    "STARTOFEVENT,01/01/2001 00:00:00,klmnopqrst\n"
        ++ "0,01/01/2001 00:00:00\n"
        ++ "1,01/01/2001 00:03:11,00:03:11\n"
        ++ "2,01/01/2001 00:07:43,00:07:43\n"
        ++ "3,01/01/2001 00:12:26,00:12:26\n"
        ++ "4,01/01/2001 00:13:11,00:13:11\n"
        ++ "5,01/01/2001 00:14:42,00:14:42\n"
        ++ "ENDOFEVENT,01/01/2001 00:19:23\n"


parsedStopwatchTimes2 : List Int
parsedStopwatchTimes2 =
    [ 3 * 60 + 11, 7 * 60 + 43, 12 * 60 + 26, 13 * 60 + 11, 14 * 60 + 42 ]


doubleStopwatches : Stopwatches
doubleStopwatches =
    let
        expectedEntries : List MergedTableRow
        expectedEntries =
            [ { index = 0, rowNumber = Just 1, entry = ExactMatch 191, included = True, underlines = noUnderlines }
            , { index = 1, rowNumber = Just 2, entry = NearMatch 464 463, included = True, underlines = noUnderlines }
            , { index = 2, rowNumber = Just 3, entry = OneWatchOnly StopwatchOne 603, included = True, underlines = noUnderlines }
            , { index = 3, rowNumber = Just 4, entry = ExactMatch 746, included = True, underlines = noUnderlines }
            , { index = 4, rowNumber = Just 5, entry = OneWatchOnly StopwatchTwo 791, included = True, underlines = noUnderlines }
            , { index = 5, rowNumber = Just 6, entry = ExactMatch 882, included = True, underlines = noUnderlines }
            , { index = 6, rowNumber = Just 7, entry = OneWatchOnly StopwatchOne 1029, included = True, underlines = noUnderlines }
            ]

        expectedMatchSummary : StopwatchMatchSummary
        expectedMatchSummary =
            { exactMatches = 3, nearMatches = 1, notNearMatches = 0, stopwatch1Only = 2, stopwatch2Only = 1 }
    in
    Double
        { times1 = parsedStopwatchTimes1
        , times2 = parsedStopwatchTimes2
        , filename1 = "stopwatch1.txt"
        , filename2 = "stopwatch2.txt"
        , mergedTableRows = expectedEntries
        , matchSummary = expectedMatchSummary
        }


flippedDoubleStopwatches : Stopwatches
flippedDoubleStopwatches =
    let
        expectedEntries : List MergedTableRow
        expectedEntries =
            [ { index = 0, rowNumber = Just 1, entry = ExactMatch 191, included = True, underlines = noUnderlines }
            , { index = 1, rowNumber = Just 2, entry = NearMatch 463 464, included = True, underlines = noUnderlines }
            , { index = 2, rowNumber = Just 3, entry = OneWatchOnly StopwatchTwo 603, included = True, underlines = noUnderlines }
            , { index = 3, rowNumber = Just 4, entry = ExactMatch 746, included = True, underlines = noUnderlines }
            , { index = 4, rowNumber = Just 5, entry = OneWatchOnly StopwatchOne 791, included = True, underlines = noUnderlines }
            , { index = 5, rowNumber = Just 6, entry = ExactMatch 882, included = True, underlines = noUnderlines }
            , { index = 6, rowNumber = Just 7, entry = OneWatchOnly StopwatchTwo 1029, included = True, underlines = noUnderlines }
            ]

        expectedMatchSummary : StopwatchMatchSummary
        expectedMatchSummary =
            { exactMatches = 3, nearMatches = 1, notNearMatches = 0, stopwatch1Only = 1, stopwatch2Only = 2 }
    in
    Double
        { times1 = parsedStopwatchTimes2
        , times2 = parsedStopwatchTimes1
        , filename1 = "stopwatch2.txt"
        , filename2 = "stopwatch1.txt"
        , mergedTableRows = expectedEntries
        , matchSummary = expectedMatchSummary
        }


stopwatchesForAdjusting : Int -> Int -> Stopwatches
stopwatchesForAdjusting stopwatch1Offset stopwatch2Offset =
    Double
        { times1 = List.map (\t -> t + stopwatch1Offset) parsedStopwatchTimes1
        , times2 = List.map (\t -> t + stopwatch2Offset) parsedStopwatchTimes2
        , filename1 = "stopwatch1.txt"
        , filename2 = "stopwatch2.txt"
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
    ",P0033,14/03/2018 09:44:06" ++ crlf ++ validBarcodeScannerData1


invalidBarcodeScannerData : String
invalidBarcodeScannerData =
    "A4580442,P0000,14/03/2018 09:47:03" ++ crlf


parsedBarcodeScannerData1 : BarcodeScannerData
parsedBarcodeScannerData1 =
    BarcodeScannerData
        [ BarcodeScannerFile
            "barcodes1.txt"
            [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
            defaultDateTime
        ]
        (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ])
        []
        []
        []
        []
        defaultDateTime


parsedBarcodeScannerData1And2 : BarcodeScannerData
parsedBarcodeScannerData1And2 =
    BarcodeScannerData
        [ BarcodeScannerFile
            "barcodes1.txt"
            [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03" ]
            defaultDateTime
        , BarcodeScannerFile
            "barcodes2.txt"
            [ ordinaryFileLine 1 "A2044293" (Just 59) "14/03/2018 09:49:44" ]
            (toPosix "2018-03-14T09:49:44.000Z")
        ]
        (Dict.fromList
            [ ( 47, [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ] )
            , ( 59, [ AthleteAndTimePair "A2044293" "14/03/2018 09:49:44" ] )
            ]
        )
        []
        []
        []
        []
        (toPosix "2018-03-14T09:49:44.000Z")


parsedBarcodeScannerDataWithIncompleteRecordFirst : BarcodeScannerData
parsedBarcodeScannerDataWithIncompleteRecordFirst =
    BarcodeScannerData
        [ BarcodeScannerFile
            "barcodes1.txt"
            [ ordinaryFileLine 1 "" (Just 33) "14/03/2018 09:44:06"
            , ordinaryFileLine 2 "A4580442" (Just 47) "14/03/2018 09:47:03"
            ]
            defaultDateTime
        ]
        (Dict.singleton 47 [ AthleteAndTimePair "A4580442" "14/03/2018 09:47:03" ])
        []
        [ PositionAndTimePair 33 "14/03/2018 09:44:06" ]
        []
        []
        defaultDateTime


parsedInvalidBarcodeScannerData : BarcodeScannerData
parsedInvalidBarcodeScannerData =
    BarcodeScannerData
        [ BarcodeScannerFile "invalid.txt" [] Nothing ]
        Dict.empty
        []
        []
        []
        [ { errorCode = "INVALID_POSITION_ZERO"
          , errorMessage = "Invalid position record 'P0000' found in barcode scanner file"
          , line = "A4580442,P0000,14/03/2018 09:47:03"
          }
        ]
        Nothing


parsedEventDateOnly : EventDateAndTime
parsedEventDateOnly =
    EventDateAndTime "14/03/2018" (toPosix "2018-03-14T00:00:00.000Z") "" Nothing


validNumberCheckerData : String
validNumberCheckerData =
    "5,4,5"


invalidNumberCheckerData : String
invalidNumberCheckerData =
    "1,2,3,4,5,6"


parsedNumberCheckerData : List AnnotatedNumberCheckerEntry
parsedNumberCheckerData =
    [ { entryNumber = 1
      , stopwatch1 = 5
      , stopwatch1Delta = 0
      , stopwatch2 = 4
      , stopwatch2Delta = -1
      , finishTokens = 5
      , finishTokensDelta = 0
      , actual = 5
      }
    ]


recentTime : Time.Posix
recentTime =
    Time.millisToPosix 1500000000000


expectedMergedStopwatchFileContents : String
expectedMergedStopwatchFileContents =
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


expectedDownloadedStopwatchData1 : String
expectedDownloadedStopwatchData1 =
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


expectedDownloadedStopwatchData2 : String
expectedDownloadedStopwatchData2 =
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


sampleNumberCheckerData : List AnnotatedNumberCheckerEntry
sampleNumberCheckerData =
    [ { entryNumber = 1
      , stopwatch1 = 5
      , stopwatch1Delta = 0
      , stopwatch2 = 4
      , stopwatch2Delta = -1
      , finishTokens = 5
      , finishTokensDelta = 0
      , actual = 5
      }
    , { entryNumber = 2
      , stopwatch1 = 11
      , stopwatch1Delta = 0
      , stopwatch2 = 10
      , stopwatch2Delta = 0
      , finishTokens = 11
      , finishTokensDelta = 0
      , actual = 11
      }
    , { entryNumber = 3
      , stopwatch1 = 18
      , stopwatch1Delta = 0
      , stopwatch2 = 17
      , stopwatch2Delta = 0
      , finishTokens = 17
      , finishTokensDelta = -1
      , actual = 18
      }
    ]


sampleNumberCheckerDataIncremented : List AnnotatedNumberCheckerEntry
sampleNumberCheckerDataIncremented =
    [ { entryNumber = 1
      , stopwatch1 = 5
      , stopwatch1Delta = 0
      , stopwatch2 = 4
      , stopwatch2Delta = -1
      , finishTokens = 5
      , finishTokensDelta = 0
      , actual = 5
      }
    , { entryNumber = 2
      , stopwatch1 = 11
      , stopwatch1Delta = -1
      , stopwatch2 = 10
      , stopwatch2Delta = -1
      , finishTokens = 11
      , finishTokensDelta = -1
      , actual = 12
      }
    , { entryNumber = 3
      , stopwatch1 = 18
      , stopwatch1Delta = 0
      , stopwatch2 = 17
      , stopwatch2Delta = 0
      , finishTokens = 17
      , finishTokensDelta = -1
      , actual = 19
      }
    ]


sampleNumberCheckerDataDecremented : List AnnotatedNumberCheckerEntry
sampleNumberCheckerDataDecremented =
    [ { entryNumber = 1
      , stopwatch1 = 5
      , stopwatch1Delta = 0
      , stopwatch2 = 4
      , stopwatch2Delta = -1
      , finishTokens = 5
      , finishTokensDelta = 0
      , actual = 5
      }
    , { entryNumber = 2
      , stopwatch1 = 11
      , stopwatch1Delta = 1
      , stopwatch2 = 10
      , stopwatch2Delta = 1
      , finishTokens = 11
      , finishTokensDelta = 1
      , actual = 10
      }
    , { entryNumber = 3
      , stopwatch1 = 18
      , stopwatch1Delta = 0
      , stopwatch2 = 17
      , stopwatch2Delta = 0
      , finishTokens = 17
      , finishTokensDelta = -1
      , actual = 17
      }
    ]


sampleNumberCheckerDataWithSecondItemRemoved : List AnnotatedNumberCheckerEntry
sampleNumberCheckerDataWithSecondItemRemoved =
    [ { entryNumber = 1
      , stopwatch1 = 5
      , stopwatch1Delta = 0
      , stopwatch2 = 4
      , stopwatch2Delta = -1
      , finishTokens = 5
      , finishTokensDelta = 0
      , actual = 5
      }
    , { entryNumber = 2
      , stopwatch1 = 18
      , stopwatch1Delta = 0
      , stopwatch2 = 17
      , stopwatch2Delta = 0
      , finishTokens = 17
      , finishTokensDelta = -1
      , actual = 18
      }
    ]


createNumberCheckerManualEntryRow : Int -> Int -> Int -> NumberCheckerManualEntryRow
createNumberCheckerManualEntryRow stopwatch1 stopwatch2 finishTokens =
    NumberCheckerManualEntryRow (numericEntryFromInt stopwatch1) (numericEntryFromInt stopwatch2) (numericEntryFromInt finishTokens)


createBarcodeScannerDataFromFiles : List BarcodeScannerFile -> BarcodeScannerData
createBarcodeScannerDataFromFiles files =
    BarcodeScannerData files Dict.empty [] [] [] [] Nothing
        |> regenerate
