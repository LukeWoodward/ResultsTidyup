module ProblemsTests exposing (suite)

import BarcodeScanner exposing (BarcodeScannerData, UnrecognisedLine)
import BarcodeScannerTests exposing (createBarcodeScannerData, toPosix)
import DataStructures exposing (EventDateAndTime)
import Dict exposing (Dict)
import Errors exposing (expectError)
import Expect
import MergedTable exposing (Stopwatches(..))
import Problems exposing (MinorProblem(..), Problem(..), ProblemsContainer, identifyProblems)
import Test exposing (Test, describe, test)


emptyEventDateAndTime : EventDateAndTime
emptyEventDateAndTime =
    EventDateAndTime "" Nothing "" Nothing


exampleEventDateAndTime : EventDateAndTime
exampleEventDateAndTime =
    EventDateAndTime "14/03/2018" (toPosix "2018-03-14T00:00:00.000Z") "09:00" (Just (9 * 60))


lateEventDateAndTime : EventDateAndTime
lateEventDateAndTime =
    EventDateAndTime "14/03/2018" (toPosix "2018-03-14T00:00:00.000Z") "10:00" (Just (10 * 60))


{-| 2018-03-14T09:00:00
-}
baseEventStartTime : Int
baseEventStartTime =
    1521018000000


suite : Test
suite =
    describe "Problems tests"
        [ describe "identifyProblems tests"
            [ test "identifyProblems returns no problems for no data" <|
                \() ->
                    identifyProblems None BarcodeScanner.empty emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [])
            , test "identifyProblems returns no problems for a single athlete with a single position" <|
                \() ->
                    identifyProblems None (createBarcodeScannerData (Dict.singleton 12 [ "A123456" ]) [] []) emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [])
            , test "identifyProblems returns no problems for three athletes with three different positions" <|
                \() ->
                    identifyProblems None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [])
            , test "identifyProblems returns a problem for an athlete with two repeated positions" <|
                \() ->
                    identifyProblems None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ AthleteWithMultiplePositions "A123456" [ 12, 19 ] ] [])
            , test "identifyProblems returns a problem for an athlete with three repeated positions" <|
                \() ->
                    identifyProblems None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A123456" ] ), ( 19, [ "A123456" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ AthleteWithMultiplePositions "A123456" [ 12, 16, 19 ] ] [])
            , test "identifyProblems returns two problems for two athletes with repeated positions" <|
                \() ->
                    identifyProblems None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A123456" ] ), ( 25, [ "A252525" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ AthleteWithMultiplePositions "A123456" [ 12, 19 ], AthleteWithMultiplePositions "A252525" [ 16, 25 ] ] [])
            , test "identifyProblems returns a problem for a position with two athletes" <|
                \() ->
                    identifyProblems None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionWithMultipleAthletes 12 [ "A123456", "A252525" ] ] [])
            , test "identifyProblems returns a problem for a position with three athletes" <|
                \() ->
                    identifyProblems None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525", "A748159" ] ), ( 19, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionWithMultipleAthletes 12 [ "A123456", "A252525", "A748159" ] ] [])
            , test "identifyProblems returns two problems for two positions with two athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456", "A252525" ] ), ( 19, [ "A987654", "A748159" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionWithMultipleAthletes 12 [ "A123456", "A252525" ], PositionWithMultipleAthletes 19 [ "A987654", "A748159" ] ] [])
            , test "identifyProblems returns no problems for a finish position not off the end" <|
                \() ->
                    identifyProblems
                        (Single "filename" [ 1000, 1100, 1200, 1300, 1400 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [])
            , test "identifyProblems returns a problem for a finish position off the end" <|
                \() ->
                    identifyProblems
                        (Single "filename" [ 1000, 1100, 1200, 1300 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionOffEndOfTimes 4 5 ] [])
            , test "identifyProblems returns a single problem for multiple finish positions off the end" <|
                \() ->
                    identifyProblems
                        (Single "filename" [ 1000, 1100 ])
                        (createBarcodeScannerData (Dict.fromList [ ( 3, [ "A123456" ] ), ( 4, [ "A252525" ] ), ( 5, [ "A987654" ] ) ]) [] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionOffEndOfTimes 2 5 ] [])
            , test "identifyProblems returns a single problem for an athlete with no position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A951623" ] [])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ AthleteMissingPosition "A951623" ] [])
            , test "identifyProblems returns multiple problems for multiple athletes with no position" <|
                \() ->
                    identifyProblems None (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [ "A321456", "A951623" ] []) emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ AthleteMissingPosition "A321456", AthleteMissingPosition "A951623" ] [])
            , test "identifyProblems returns a single problem for a position with no athlete" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [ 15 ])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionMissingAthlete 15 ] [])
            , test "identifyProblems returns multiple problems for positions with no athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ]) [] [ 15, 18, 26 ])
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionMissingAthlete 15, PositionMissingAthlete 18, PositionMissingAthlete 26 ] [])
            , test "identifyProblems returns a problem for an unrecognised barcode-scanner line" <|
                \() ->
                    identifyProblems
                        None
                        (BarcodeScannerData Dict.empty [] [] [ UnrecognisedLine "This is not a valid line" "code" "message" ] Nothing)
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ UnrecognisedBarcodeScannerLine "This is not a valid line" ] [])
            , test "identifyProblems returns a minor problem for the same athlete with the same finish position twice" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525", "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            []
                            []
                        )
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [ AthleteInSamePositionMultipleTimes "A252525" 16 ])
            , test "identifyProblems returns two minor problems for two athletes with the same finish position twice" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525", "A252525" ] ), ( 19, [ "A987654", "A987654" ] ) ])
                            []
                            []
                        )
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [ AthleteInSamePositionMultipleTimes "A252525" 16, AthleteInSamePositionMultipleTimes "A987654" 19 ])
            , test "identifyProblems returns a minor problem for an athlete with a position and with a missing position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            [ "A252525" ]
                            []
                        )
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [ AthleteWithAndWithoutPosition "A252525" 16 ])
            , test "identifyProblems returns two minor problems for two athletes with and without a position" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            [ "A252525", "A987654" ]
                            []
                        )
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [ AthleteWithAndWithoutPosition "A252525" 16, AthleteWithAndWithoutPosition "A987654" 19 ])
            , test "identifyProblems returns a minor problem for a position with and without an athlete" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            []
                            [ 19 ]
                        )
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [ PositionWithAndWithoutAthlete 19 "A987654" ])
            , test "identifyProblems returns two minor problems for two positions with and without athletes" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.fromList [ ( 12, [ "A123456" ] ), ( 16, [ "A252525" ] ), ( 19, [ "A987654" ] ) ])
                            []
                            [ 19, 12 ]
                        )
                        emptyEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [ PositionWithAndWithoutAthlete 19 "A987654", PositionWithAndWithoutAthlete 12 "A123456" ])
            , test "identifyProblems returns no problems for scanned barcodes with their scan time after the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.singleton 12 [ "A123456" ])
                            []
                            []
                        )
                        exampleEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [])
            , test "identifyProblems returns a minor problem for scanned barcodes with their scan time before the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            (Dict.singleton 12 [ "A123456" ])
                            []
                            []
                        )
                        lateEventDateAndTime
                        |> Expect.equal (ProblemsContainer [] [ BarcodesScannedBeforeEventStart 1 (baseEventStartTime + 60 * 60 * 1000) "14/03/2018 10:00" ])
            , test "identifyProblems returns no problems for athletes with no finish tokens but with their scan time after the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            Dict.empty
                            [ "A123456" ]
                            []
                        )
                        exampleEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ AthleteMissingPosition "A123456" ] [])
            , test "identifyProblems returns a minor problem for athletes with no finish tokens but with their scan time before the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            Dict.empty
                            [ "A123456" ]
                            []
                        )
                        lateEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ AthleteMissingPosition "A123456" ] [ BarcodesScannedBeforeEventStart 1 (baseEventStartTime + 60 * 60 * 1000) "14/03/2018 10:00" ])
            , test "identifyProblems returns no problems for finish tokens with no associated athletes but with their scan time after the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            Dict.empty
                            []
                            [ 19 ]
                        )
                        exampleEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionMissingAthlete 19 ] [])
            , test "identifyProblems returns a minor problem for finish tokens with no associated athletes but with their scan time before the event start" <|
                \() ->
                    identifyProblems
                        None
                        (createBarcodeScannerData
                            Dict.empty
                            []
                            [ 19 ]
                        )
                        lateEventDateAndTime
                        |> Expect.equal (ProblemsContainer [ PositionMissingAthlete 19 ] [ BarcodesScannedBeforeEventStart 1 (baseEventStartTime + 60 * 60 * 1000) "14/03/2018 10:00" ])
            ]
        ]
