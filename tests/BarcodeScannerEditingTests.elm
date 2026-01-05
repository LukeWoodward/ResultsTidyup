module BarcodeScannerEditingTests exposing (suite)

import BarcodeScanner exposing (BarcodeScannerData, BarcodeScannerFile, LineContents(..))
import BarcodeScannerEditing
    exposing
        ( BarcodeScannerEditDetails(..)
        , BarcodeScannerRowEditDetails
        , BarcodeScannerRowEditLocation
        , BarcodeScannerValidationError(..)
        , elementToFocusWhenOpening
        , isValidAthlete
        , isValidFinishPosition
        , startEditing
        , tryUpdateBarcodeScannerLine
        , updateEditDetails
        )
import Commands exposing (ElementToFocus(..))
import DataEntry exposing (IntegerEntry)
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestData exposing (createBarcodeScannerDataFromFiles, ordinaryFileLine)


initialDetails : BarcodeScannerRowEditDetails
initialDetails =
    BarcodeScannerRowEditDetails
        (BarcodeScannerRowEditLocation "file.txt" 3)
        (Ordinary "A229804" (Just 22))
        (IntegerEntry "A229804" (Just 229804))
        (IntegerEntry "22" (Just 22))
        Nothing
        False


validIntegerEntry : IntegerEntry
validIntegerEntry =
    IntegerEntry "944" (Just 944)


invalidIntegerEntry : IntegerEntry
invalidIntegerEntry =
    IntegerEntry "Invalid" Nothing


runValidationTest : Maybe BarcodeScannerValidationError -> BarcodeScannerRowEditDetails -> Expectation
runValidationTest validationError editDetails =
    BarcodeScannerEditing.validate editDetails
        |> Expect.equal validationError


suite : Test
suite =
    describe "BarcodeScannerEditing tests"
        [ describe "startEditing tests"
            [ test "Can start editing with a complete row" <|
                \() ->
                    startEditing (BarcodeScannerRowEditLocation "file.txt" 34) (Ordinary "A182095" (Just 47)) False
                        |> Expect.equal
                            (BarcodeScannerRowEditDetails
                                (BarcodeScannerRowEditLocation "file.txt" 34)
                                (Ordinary "A182095" (Just 47))
                                (IntegerEntry "A182095" (Just 182095))
                                (IntegerEntry "47" (Just 47))
                                Nothing
                                False
                            )
            ]
        , describe "elementToFocusWhenOpening tests"
            [ test "Focuses athlete input when opening to edit an ordinary item" <|
                \() ->
                    elementToFocusWhenOpening (Ordinary "A4580442" (Just 47))
                        |> Expect.equal BarcodeScannerEditingAthleteInput
            ]
        , describe "updateEditDetails tests"
            [ describe "AthleteChanged tests"
                [ test "Can change an athlete to a valid value" <|
                    \() ->
                        updateEditDetails (AthleteChanged "A769422") initialDetails
                            |> Expect.equal { initialDetails | athleteEntered = IntegerEntry "A769422" (Just 769422) }
                , test "Can change an athlete to a valid value lowercased" <|
                    \() ->
                        updateEditDetails (AthleteChanged "a769422") initialDetails
                            |> Expect.equal { initialDetails | athleteEntered = IntegerEntry "a769422" (Just 769422) }
                , test "Can change an athlete to an invalid value with no validation error" <|
                    \() ->
                        updateEditDetails (AthleteChanged "Invalid number") initialDetails
                            |> Expect.equal { initialDetails | athleteEntered = IntegerEntry "Invalid number" Nothing }
                ]
            , describe "FinishPositionChanged tests"
                [ test "Can change a finish position to a valid value" <|
                    \() ->
                        updateEditDetails (FinishPositionChanged "77") initialDetails
                            |> Expect.equal { initialDetails | finishPositionEntered = IntegerEntry "77" (Just 77) }
                , test "Can change an athlete to an invalid value with no validation error" <|
                    \() ->
                        updateEditDetails (FinishPositionChanged "Invalid number") initialDetails
                            |> Expect.equal { initialDetails | finishPositionEntered = IntegerEntry "Invalid number" Nothing }
                ]
            ]
        , describe "validate tests"
            [ test "Returns no validation error for both valid entries" <|
                \() ->
                    runValidationTest
                        Nothing
                        { initialDetails | athleteEntered = validIntegerEntry, finishPositionEntered = validIntegerEntry }
            , test "Returns validation error for invalid athlete and valid finish position" <|
                \() ->
                    runValidationTest
                        (Just InvalidAthleteNumber)
                        { initialDetails | athleteEntered = invalidIntegerEntry, finishPositionEntered = validIntegerEntry }
            , test "Returns validation error for valid athlete and invalid finish position" <|
                \() ->
                    runValidationTest
                        (Just InvalidFinishPosition)
                        { initialDetails | athleteEntered = validIntegerEntry, finishPositionEntered = invalidIntegerEntry }
            , test "Returns validation error for both invalid entries" <|
                \() ->
                    runValidationTest
                        (Just InvalidAthleteNumberAndFinishPosition)
                        { initialDetails | athleteEntered = invalidIntegerEntry, finishPositionEntered = invalidIntegerEntry }
            ]
        , describe "isValidAthlete tests"
            [ test "Athlete not valid if validation error for the athlete" <|
                \() ->
                    isValidAthlete { initialDetails | validationError = Just InvalidAthleteNumber }
                        |> Expect.equal False
            , test "Athlete valid if validation error for the finish position" <|
                \() ->
                    isValidAthlete { initialDetails | validationError = Just InvalidFinishPosition }
                        |> Expect.equal True
            , test "Athlete not valid if validation error for both the athlete and the finish position" <|
                \() ->
                    isValidAthlete { initialDetails | validationError = Just InvalidAthleteNumberAndFinishPosition }
                        |> Expect.equal False
            , test "Athlete valid if no validation error" <|
                \() ->
                    isValidAthlete initialDetails
                        |> Expect.equal True
            ]
        , describe "isValidFinishPosition tests"
            [ test "Finish position valid if validation error for the athlete" <|
                \() ->
                    isValidFinishPosition { initialDetails | validationError = Just InvalidAthleteNumber }
                        |> Expect.equal True
            , test "Finish position not valid if validation error for the finish position" <|
                \() ->
                    isValidFinishPosition { initialDetails | validationError = Just InvalidFinishPosition }
                        |> Expect.equal False
            , test "Finish position not valid if validation error for both the athlete and the finish position" <|
                \() ->
                    isValidFinishPosition { initialDetails | validationError = Just InvalidAthleteNumberAndFinishPosition }
                        |> Expect.equal False
            , test "Finish position valid if no validation error" <|
                \() ->
                    isValidFinishPosition initialDetails
                        |> Expect.equal True
            ]
        , describe "tryUpdateBarcodeScannerLine tests"
            [ test "Updating barcode scanner line when successful returns success" <|
                \() ->
                    let
                        initialBarcodeScannerData : BarcodeScannerData
                        initialBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    "Name6"
                                    [ ordinaryFileLine 1 "A4580442" (Just 47) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                ]

                        expectedBarcodeScannerData : BarcodeScannerData
                        expectedBarcodeScannerData =
                            createBarcodeScannerDataFromFiles
                                [ BarcodeScannerFile
                                    "barcodes6.txt"
                                    "Name6"
                                    [ ordinaryFileLine 1 "A2022807" (Just 37) "14/03/2018 09:47:03"
                                    , ordinaryFileLine 2 "A1866207" (Just 58) "14/03/2018 09:48:44"
                                    ]
                                ]

                        rowEditDetails : BarcodeScannerRowEditDetails
                        rowEditDetails =
                            BarcodeScannerRowEditDetails
                                (BarcodeScannerRowEditLocation "barcodes6.txt" 1)
                                (Ordinary "A4580442" (Just 47))
                                (IntegerEntry "A2022807" (Just 2022807))
                                (IntegerEntry "37" (Just 37))
                                Nothing
                                False
                    in
                    tryUpdateBarcodeScannerLine rowEditDetails initialBarcodeScannerData
                        |> Expect.equal (Ok expectedBarcodeScannerData)
            , test "Updating barcode scanner line when unsuccessful returns validation error" <|
                \() ->
                    tryUpdateBarcodeScannerLine { initialDetails | athleteEntered = invalidIntegerEntry } BarcodeScanner.empty
                        |> Expect.equal (Err InvalidAthleteNumber)
            ]
        ]
