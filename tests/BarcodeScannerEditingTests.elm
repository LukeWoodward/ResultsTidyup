module BarcodeScannerEditingTests exposing (suite)

import BarcodeScanner exposing (BarcodeScannerData, BarcodeScannerFile, LineContents(..))
import BarcodeScannerEditing
    exposing
        ( BarcodeScannerEditDetails(..)
        , BarcodeScannerFieldBeingEdited(..)
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
import Expect exposing (Expectation)
import NumericEntry exposing (IntegerEntry)
import Test exposing (Test, describe, test)
import TestData exposing (createBarcodeScannerDataFromFiles, ordinaryFileLine, toPosix)


initialDetails : BarcodeScannerRowEditDetails
initialDetails =
    BarcodeScannerRowEditDetails
        (BarcodeScannerRowEditLocation "file.txt" 3)
        (Ordinary "A229804" (Just 22))
        (IntegerEntry "A229804" (Just 229804))
        (IntegerEntry "22" (Just 22))
        Both
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
                                Both
                                Nothing
                                False
                            )
            , test "Can start editing with a mis-scanned item" <|
                \() ->
                    startEditing (BarcodeScannerRowEditLocation "file.txt" 51) (MisScan "d&084") False
                        |> Expect.equal
                            (BarcodeScannerRowEditDetails
                                (BarcodeScannerRowEditLocation "file.txt" 51)
                                (MisScan "d&084")
                                (IntegerEntry "" Nothing)
                                (IntegerEntry "" Nothing)
                                Neither
                                Nothing
                                False
                            )
            ]
        , describe "elementToFocusWhenOpening tests"
            [ test "Focuses athlete input when opening to edit an ordinary item" <|
                \() ->
                    elementToFocusWhenOpening (Ordinary "A4580442" (Just 47))
                        |> Expect.equal BarcodeScannerEditingAthleteInput
            , test "Focuses radio button when opening to edit a mis-scanned item" <|
                \() ->
                    elementToFocusWhenOpening (MisScan "&d084")
                        |> Expect.equal BarcodeScannerEditingAthleteRadioButton
            ]
        , describe "updateEditDetails tests"
            [ describe "ChangeWhatsBeingEdited tests"
                [ test "Attempting to change what's being edited when both selected has no effect" <|
                    \() ->
                        let
                            fields : List BarcodeScannerFieldBeingEdited
                            fields =
                                [ Neither, AthleteOnly, FinishPositionOnly, Both ]
                        in
                        fields
                            |> List.map (\field -> updateEditDetails (ChangeWhatsBeingEdited field) initialDetails)
                            |> Expect.equal (List.repeat 4 initialDetails)
                , test "Can change whats being edited to a single field" <|
                    \() ->
                        let
                            fields : List ( BarcodeScannerFieldBeingEdited, BarcodeScannerFieldBeingEdited )
                            fields =
                                [ ( Neither, AthleteOnly )
                                , ( AthleteOnly, AthleteOnly )
                                , ( FinishPositionOnly, AthleteOnly )
                                , ( Neither, FinishPositionOnly )
                                , ( AthleteOnly, FinishPositionOnly )
                                , ( FinishPositionOnly, FinishPositionOnly )
                                ]

                            actualValues : List BarcodeScannerRowEditDetails
                            actualValues =
                                List.map
                                    (\( oldField, newField ) ->
                                        { initialDetails | fieldBeingEdited = oldField }
                                            |> updateEditDetails (ChangeWhatsBeingEdited newField)
                                    )
                                    fields

                            expectedValues : List BarcodeScannerRowEditDetails
                            expectedValues =
                                List.map
                                    (\( _, newField ) ->
                                        { initialDetails | fieldBeingEdited = newField }
                                    )
                                    fields
                        in
                        Expect.equal expectedValues actualValues
                , test "Cannot change whats being edited to Neither or Both" <|
                    \() ->
                        let
                            fields : List ( BarcodeScannerFieldBeingEdited, BarcodeScannerFieldBeingEdited )
                            fields =
                                [ ( Neither, Neither )
                                , ( AthleteOnly, Neither )
                                , ( FinishPositionOnly, Neither )
                                , ( Neither, Both )
                                , ( AthleteOnly, Both )
                                , ( FinishPositionOnly, Both )
                                ]

                            actualValues : List BarcodeScannerRowEditDetails
                            actualValues =
                                List.map
                                    (\( oldField, newField ) ->
                                        { initialDetails | fieldBeingEdited = oldField }
                                            |> updateEditDetails (ChangeWhatsBeingEdited newField)
                                    )
                                    fields

                            expectedValues : List BarcodeScannerRowEditDetails
                            expectedValues =
                                List.map
                                    (\( oldField, _ ) ->
                                        { initialDetails | fieldBeingEdited = oldField }
                                    )
                                    fields
                        in
                        Expect.equal expectedValues actualValues
                ]
            , describe "AthleteChanged tests"
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
            [ test "Returns validation error for neither selected" <|
                \() ->
                    runValidationTest (Just NeitherSelected) { initialDetails | fieldBeingEdited = Neither }
            , test "Returns no validation error for athlete selected and valid entry" <|
                \() ->
                    runValidationTest Nothing { initialDetails | fieldBeingEdited = AthleteOnly, athleteEntered = validIntegerEntry }
            , test "Returns validation error for athlete selected and invalid entry" <|
                \() ->
                    runValidationTest (Just InvalidAthleteNumber) { initialDetails | fieldBeingEdited = AthleteOnly, athleteEntered = invalidIntegerEntry }
            , test "Returns no validation error for finish position selected and valid entry" <|
                \() ->
                    runValidationTest Nothing { initialDetails | fieldBeingEdited = FinishPositionOnly, finishPositionEntered = validIntegerEntry }
            , test "Returns validation error for finish position selected and invalid entry" <|
                \() ->
                    runValidationTest
                        (Just InvalidFinishPosition)
                        { initialDetails | fieldBeingEdited = FinishPositionOnly, finishPositionEntered = invalidIntegerEntry }
            , test "Returns no validation error for both selected and both valid entries" <|
                \() ->
                    runValidationTest
                        Nothing
                        { initialDetails | fieldBeingEdited = Both, athleteEntered = validIntegerEntry, finishPositionEntered = validIntegerEntry }
            , test "Returns validation error for both selected, invalid athlete and valid finish position" <|
                \() ->
                    runValidationTest
                        (Just InvalidAthleteNumber)
                        { initialDetails | fieldBeingEdited = Both, athleteEntered = invalidIntegerEntry, finishPositionEntered = validIntegerEntry }
            , test "Returns validation error for both selected, valid athlete and invalid finish position" <|
                \() ->
                    runValidationTest
                        (Just InvalidFinishPosition)
                        { initialDetails | fieldBeingEdited = Both, athleteEntered = validIntegerEntry, finishPositionEntered = invalidIntegerEntry }
            , test "Returns validation error for both selected and both invalid entries" <|
                \() ->
                    runValidationTest
                        (Just InvalidAthleteNumberAndFinishPosition)
                        { initialDetails | fieldBeingEdited = Both, athleteEntered = invalidIntegerEntry, finishPositionEntered = invalidIntegerEntry }
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
            , test "Athlete valid if validation error for neither selected" <|
                \() ->
                    isValidAthlete { initialDetails | validationError = Just NeitherSelected }
                        |> Expect.equal True
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
            , test "Finish position valid if validation error for neither selected" <|
                \() ->
                    isValidFinishPosition { initialDetails | validationError = Just NeitherSelected }
                        |> Expect.equal True
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

                        rowEditDetails : BarcodeScannerRowEditDetails
                        rowEditDetails =
                            BarcodeScannerRowEditDetails
                                (BarcodeScannerRowEditLocation "barcodes6.txt" 1)
                                (Ordinary "A4580442" (Just 47))
                                (IntegerEntry "A2022807" (Just 2022807))
                                (IntegerEntry "37" (Just 37))
                                Both
                                Nothing
                                False
                    in
                    tryUpdateBarcodeScannerLine rowEditDetails initialBarcodeScannerData
                        |> Expect.equal (Ok expectedBarcodeScannerData)
            , test "Updating barcode scanner line when unsuccessful returns validation error" <|
                \() ->
                    tryUpdateBarcodeScannerLine { initialDetails | fieldBeingEdited = Neither } BarcodeScanner.empty
                        |> Expect.equal (Err NeitherSelected)
            ]
        ]
