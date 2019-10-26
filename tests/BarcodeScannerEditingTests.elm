module BarcodeScannerEditingTests exposing (suite)

import BarcodeScanner exposing (LineContents(..))
import BarcodeScannerEditing
    exposing
        ( BarcodeScannerEditDetails(..)
        , BarcodeScannerFieldBeingEdited(..)
        , BarcodeScannerRowEditDetails
        , BarcodeScannerRowEditLocation
        , BarcodeScannerValidationError(..)
        , elementToFocusWhenOpening
        , startEditing
        , updateEditDetails
        )
import Commands exposing (ElementToFocus(..))
import Expect exposing (Expectation)
import NumericEntry exposing (NumericEntry)
import Test exposing (Test, describe, test)


initialDetails : BarcodeScannerRowEditDetails
initialDetails =
    BarcodeScannerRowEditDetails
        (BarcodeScannerRowEditLocation "file.txt" 3)
        (Ordinary "A229804" (Just 22))
        (NumericEntry "A229804" (Just 229804))
        (NumericEntry "22" (Just 22))
        Both
        Nothing
        False


validNumericEntry : NumericEntry
validNumericEntry =
    NumericEntry "944" (Just 944)


invalidNumericEntry : NumericEntry
invalidNumericEntry =
    NumericEntry "Invalid" Nothing


runValidationTest : Maybe BarcodeScannerValidationError -> BarcodeScannerRowEditDetails -> Expectation
runValidationTest validationError editDetails =
    BarcodeScannerEditing.validate editDetails
        |> Expect.equal { editDetails | validationError = validationError }


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
                                (NumericEntry "A182095" (Just 182095))
                                (NumericEntry "47" (Just 47))
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
                                (NumericEntry "" Nothing)
                                (NumericEntry "" Nothing)
                                Neither
                                (Just NeitherSelected)
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
                            |> Expect.equal { initialDetails | athleteEntered = NumericEntry "A769422" (Just 769422) }
                , test "Can change an athlete to a valid value lowercased" <|
                    \() ->
                        updateEditDetails (AthleteChanged "a769422") initialDetails
                            |> Expect.equal { initialDetails | athleteEntered = NumericEntry "a769422" (Just 769422) }
                , test "Can change an athlete to an invalid value" <|
                    \() ->
                        updateEditDetails (AthleteChanged "Invalid number") initialDetails
                            |> Expect.equal
                                { initialDetails
                                    | athleteEntered = NumericEntry "Invalid number" Nothing
                                    , validationError = Just InvalidAthleteNumber
                                }
                ]
            , describe "FinishPositionChanged tests"
                [ test "Can change a finish position to a valid value" <|
                    \() ->
                        updateEditDetails (FinishPositionChanged "77") initialDetails
                            |> Expect.equal { initialDetails | finishPositionEntered = NumericEntry "77" (Just 77) }
                , test "Can change an athlete to an invalid value" <|
                    \() ->
                        updateEditDetails (FinishPositionChanged "Invalid number") initialDetails
                            |> Expect.equal
                                { initialDetails
                                    | finishPositionEntered = NumericEntry "Invalid number" Nothing
                                    , validationError = Just InvalidFinishPosition
                                }
                ]
            ]
        , describe "validate tests"
            [ test "Returns validation error for neither selected" <|
                \() ->
                    runValidationTest (Just NeitherSelected) { initialDetails | fieldBeingEdited = Neither }
            , test "Returns no validation error for athlete selected and valid entry" <|
                \() ->
                    runValidationTest Nothing { initialDetails | fieldBeingEdited = AthleteOnly, athleteEntered = validNumericEntry }
            , test "Returns validation error for athlete selected and invalid entry" <|
                \() ->
                    runValidationTest (Just InvalidAthleteNumber) { initialDetails | fieldBeingEdited = AthleteOnly, athleteEntered = invalidNumericEntry }
            , test "Returns no validation error for finish position selected and valid entry" <|
                \() ->
                    runValidationTest Nothing { initialDetails | fieldBeingEdited = FinishPositionOnly, finishPositionEntered = validNumericEntry }
            , test "Returns validation error for finish position selected and invalid entry" <|
                \() ->
                    runValidationTest
                        (Just InvalidFinishPosition)
                        { initialDetails | fieldBeingEdited = FinishPositionOnly, finishPositionEntered = invalidNumericEntry }
            , test "Returns no validation error for both selected and both valid entries" <|
                \() ->
                    runValidationTest
                        Nothing
                        { initialDetails | fieldBeingEdited = Both, athleteEntered = validNumericEntry, finishPositionEntered = validNumericEntry }
            , test "Returns validation error for both selected, invalid athlete and valid finish position" <|
                \() ->
                    runValidationTest
                        (Just InvalidAthleteNumber)
                        { initialDetails | fieldBeingEdited = Both, athleteEntered = invalidNumericEntry, finishPositionEntered = validNumericEntry }
            , test "Returns validation error for both selected, valid athlete and invalid finish position" <|
                \() ->
                    runValidationTest
                        (Just InvalidFinishPosition)
                        { initialDetails | fieldBeingEdited = Both, athleteEntered = validNumericEntry, finishPositionEntered = invalidNumericEntry }
            , test "Returns validation error for both selected and both invalid entries" <|
                \() ->
                    runValidationTest
                        (Just InvalidAthleteNumberAndFinishPosition)
                        { initialDetails | fieldBeingEdited = Both, athleteEntered = invalidNumericEntry, finishPositionEntered = invalidNumericEntry }
            ]
        ]
