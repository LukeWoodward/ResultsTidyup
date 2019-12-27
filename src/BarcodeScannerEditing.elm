module BarcodeScannerEditing exposing
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
    , validate
    )

import BarcodeScanner exposing (BarcodeScannerData, BarcodeScannerFileLine, DeletionStatus(..), LineContents(..), updateBarcodeScannerLine)
import Commands exposing (ElementToFocus(..))
import NumericEntry
    exposing
        ( IntegerEntry
        , emptyIntegerEntry
        , integerEntryFromAthleteNumber
        , integerEntryFromMaybeInt
        , integerEntryFromString
        , isValidEntry
        )


type alias BarcodeScannerRowEditLocation =
    { fileName : String
    , lineNumber : Int
    }


type BarcodeScannerFieldBeingEdited
    = Neither
    | AthleteOnly
    | FinishPositionOnly
    | Both


type BarcodeScannerValidationError
    = InvalidAthleteNumber
    | InvalidFinishPosition
    | InvalidAthleteNumberAndFinishPosition
    | NeitherSelected


type alias BarcodeScannerRowEditDetails =
    { location : BarcodeScannerRowEditLocation
    , currentContents : LineContents
    , athleteEntered : IntegerEntry
    , finishPositionEntered : IntegerEntry
    , fieldBeingEdited : BarcodeScannerFieldBeingEdited
    , validationError : Maybe BarcodeScannerValidationError
    , isDeleted : Bool
    }


type BarcodeScannerEditDetails
    = ChangeWhatsBeingEdited BarcodeScannerFieldBeingEdited
    | AthleteChanged String
    | FinishPositionChanged String


startEditing : BarcodeScannerRowEditLocation -> LineContents -> Bool -> BarcodeScannerRowEditDetails
startEditing location contents isDeleted =
    case contents of
        Ordinary athlete finishPosition ->
            BarcodeScannerRowEditDetails location contents (integerEntryFromAthleteNumber athlete) (integerEntryFromMaybeInt finishPosition) Both Nothing isDeleted

        MisScan misScannedText ->
            BarcodeScannerRowEditDetails location contents emptyIntegerEntry emptyIntegerEntry Neither Nothing isDeleted


elementToFocusWhenOpening : LineContents -> ElementToFocus
elementToFocusWhenOpening contents =
    case contents of
        Ordinary _ _ ->
            BarcodeScannerEditingAthleteInput

        MisScan _ ->
            BarcodeScannerEditingAthleteRadioButton


updateEditDetails : BarcodeScannerEditDetails -> BarcodeScannerRowEditDetails -> BarcodeScannerRowEditDetails
updateEditDetails editDetails currentDetails =
    let
        updatedDetails : BarcodeScannerRowEditDetails
        updatedDetails =
            case editDetails of
                ChangeWhatsBeingEdited newEditedField ->
                    if currentDetails.fieldBeingEdited == Both then
                        currentDetails

                    else
                        case newEditedField of
                            AthleteOnly ->
                                { currentDetails | fieldBeingEdited = AthleteOnly }

                            FinishPositionOnly ->
                                { currentDetails | fieldBeingEdited = FinishPositionOnly }

                            Both ->
                                currentDetails

                            Neither ->
                                currentDetails

                AthleteChanged newAthlete ->
                    { currentDetails | athleteEntered = integerEntryFromAthleteNumber newAthlete }

                FinishPositionChanged newFinishPosition ->
                    { currentDetails | finishPositionEntered = integerEntryFromString newFinishPosition }
    in
    { updatedDetails | validationError = Nothing }


isValidAthlete : BarcodeScannerRowEditDetails -> Bool
isValidAthlete rowEditDetails =
    case rowEditDetails.validationError of
        Just InvalidAthleteNumber ->
            False

        Just InvalidFinishPosition ->
            True

        Just InvalidAthleteNumberAndFinishPosition ->
            False

        Just NeitherSelected ->
            True

        Nothing ->
            True


isValidFinishPosition : BarcodeScannerRowEditDetails -> Bool
isValidFinishPosition rowEditDetails =
    case rowEditDetails.validationError of
        Just InvalidAthleteNumber ->
            True

        Just InvalidFinishPosition ->
            False

        Just InvalidAthleteNumberAndFinishPosition ->
            False

        Just NeitherSelected ->
            True

        Nothing ->
            True


validate : BarcodeScannerRowEditDetails -> Maybe BarcodeScannerValidationError
validate currentDetails =
    case currentDetails.fieldBeingEdited of
        Neither ->
            Just NeitherSelected

        AthleteOnly ->
            if isValidEntry currentDetails.athleteEntered then
                Nothing

            else
                Just InvalidAthleteNumber

        FinishPositionOnly ->
            if isValidEntry currentDetails.finishPositionEntered then
                Nothing

            else
                Just InvalidFinishPosition

        Both ->
            case ( isValidEntry currentDetails.athleteEntered, isValidEntry currentDetails.finishPositionEntered ) of
                ( False, False ) ->
                    Just InvalidAthleteNumberAndFinishPosition

                ( False, True ) ->
                    Just InvalidAthleteNumber

                ( True, False ) ->
                    Just InvalidFinishPosition

                ( True, True ) ->
                    Nothing


tryUpdateBarcodeScannerLine : BarcodeScannerRowEditDetails -> BarcodeScannerData -> Result BarcodeScannerValidationError BarcodeScannerData
tryUpdateBarcodeScannerLine rowEditDetails barcodeScannerData =
    let
        validationErrorMaybe : Maybe BarcodeScannerValidationError
        validationErrorMaybe =
            validate rowEditDetails
    in
    case validationErrorMaybe of
        Just validationError ->
            Err validationError

        Nothing ->
            let
                athlete : String
                athlete =
                    case rowEditDetails.athleteEntered.parsedValue of
                        Just athleteNum ->
                            "A" ++ String.fromInt athleteNum

                        Nothing ->
                            ""
            in
            updateBarcodeScannerLine
                rowEditDetails.location.fileName
                rowEditDetails.location.lineNumber
                athlete
                rowEditDetails.finishPositionEntered.parsedValue
                barcodeScannerData
                |> Ok
