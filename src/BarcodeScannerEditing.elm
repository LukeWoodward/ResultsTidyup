module BarcodeScannerEditing exposing
    ( BarcodeScannerEditDetails(..)
    , BarcodeScannerFieldBeingEdited(..)
    , BarcodeScannerRowEditDetails
    , BarcodeScannerRowEditLocation
    , BarcodeScannerValidationError(..)
    , startEditing
    , updateEditDetails
    , validate
    )

import BarcodeScanner exposing (LineContents(..))
import NumericEntry
    exposing
        ( NumericEntry
        , emptyNumericEntry
        , isValidEntry
        , numericEntryFromAthleteNumber
        , numericEntryFromMaybeInt
        , numericEntryFromString
        )


type alias BarcodeScannerRowEditLocation =
    { fileNumber : Int
    , lineNumber : Int
    }


type BarcodeScannerFieldBeingEdited
    = Neither
    | AthleteOnly
    | FinishPositionOnly
    | Both


type alias BarcodeScannerRowEditDetails =
    { location : BarcodeScannerRowEditLocation
    , currentContents : LineContents
    , athleteEntered : NumericEntry
    , finishPositionEntered : NumericEntry
    , fieldBeingEdited : BarcodeScannerFieldBeingEdited
    }


type BarcodeScannerEditDetails
    = ChangeWhatsBeingEdited BarcodeScannerFieldBeingEdited
    | AthleteChanged String
    | FinishPositionChanged String


type BarcodeScannerValidationError
    = InvalidAthleteNumber
    | InvalidFinishPosition
    | InvalidAthleteNumberAndFinishPosition
    | NeitherSelected


startEditing : BarcodeScannerRowEditLocation -> LineContents -> BarcodeScannerRowEditDetails
startEditing location contents =
    case contents of
        Ordinary athlete finishPosition ->
            BarcodeScannerRowEditDetails location contents (numericEntryFromAthleteNumber athlete) (numericEntryFromMaybeInt finishPosition) Both

        MisScan misScannedText ->
            BarcodeScannerRowEditDetails location contents emptyNumericEntry emptyNumericEntry Neither


updateEditDetails : BarcodeScannerEditDetails -> BarcodeScannerRowEditDetails -> BarcodeScannerRowEditDetails
updateEditDetails editDetails currentDetails =
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
            { currentDetails | athleteEntered = numericEntryFromAthleteNumber newAthlete }

        FinishPositionChanged newFinishPosition ->
            { currentDetails | finishPositionEntered = numericEntryFromString newFinishPosition }


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
