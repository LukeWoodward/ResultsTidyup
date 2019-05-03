module BarcodeScannerEditing exposing
    ( BarcodeScannerEditDetails(..)
    , BarcodeScannerFieldBeingEdited(..)
    , BarcodeScannerRowEditDetails
    , BarcodeScannerRowEditLocation
    , BarcodeScannerValidationError(..)
    , elementToFocusWhenOpening
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
    , athleteEntered : NumericEntry
    , finishPositionEntered : NumericEntry
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
            validate (BarcodeScannerRowEditDetails location contents (numericEntryFromAthleteNumber athlete) (numericEntryFromMaybeInt finishPosition) Both Nothing isDeleted)

        MisScan misScannedText ->
            validate (BarcodeScannerRowEditDetails location contents emptyNumericEntry emptyNumericEntry Neither Nothing isDeleted)


elementToFocusWhenOpening : LineContents -> String
elementToFocusWhenOpening contents =
    case contents of
        Ordinary _ _ ->
            "barcodeScannerEditAthlete"

        MisScan _ ->
            "athleteRadio"


updateEditDetails : BarcodeScannerEditDetails -> BarcodeScannerRowEditDetails -> BarcodeScannerRowEditDetails
updateEditDetails editDetails currentDetails =
    case editDetails of
        ChangeWhatsBeingEdited newEditedField ->
            if currentDetails.fieldBeingEdited == Both then
                currentDetails

            else
                case newEditedField of
                    AthleteOnly ->
                        validate { currentDetails | fieldBeingEdited = AthleteOnly }

                    FinishPositionOnly ->
                        validate { currentDetails | fieldBeingEdited = FinishPositionOnly }

                    Both ->
                        currentDetails

                    Neither ->
                        currentDetails

        AthleteChanged newAthlete ->
            validate { currentDetails | athleteEntered = numericEntryFromAthleteNumber newAthlete }

        FinishPositionChanged newFinishPosition ->
            validate { currentDetails | finishPositionEntered = numericEntryFromString newFinishPosition }


validate : BarcodeScannerRowEditDetails -> BarcodeScannerRowEditDetails
validate currentDetails =
    let
        validationError : Maybe BarcodeScannerValidationError
        validationError =
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
    in
    { currentDetails | validationError = validationError }
