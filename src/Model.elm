module Model exposing
    ( DialogDetails(..)
    , Model
    , NumberCheckerManualEntryRow
    , emptyNumberCheckerManualEntryRow
    , initModel
    )

import BarcodeScanner exposing (BarcodeScannerData, LineContents)
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails, BarcodeScannerRowEditLocation)
import Bootstrap.Tab as Tab
import DataEntry exposing (IntegerEntry, emptyEntry)
import Error exposing (FileError)
import EventDateAndTime exposing (EventDateAndTime)
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import Problems exposing (Problems)
import Stopwatch exposing (Stopwatches(..))
import Time exposing (Posix)
import TokenOperations exposing (TokenOperationEditDetails)


type alias NumberCheckerManualEntryRow =
    { stopwatch1 : IntegerEntry
    , stopwatch2 : IntegerEntry
    , finishTokens : IntegerEntry
    }


emptyNumberCheckerManualEntryRow : NumberCheckerManualEntryRow
emptyNumberCheckerManualEntryRow =
    NumberCheckerManualEntryRow emptyEntry emptyEntry emptyEntry


type DialogDetails
    = NoDialog
    | BarcodeScannerRowEditDialog BarcodeScannerRowEditDetails
    | TokenOperationsDialog TokenOperationEditDetails


type alias Model =
    { isBeta : Bool
    , stopwatches : Stopwatches
    , lastErrors : List FileError
    , numberCheckerEntries : List AnnotatedNumberCheckerEntry
    , lastHeight : Maybe Int
    , highlightedNumberCheckerId : Maybe Int
    , barcodeScannerData : BarcodeScannerData
    , problems : Problems
    , eventDateAndTime : EventDateAndTime
    , numberCheckerManualEntryRow : NumberCheckerManualEntryRow
    , secondTab : Tab.State
    , barcodeScannerTab : Tab.State
    , dialogDetails : DialogDetails
    }


initModel : Model
initModel =
    { isBeta = False
    , stopwatches = None
    , lastErrors = []
    , numberCheckerEntries = []
    , lastHeight = Nothing
    , highlightedNumberCheckerId = Nothing
    , barcodeScannerData = BarcodeScanner.empty
    , problems = Problems.noProblems
    , eventDateAndTime = EventDateAndTime emptyEntry emptyEntry
    , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
    , secondTab = Tab.initialState
    , barcodeScannerTab = Tab.initialState
    , dialogDetails = NoDialog
    }
