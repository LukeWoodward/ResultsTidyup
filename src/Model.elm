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
import PastedFile exposing (PastedFileDetails)
import Problems exposing (IgnoredProblems, Problems)
import Stopwatch exposing (Stopwatches(..))
import StopwatchOperations exposing (StopwatchOperationEditDetails)
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
    | StopwatchOperationsDialog StopwatchOperationEditDetails
    | TokenOperationsDialog TokenOperationEditDetails
    | PasteFileDialog PastedFileDetails


type alias Model =
    { isBeta : Bool
    , stopwatches : Stopwatches
    , lastErrors : List FileError
    , numberCheckerEntries : List AnnotatedNumberCheckerEntry
    , highlightedNumberCheckerId : Maybe Int
    , barcodeScannerData : BarcodeScannerData
    , problems : Problems
    , ignoredProblems : IgnoredProblems
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
    , highlightedNumberCheckerId = Nothing
    , barcodeScannerData = BarcodeScanner.empty
    , problems = Problems.noProblems
    , ignoredProblems = Problems.noIgnoredProblems
    , eventDateAndTime = EventDateAndTime emptyEntry emptyEntry
    , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
    , secondTab = Tab.initialState
    , barcodeScannerTab = Tab.initialState
    , dialogDetails = NoDialog
    }
