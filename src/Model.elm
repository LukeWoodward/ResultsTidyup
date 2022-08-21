module Model exposing
    ( DialogDetails(..)
    , Model
    , NumberCheckerManualEntryRow
    , emptyNumberCheckerManualEntryRow
    , initModel
    )

import BarcodeScanner exposing (BarcodeScannerData)
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails)
import Bootstrap.Tab as Tab
import DataEntry exposing (IntegerEntry, emptyEntry)
import Error exposing (FileError)
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import PastedFile exposing (PastedFileDetails)
import Problems exposing (IgnoredProblems, Problems)
import Timer exposing (Timers(..))
import TimerOperations exposing (TimerOperationEditDetails)
import TokenOperations exposing (TokenOperationEditDetails)


type alias NumberCheckerManualEntryRow =
    { timer1 : IntegerEntry
    , timer2 : IntegerEntry
    , finishTokens : IntegerEntry
    }


emptyNumberCheckerManualEntryRow : NumberCheckerManualEntryRow
emptyNumberCheckerManualEntryRow =
    NumberCheckerManualEntryRow emptyEntry emptyEntry emptyEntry


type DialogDetails
    = NoDialog
    | BarcodeScannerRowEditDialog BarcodeScannerRowEditDetails
    | TimerOperationsDialog TimerOperationEditDetails
    | TokenOperationsDialog TokenOperationEditDetails
    | PasteFileDialog PastedFileDetails


type alias Model =
    { isBeta : Bool
    , timers : Timers
    , lastErrors : List FileError
    , numberCheckerEntries : List AnnotatedNumberCheckerEntry
    , highlightedNumberCheckerId : Maybe Int
    , barcodeScannerData : BarcodeScannerData
    , problems : Problems
    , ignoredProblems : IgnoredProblems
    , numberCheckerManualEntryRow : NumberCheckerManualEntryRow
    , secondTab : Tab.State
    , barcodeScannerTab : Tab.State
    , dialogDetails : DialogDetails
    }


initModel : Model
initModel =
    { isBeta = False
    , timers = None
    , lastErrors = []
    , numberCheckerEntries = []
    , highlightedNumberCheckerId = Nothing
    , barcodeScannerData = BarcodeScanner.empty
    , problems = Problems.noProblems
    , ignoredProblems = Problems.noIgnoredProblems
    , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
    , secondTab = Tab.initialState
    , barcodeScannerTab = Tab.initialState
    , dialogDetails = NoDialog
    }
