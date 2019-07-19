module Model exposing
    ( DialogDetails(..)
    , Model
    , NumberCheckerManualEntryRow
    , ProblemEntry
    , emptyNumberCheckerManualEntryRow
    , emptyNumericEntry
    , initModel
    )

import BarcodeScanner exposing (BarcodeScannerData, LineContents)
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails, BarcodeScannerRowEditLocation)
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Tab as Tab
import Error exposing (FileError)
import EventDateAndTime exposing (EventDateAndTime)
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import NumericEntry exposing (NumericEntry)
import Problems exposing (Problem)
import Stopwatch exposing (Stopwatches(..))
import Time exposing (Posix)


emptyNumericEntry : NumericEntry
emptyNumericEntry =
    NumericEntry "" Nothing


type alias NumberCheckerManualEntryRow =
    { stopwatch1 : NumericEntry
    , stopwatch2 : NumericEntry
    , finishTokens : NumericEntry
    }


emptyNumberCheckerManualEntryRow : NumberCheckerManualEntryRow
emptyNumberCheckerManualEntryRow =
    NumberCheckerManualEntryRow emptyNumericEntry emptyNumericEntry emptyNumericEntry


type alias ProblemEntry =
    { problem : Problem
    , index : Int
    , ignored : Bool
    }


type DialogDetails
    = NoDialog
    | BarcodeScannerRowEditDialog BarcodeScannerRowEditDetails


type alias Model =
    { isBeta : Bool
    , stopwatches : Stopwatches
    , lastErrors : List FileError
    , numberCheckerEntries : List AnnotatedNumberCheckerEntry
    , lastHeight : Maybe Int
    , highlightedNumberCheckerId : Maybe Int
    , barcodeScannerData : BarcodeScannerData
    , problems : List ProblemEntry
    , eventDateAndTime : EventDateAndTime
    , numberCheckerManualEntryRow : NumberCheckerManualEntryRow
    , secondTab : Tab.State
    , barcodeScannerTab : Tab.State
    , dialogDetails : DialogDetails
    , actionsDropdownState : Dropdown.State
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
    , problems = []
    , eventDateAndTime = EventDateAndTime "" Nothing "" Nothing
    , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
    , secondTab = Tab.initialState
    , barcodeScannerTab = Tab.initialState
    , dialogDetails = NoDialog
    , actionsDropdownState = Dropdown.initialState
    }
