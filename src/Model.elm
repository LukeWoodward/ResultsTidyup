module Model exposing (Model, NumberCheckerManualEntryRow, NumericEntry, emptyNumberCheckerManualEntryRow, emptyNumericEntry, initModel)

import BarcodeScanner exposing (BarcodeScannerData)
import DataStructures exposing (EventDateAndTime, SecondTab(..))
import Error exposing (Error)
import MergedTable exposing (Stopwatches(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import Problems exposing (ProblemsContainer)
import Time exposing (Posix)


type alias NumericEntry =
    { enteredValue : String
    , parsedValue : Maybe Int
    }


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


type alias Model =
    { stopwatches : Stopwatches
    , lastError : Maybe Error
    , numberCheckerEntries : List AnnotatedNumberCheckerEntry
    , lastHeight : Maybe Int
    , highlightedNumberCheckerId : Maybe Int
    , barcodeScannerFiles : List String
    , barcodeScannerData : BarcodeScannerData
    , problems : ProblemsContainer
    , eventDateAndTime : EventDateAndTime
    , numberCheckerManualEntryRow : NumberCheckerManualEntryRow
    , secondTab : SecondTab
    }


initModel : Model
initModel =
    { stopwatches = None
    , lastError = Nothing
    , numberCheckerEntries = []
    , lastHeight = Nothing
    , highlightedNumberCheckerId = Nothing
    , barcodeScannerFiles = []
    , barcodeScannerData = BarcodeScanner.empty
    , problems = Problems.empty
    , eventDateAndTime = EventDateAndTime "" Nothing "" Nothing
    , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
    , secondTab = NumberCheckerTab
    }
