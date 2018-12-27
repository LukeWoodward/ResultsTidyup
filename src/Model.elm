module Model exposing (Model, initModel)

import BarcodeScanner exposing (BarcodeScannerData)
import Error exposing (Error)
import MergedTable exposing (Stopwatches(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import Problems exposing (ProblemsContainer)


type alias Model =
    { stopwatches : Stopwatches
    , lastError : Maybe Error
    , numberCheckerEntries : List AnnotatedNumberCheckerEntry
    , lastHeight : Maybe Int
    , highlightedNumberCheckerId : Maybe Int
    , barcodeScannerFiles : List String
    , barcodeScannerData : BarcodeScannerData
    , problems : ProblemsContainer
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
    }
