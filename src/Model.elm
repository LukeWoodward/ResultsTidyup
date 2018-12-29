module Model exposing (EventDateAndTime, Model, initModel)

import BarcodeScanner exposing (BarcodeScannerData)
import Error exposing (Error)
import MergedTable exposing (Stopwatches(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import Problems exposing (ProblemsContainer)
import Time exposing (Posix)


type alias EventDateAndTime =
    { enteredDate : String
    , validatedDate : Maybe Posix
    , time : Maybe Int
    }


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
    , eventDateAndTime = EventDateAndTime "" Nothing Nothing
    }
