module Model exposing
    ( DialogDetails(..)
    , Model
    , initModel
    )

import BarcodeScanner exposing (BarcodeScannerData)
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails)
import Error exposing (FileError)
import PastedFile exposing (PastedFileDetails)
import Problems exposing (IgnoredProblems, Problems)
import Timer exposing (Timers(..))
import TimerOperations exposing (TimerOperationEditDetails)
import TokenOperations exposing (TokenOperationEditDetails)


type DialogDetails
    = NoDialog
    | BarcodeScannerRowEditDialog BarcodeScannerRowEditDetails
    | TimerOperationsDialog TimerOperationEditDetails
    | TokenOperationsDialog TokenOperationEditDetails
    | PasteFileDialog PastedFileDetails
    | ConfirmClearEverythingDialog


type alias Model =
    { isBeta : Bool
    , timers : Timers
    , lastErrors : List FileError
    , barcodeScannerData : BarcodeScannerData
    , problems : Problems
    , ignoredProblems : IgnoredProblems
    , barcodeScannerTab : Maybe Int
    , dialogDetails : DialogDetails
    }


initModel : Model
initModel =
    { isBeta = False
    , timers = None
    , lastErrors = []
    , barcodeScannerData = BarcodeScanner.empty
    , problems = Problems.noProblems
    , ignoredProblems = Problems.noIgnoredProblems
    , barcodeScannerTab = Nothing
    , dialogDetails = NoDialog
    }
