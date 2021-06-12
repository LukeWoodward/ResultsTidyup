module Msg exposing (Msg(..), NumberCheckerFieldChange(..))

import BarcodeScanner exposing (LineContents)
import BarcodeScannerEditing exposing (BarcodeScannerEditDetails, BarcodeScannerRowEditDetails, BarcodeScannerRowEditLocation)
import Bootstrap.Tab as Tab
import Commands exposing (CurrentDateAndTimeOperation)
import File exposing (File)
import FileHandling exposing (InteropFile)
import ProblemFixing exposing (ProblemFix, ProblemIgnorance)
import Stopwatch exposing (WhichStopwatch(..))
import StopwatchOperations exposing (StopwatchOperationChangeType, StopwatchOperationEditDetails)
import Time exposing (Posix, Zone)
import TokenOperations exposing (TokenOperationChangeType(..), TokenOperationEditDetails)


type NumberCheckerFieldChange
    = Stopwatch1
    | Stopwatch2
    | FinishTokens


type Msg
    = NoOp
    | FilesDropped (List InteropFile)
    | ToggleTableRow Int
    | DownloadStopwatch WhichStopwatch Zone Posix
    | RemoveStopwatch WhichStopwatch
    | FlipStopwatches
    | ClearAllData
    | RequestCurrentDateAndTime CurrentDateAndTimeOperation
    | DownloadMergedStopwatchData Zone Posix
    | MouseEnterNumberCheckerRow Int
    | MouseLeaveNumberCheckerRow Int
    | DeleteNumberCheckerRow Int
    | EventDateChanged String
    | EventTimeChanged String
    | NumberCheckerFieldChanged NumberCheckerFieldChange String
    | AddNumberCheckerRow
    | EditNumberCheckerRow Int
    | IncrementNumberCheckerRowActualCount Int
    | DecrementNumberCheckerRowActualCount Int
    | FixProblem ProblemFix
    | IgnoreProblem ProblemIgnorance
    | ChangeSecondTab Tab.State
    | ChangeBarcodeScannerTab Tab.State
    | ClearErrors
    | DownloadBarcodeScannerFile String Zone Posix
    | DownloadAllBarcodeScannerData Zone Posix
    | RemoveBarcodeScannerFile String
    | ShowBarcodeScannerEditModal BarcodeScannerRowEditLocation LineContents Bool
    | BarcodeScannerEdit BarcodeScannerEditDetails
    | UpdateRowFromBarcodeScannerEditModal BarcodeScannerRowEditDetails
    | DeleteRowFromBarcodeScannerEditModal BarcodeScannerRowEditLocation
    | ShowStopwatchOperationsModal
    | StopwatchOperationEdit StopwatchOperationChangeType
    | ApplyStopwatchOperation StopwatchOperationEditDetails
    | ShowTokenOperationsModal
    | TokenOperationEdit TokenOperationChangeType
    | ApplyTokenOperation TokenOperationEditDetails
    | ReturnKeyPressed
    | CloseModal
    | OpenUploadFileDialog
    | OpenPasteFileDialog
    | PastedFileChanged String
    | PastedFileUploaded String Zone Posix
    | FilesUploaded File (List File)
