module Msg exposing (Msg(..), NumberCheckerFieldChange(..))

import BarcodeScanner exposing (LineContents)
import BarcodeScannerEditing exposing (BarcodeScannerEditDetails, BarcodeScannerRowEditDetails, BarcodeScannerRowEditLocation)
import Bootstrap.Tab as Tab
import Commands exposing (DownloadOperation)
import File exposing (File)
import FileHandling exposing (InteropFile)
import ProblemFixing exposing (ProblemFix)
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
    | GetCurrentDateForDownloadFile DownloadOperation
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
    | ChangeSecondTab Tab.State
    | ChangeBarcodeScannerTab Tab.State
    | ClearErrors
    | DownloadBarcodeScannerFile String Zone Posix
    | DownloadAllBarcodeScannerData Zone Posix
    | RemoveBarcodeScannerFile String
    | IgnoreProblem Int
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
    | FilesUploaded File (List File)
