module Msg exposing (Msg(..), NumberCheckerFieldChange(..))

import BarcodeScanner exposing (LineContents)
import BarcodeScannerEditing exposing (BarcodeScannerEditDetails, BarcodeScannerRowEditDetails, BarcodeScannerRowEditLocation)
import Bootstrap.Tab as Tab
import Commands exposing (CurrentDateAndTimeOperation)
import File exposing (File)
import FileHandling exposing (InteropFile)
import ProblemFixing exposing (ProblemFix, ProblemIgnorance)
import Time exposing (Posix, Zone)
import Timer exposing (WhichTimer(..))
import TimerOperations exposing (TimerOperationChangeType, TimerOperationEditDetails)
import TokenOperations exposing (TokenOperationChangeType(..), TokenOperationEditDetails)


type NumberCheckerFieldChange
    = Timer1
    | Timer2
    | FinishTokens


type Msg
    = NoOp
    | FilesDropped (List InteropFile)
    | ToggleTableRow Int
    | DownloadTimer WhichTimer Zone Posix
    | RemoveTimer WhichTimer
    | FlipTimers
    | ClearAllData
    | RequestCurrentDateAndTime CurrentDateAndTimeOperation
    | DownloadMergedTimerData Zone Posix
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
    | ShowTimerOperationsModal
    | TimerOperationEdit TimerOperationChangeType
    | ApplyTimerOperation TimerOperationEditDetails
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
