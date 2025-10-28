module Msg exposing (Msg(..))

import BarcodeScanner exposing (LineContents)
import BarcodeScannerEditing exposing (BarcodeScannerEditDetails, BarcodeScannerRowEditDetails, BarcodeScannerRowEditLocation)
import Commands exposing (CurrentDateAndTimeOperation)
import File exposing (File)
import FileHandling exposing (AddedFile, InteropFile)
import ProblemFixing exposing (ProblemFix, ProblemIgnorance)
import Time exposing (Posix, Zone)
import Timer exposing (WhichTimer(..))
import TimerOperations exposing (TimerOperationChangeType, TimerOperationEditDetails)
import TokenOperations exposing (TokenOperationChangeType(..), TokenOperationEditDetails)


type Msg
    = NoOp
    | FilesDropped (List InteropFile)
    | FilesAdded (List AddedFile)
    | ToggleTableRow Int
    | DownloadTimer WhichTimer Zone Posix
    | RemoveTimer WhichTimer
    | FlipTimers
    | ClearAllData
    | RequestCurrentDateAndTime CurrentDateAndTimeOperation
    | DownloadMergedTimerData Zone Posix
    | FixProblem ProblemFix
    | IgnoreProblem ProblemIgnorance
    | ChangeBarcodeScannerTab Int
    | ClearErrors
    | DownloadBarcodeScannerFile String Zone Posix
    | DownloadAllBarcodeScannerData Zone Posix
    | RemoveBarcodeScannerFile Int
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
    | OpenConfirmClearEverythingDialog
    | PastedFileChanged String
    | PastedFileUploaded String Zone Posix
    | FilesUploaded File (List File)
