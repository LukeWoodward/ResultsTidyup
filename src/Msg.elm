module Msg exposing (Msg(..), NumberCheckerFieldChange(..))

import BarcodeScanner exposing (LineContents)
import BarcodeScannerEditing exposing (BarcodeScannerEditDetails, BarcodeScannerRowEditLocation)
import Bootstrap.Tab as Tab
import DataStructures exposing (InteropFile, ProblemFix, SecondTab, WhichStopwatch(..))
import Time exposing (Posix, Zone)


type NumberCheckerFieldChange
    = Stopwatch1
    | Stopwatch2
    | FinishTokens


type Msg
    = NoOp
    | FilesDropped (List InteropFile)
    | ToggleTableRow Int
    | DeleteStopwatch WhichStopwatch
    | FlipStopwatches
    | ClearAllData
    | GetCurrentDateForDownloadFile (Zone -> Posix -> Msg)
    | DownloadMergedStopwatchData Zone Posix
    | ContainerHeightChanged Int
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
    | DownloadBarcodeScannerFile Int Zone Posix
    | DeleteBarcodeScannerFile String
    | IgnoreProblem Int
    | ShowBarcodeScannerEditModal BarcodeScannerRowEditLocation LineContents
    | BarcodeScannerEdit BarcodeScannerEditDetails
    | DeleteRowFromBarcodeScannerEditModel BarcodeScannerRowEditLocation
    | CloseBarcodeScannerEditModal
