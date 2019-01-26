module Msg exposing (Msg(..), NumberCheckerFieldChange(..))

import DataStructures exposing (InteropFile, MinorProblemFix, SecondTab, WhichStopwatch(..))
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
    | ClearBarcodeScannerData
    | ClearAllData
    | GetCurrentDateForDownloadFile (Zone -> Posix -> Msg)
    | DownloadMergedStopwatchData Zone Posix
    | DownloadBarcodeScannerData Zone Posix
    | ContainerHeightChanged Int
    | MouseEnterNumberCheckerRow Int
    | MouseLeaveNumberCheckerRow Int
    | DeleteNumberCheckerRow Int
    | EventDateChanged String
    | EventTimeChanged String
    | NumberCheckerFieldChanged NumberCheckerFieldChange String
    | AddNumberCheckerRow
    | EditNumberCheckerRow Int
    | FixMinorProblem MinorProblemFix
    | ChangeSecondTab SecondTab
