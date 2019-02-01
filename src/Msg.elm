module Msg exposing (Msg(..), NumberCheckerFieldChange(..))

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
    | FixProblem ProblemFix
    | ChangeSecondTab SecondTab
    | ClearErrors
    | DownloadBarcodeScannerFile Int Zone Posix
    | DeleteBarcodeScannerFile Int
