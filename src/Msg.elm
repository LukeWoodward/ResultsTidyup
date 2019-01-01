module Msg exposing (Msg(..), NumberCheckerFieldChange(..))

import DataStructures exposing (InteropFile, WhichStopwatch(..))
import Time exposing (Posix, Zone)


type NumberCheckerFieldChange
    = Stopwatch1
    | Stopwatch2
    | FinishTokens


type Msg
    = NoOp
    | FileDropped InteropFile
    | ToggleTableRow Int
    | DeleteStopwatch WhichStopwatch
    | FlipStopwatches
    | ClearBarcodeScannerData
    | GetCurrentDateForDownloadFile
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
