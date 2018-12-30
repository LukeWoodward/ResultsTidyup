module Msg exposing (Msg(..))

import DataStructures exposing (InteropFile, WhichStopwatch(..))
import Time exposing (Posix, Zone)


type Msg
    = FileDropped InteropFile
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
