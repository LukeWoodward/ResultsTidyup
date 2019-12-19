module Commands exposing (Command(..), DownloadOperation(..), ElementToFocus(..))

import File exposing (File)
import FileHandling exposing (InteropFile)
import Stopwatch exposing (WhichStopwatch)
import Time exposing (Posix, Zone)


type ElementToFocus
    = NumberCheckerManualEntryRowFirstCell
    | BarcodeScannerEditingAthleteInput
    | BarcodeScannerEditingAthleteRadioButton


type DownloadOperation
    = DownloadSingleStopwatch WhichStopwatch
    | DownloadMergedStopwatches
    | DownloadBarcodeScannerFile String
    | DownloadAllBarcodeScannerData


type Command
    = NoCommand
    | GetCurrentDateAndTime DownloadOperation
    | DownloadFile String InteropFile
    | FocusElement ElementToFocus
    | SaveEventStartTime Int
    | SelectFileForUpload
    | ReadFiles (List File)
