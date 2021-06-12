module Commands exposing (Command(..), CurrentDateAndTimeOperation(..), ElementToFocus(..))

import File exposing (File)
import FileHandling exposing (InteropFile)
import Stopwatch exposing (WhichStopwatch)
import Time exposing (Posix, Zone)


type ElementToFocus
    = NumberCheckerManualEntryRowFirstCell
    | BarcodeScannerEditingAthleteInput
    | BarcodeScannerEditingAthleteRadioButton
    | PasteFileDialogTextArea


type CurrentDateAndTimeOperation
    = DownloadSingleStopwatch WhichStopwatch
    | DownloadMergedStopwatches
    | DownloadBarcodeScannerFile String
    | DownloadAllBarcodeScannerData
    | UploadPastedFile String


type Command
    = NoCommand
    | GetCurrentDateAndTime CurrentDateAndTimeOperation
    | DownloadFile String InteropFile
    | FocusElement ElementToFocus
    | SaveEventStartTime Int
    | SelectFileForUpload
    | ReadFiles (List File)
