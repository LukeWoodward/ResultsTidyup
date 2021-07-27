module Commands exposing (Command(..), CurrentDateAndTimeOperation(..), ElementToFocus(..))

import File exposing (File)
import FileHandling exposing (InteropFile)
import Time exposing (Posix, Zone)
import Timer exposing (WhichTimer)


type ElementToFocus
    = NumberCheckerManualEntryRowFirstCell
    | BarcodeScannerEditingAthleteInput
    | BarcodeScannerEditingAthleteRadioButton
    | PasteFileDialogTextArea


type CurrentDateAndTimeOperation
    = DownloadSingleTimer WhichTimer
    | DownloadMergedTimers
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
