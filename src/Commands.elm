module Commands exposing (Command(..), ElementToFocus(..))

import FileHandling exposing (InteropFile)
import Time exposing (Posix, Zone)


type Command a
    = NoCommand
    | GetCurrentDateAndTime (Zone -> Posix -> a)
    | DownloadFile String InteropFile
    | FocusElement ElementToFocus
    | SaveEventStartTime Int


type ElementToFocus
    = NumberCheckerManualEntryRowFirstCell
    | BarcodeScannerEditingAthleteInput
    | BarcodeScannerEditingAthleteRadioButton
