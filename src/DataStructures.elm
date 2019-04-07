module DataStructures exposing (EventDateAndTime, InteropFile, ProblemFix(..), SecondTab(..), WhichStopwatch(..))

import Time exposing (Posix)


type WhichStopwatch
    = StopwatchOne
    | StopwatchTwo


type alias InteropFile =
    { fileName : String
    , fileText : String
    }


type alias EventDateAndTime =
    { enteredDate : String
    , validatedDate : Maybe Posix
    , enteredTime : String
    , validatedTime : Maybe Int
    }


type SecondTab
    = BarcodeScannersTab
    | NumberCheckerTab


type ProblemFix
    = RemoveUnassociatedFinishToken Int
    | RemoveUnassociatedAthlete String
    | RemoveDuplicateScans Int String
    | RemoveScansBeforeEventStart Int
    | AdjustStopwatch WhichStopwatch Int
    | SwapBarcodes String Int Int
