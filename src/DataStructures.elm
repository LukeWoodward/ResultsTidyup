module DataStructures exposing (EventDateAndTime, InteropFile, MinorProblemFix(..), SecondTab(..), WhichStopwatch(..))

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


type MinorProblemFix
    = RemoveUnassociatedFinishToken Int
    | RemoveUnassociatedAthlete String
    | RemoveDuplicateScans Int String
    | RemoveScansBeforeEventStart Int
