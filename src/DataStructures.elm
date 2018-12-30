module DataStructures exposing (InteropFile, WhichStopwatch(..))


type WhichStopwatch
    = StopwatchOne
    | StopwatchTwo


type alias InteropFile =
    { fileName : String
    , fileText : String
    }
