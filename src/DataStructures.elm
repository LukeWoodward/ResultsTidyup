module DataStructures exposing (EventDateAndTime, SecondTab(..))

import Stopwatch exposing (WhichStopwatch)
import Time exposing (Posix)


type alias EventDateAndTime =
    { enteredDate : String
    , validatedDate : Maybe Posix
    , enteredTime : String
    , validatedTime : Maybe Int
    }


type SecondTab
    = BarcodeScannersTab
    | NumberCheckerTab
