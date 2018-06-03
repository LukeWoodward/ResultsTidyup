module ParkrunStopwatch exposing (..)

import Html exposing (program)
import Stopwatch exposing (Stopwatch)


-- Model, Update, View.


type alias Model =
    { stopwatches : List Stopwatch
    }


model : Int
model =
    0
