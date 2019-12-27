module EventDateAndTime exposing (EventDateAndTime)

import NumericEntry exposing (IntegerEntry)
import Time exposing (Posix)


type alias EventDateAndTime =
    { enteredDate : String
    , validatedDate : Maybe Posix
    , time : IntegerEntry
    }
