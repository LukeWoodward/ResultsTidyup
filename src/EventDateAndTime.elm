module EventDateAndTime exposing (EventDateAndTime)

import Time exposing (Posix)


type alias EventDateAndTime =
    { enteredDate : String
    , validatedDate : Maybe Posix
    , enteredTime : String
    , validatedTime : Maybe Int
    }
