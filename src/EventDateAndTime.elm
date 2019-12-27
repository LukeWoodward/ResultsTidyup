module EventDateAndTime exposing (EventDateAndTime)

import DataEntry exposing (DateEntry, IntegerEntry)


type alias EventDateAndTime =
    { date : DateEntry
    , time : IntegerEntry
    }
