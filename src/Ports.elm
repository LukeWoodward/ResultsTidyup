port module Ports exposing (filesDropped, getInitialHeight, heightUpdated, recordEventStartTime)

import DataStructures exposing (InteropFile)


port getInitialHeight : () -> Cmd msg


port filesDropped : (List InteropFile -> msg) -> Sub msg


port heightUpdated : (Int -> msg) -> Sub msg


port recordEventStartTime : Int -> Cmd msg
