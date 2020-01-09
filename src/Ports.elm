port module Ports exposing (filesDropped, recordEventStartTime)

import FileHandling exposing (InteropFile)


port filesDropped : (List InteropFile -> msg) -> Sub msg


port recordEventStartTime : Int -> Cmd msg
