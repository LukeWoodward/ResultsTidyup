port module Ports exposing (filesDropped)

import FileHandling exposing (InteropFile)


port filesDropped : (List InteropFile -> msg) -> Sub msg
