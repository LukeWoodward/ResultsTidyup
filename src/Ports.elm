port module Ports exposing (fileDrop, getInitialHeight, heightUpdated)

import DataStructures exposing (InteropFile)


port getInitialHeight : () -> Cmd msg


port fileDrop : (InteropFile -> msg) -> Sub msg


port heightUpdated : (Int -> msg) -> Sub msg
