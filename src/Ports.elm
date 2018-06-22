port module Ports exposing (..)


type alias InteropFile =
    { fileName : String
    , fileText : String
    }


port getInitialHeight : () -> Cmd msg


port fileDrop : (InteropFile -> msg) -> Sub msg


port downloadMergedTimesToFile : InteropFile -> Cmd msg


port heightUpdated : (Int -> msg) -> Sub msg
