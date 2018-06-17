port module Ports exposing (..)


type alias InteropFile =
    { fileName : String
    , fileText : String
    }


port fileDrop : (InteropFile -> msg) -> Sub msg


port downloadMergedTimesToFile : InteropFile -> Cmd msg
