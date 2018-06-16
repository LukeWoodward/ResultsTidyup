port module Ports exposing (..)


type alias DroppedFile =
    { fileName : String
    , fileText : String
    }


port fileDrop : (DroppedFile -> msg) -> Sub msg
