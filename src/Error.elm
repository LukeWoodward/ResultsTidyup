module Error exposing (Error, FileError, mapError)


type alias Error =
    { code : String
    , message : String
    }


type alias FileError =
    { code : String
    , message : String
    , fileName : String
    }


mapError : String -> Error -> FileError
mapError fileName { code, message } =
    FileError code message fileName
