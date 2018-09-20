module Errors exposing (expectError)

import Expect exposing (Expectation)
import Error exposing (Error)


expectError : String -> Result Error a -> Expectation
expectError expectedCode result =
    case result of
        Ok value ->
            Expect.fail ("Expected to fail with error " ++ expectedCode ++ ", succeeded with " ++ (Debug.toString value))

        Err error ->
            Expect.equal expectedCode error.code
