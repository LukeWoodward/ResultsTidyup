module Errors exposing (expectError)

import Error exposing (Error)
import Expect exposing (Expectation)


expectError : String -> Result Error a -> Expectation
expectError expectedCode result =
    case result of
        Ok value ->
            Expect.fail ("Expected to fail with error " ++ expectedCode ++ ", succeeded with " ++ Debug.toString value)

        Err error ->
            Expect.equal expectedCode error.code
