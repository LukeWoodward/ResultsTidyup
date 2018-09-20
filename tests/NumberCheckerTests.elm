module NumberCheckerTests exposing (suite)

import Expect
import Test exposing (describe, test, Test)
import NumberChecker exposing (..)
import Errors exposing (expectError)


suite : Test
suite =
    describe "Number Checker tests"
        [ describe "parseNumberCheckerFile tests"
            [ test "reads single entry from valid line" <|
                \() ->
                    parseNumberCheckerFile "5,4,5"
                        |> Expect.equal (Ok [ NumberCheckerEntry 5 4 5 ])
            , test "reads single entry from valid line with additional whitespace" <|
                \() ->
                    parseNumberCheckerFile " 5  , \t 4 , 5  \t \t"
                        |> Expect.equal (Ok [ NumberCheckerEntry 5 4 5 ])
            , test "reads two entries from two valid lines" <|
                \() ->
                    parseNumberCheckerFile "5,4,5\n6,6,7"
                        |> Expect.equal
                            (Ok
                                [ NumberCheckerEntry 5 4 5
                                , NumberCheckerEntry 6 6 7
                                ]
                            )
            , test "reads two entries from two valid lines ignoring blank lines" <|
                \() ->
                    parseNumberCheckerFile "\r\n5,4,5\r\n\r\n\r\n6,6,7\r\n\r\n"
                        |> Expect.equal
                            (Ok
                                [ NumberCheckerEntry 5 4 5
                                , NumberCheckerEntry 6 6 7
                                ]
                            )
            , test "reads two entries from three valid lines ordering by finish-token count" <|
                \() ->
                    parseNumberCheckerFile "8,8,9\n5,4,5\n6,6,7"
                        |> Expect.equal
                            (Ok
                                [ NumberCheckerEntry 5 4 5
                                , NumberCheckerEntry 6 6 7
                                , NumberCheckerEntry 8 8 9
                                ]
                            )
            , test "fails with empty string" <|
                \() ->
                    parseNumberCheckerFile ""
                        |> expectError "EMPTY_FILE"
            , test "fails with invalid number string" <|
                \() ->
                    parseNumberCheckerFile "5,four,5"
                        |> expectError "INVALID_NUMBER"
            , test "fails with line containing too few numbers" <|
                \() ->
                    parseNumberCheckerFile "5,4"
                        |> expectError "WRONG_PART_COUNT"
            , test "fails with line containing too many numbers" <|
                \() ->
                    parseNumberCheckerFile "5,4,5,5"
                        |> expectError "WRONG_PART_COUNT"
            , test "fails with line containing zero" <|
                \() ->
                    parseNumberCheckerFile "5,4,0"
                        |> expectError "ZERO_OR_NEGATIVE_ENTRY"
            , test "fails with line containing negative number" <|
                \() ->
                    parseNumberCheckerFile "5,-1,5"
                        |> expectError "ZERO_OR_NEGATIVE_ENTRY"
            ]
        , describe "annotate tests"
            [ test "annotates an empty list as empty" <|
                \() ->
                    annotate []
                        |> Expect.equal []
            , test "annotates a single-element list with no differences with zeroes" <|
                \() ->
                    annotate [ NumberCheckerEntry 7 7 7 ]
                        |> Expect.equal [ AnnotatedNumberCheckerEntry 1 7 0 7 0 7 0 ]
            , test "annotates a single-element list with a possibly-too-large finish tokens count as a delta with that" <|
                \() ->
                    annotate [ NumberCheckerEntry 7 7 8 ]
                        |> Expect.equal [ AnnotatedNumberCheckerEntry 1 7 0 7 0 8 1 ]
            , test "annotates a single-element list with a possibly-too-small finish tokens count as a delta with that" <|
                \() ->
                    annotate [ NumberCheckerEntry 7 7 6 ]
                        |> Expect.equal [ AnnotatedNumberCheckerEntry 1 7 0 7 0 6 -1 ]
            , test "annotates a single-element list with a possibly-too-large stopwatch-1 count as a delta with that" <|
                \() ->
                    annotate [ NumberCheckerEntry 9 7 7 ]
                        |> Expect.equal [ AnnotatedNumberCheckerEntry 1 9 2 7 0 7 0 ]
            , test "annotates a single-element list with a possibly-too-small stopwatch-1 count as a delta with that" <|
                \() ->
                    annotate [ NumberCheckerEntry 4 7 7 ]
                        |> Expect.equal [ AnnotatedNumberCheckerEntry 1 4 -3 7 0 7 0 ]
            , test "annotates a single-element list with a possibly-too-large stopwatch-2 count as a delta with that" <|
                \() ->
                    annotate [ NumberCheckerEntry 7 8 7 ]
                        |> Expect.equal [ AnnotatedNumberCheckerEntry 1 7 0 8 1 7 0 ]
            , test "annotates a single-element list with a possibly-too-small stopwatch-2 count as a delta with that" <|
                \() ->
                    annotate [ NumberCheckerEntry 7 5 7 ]
                        |> Expect.equal [ AnnotatedNumberCheckerEntry 1 7 0 5 -2 7 0 ]
            , test "annotates a single-element list with all values different as relative to the finish tokens" <|
                \() ->
                    annotate [ NumberCheckerEntry 7 8 9 ]
                        |> Expect.equal [ AnnotatedNumberCheckerEntry 1 7 -2 8 -1 9 0 ]
            , test "annotates a three-element list with no differences with zeroes" <|
                \() ->
                    annotate
                        [ NumberCheckerEntry 7 7 7
                        , NumberCheckerEntry 15 15 15
                        , NumberCheckerEntry 26 26 26
                        ]
                        |> Expect.equal
                            [ AnnotatedNumberCheckerEntry 1 7 0 7 0 7 0
                            , AnnotatedNumberCheckerEntry 2 15 0 15 0 15 0
                            , AnnotatedNumberCheckerEntry 3 26 0 26 0 26 0
                            ]
            , test "annotates a three-element list with some differences" <|
                \() ->
                    annotate
                        [ NumberCheckerEntry 7 6 7
                        , NumberCheckerEntry 18 16 17
                        , NumberCheckerEntry 29 27 29
                        , NumberCheckerEntry 34 32 34
                        ]
                        |> Expect.equal
                            [ AnnotatedNumberCheckerEntry 1 7 0 6 -1 7 0
                            , AnnotatedNumberCheckerEntry 2 18 1 16 0 17 0
                            , AnnotatedNumberCheckerEntry 3 29 0 27 0 29 1
                            , AnnotatedNumberCheckerEntry 4 34 0 32 0 34 0
                            ]
            ]
        ]
