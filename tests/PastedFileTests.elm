module PastedFileTests exposing (stopwatchFileContents, suite)

import Errors exposing (expectError)
import Expect
import PastedFile exposing (..)
import Test exposing (Test, describe, test)


stopwatchFileContents : String
stopwatchFileContents =
    "STARTOFEVENT,01/01/2001 00:00:00,test\n"
        ++ "0,01/01/2001 00:00:00\n"
        ++ "1,01/01/2001 00:19:22,00:19:22\n"
        ++ "2,01/01/2001 00:20:07,00:20:07\n"
        ++ "3,01/01/2001 00:23:14,00:23:14\n"
        ++ "ENDOFEVENT,01/01/2001 00:34:42\n"


barcodeScannerFileContents : String
barcodeScannerFileContents =
    "A12345,P0002,01/01/2001 09:22:07\n"
        ++ "A54321,P0006,01/01/2001 09:24:14\n"
        ++ "A13579,P0003,01/01/2001 09:24:49\n"
        ++ "A86420,P0001,01/01/2001 09:25:02\n"


suite : Test
suite =
    describe "PastedFile tests"
        [ describe "interpretPastedFile tests"
            [ test "interpretPastedFile of an empty string is NoFilePasted" <|
                \() ->
                    interpretPastedFile ""
                        |> Expect.equal NoFilePasted
            , test "interpretPastedFile of an whitespace-noly string is NoFilePasted" <|
                \() ->
                    interpretPastedFile "           \n     \t  \n    "
                        |> Expect.equal NoFilePasted
            , test "interpretPastedFile of a stopwatch file is StopwatchFilePasted" <|
                \() ->
                    interpretPastedFile stopwatchFileContents
                        |> Expect.equal (StopwatchFilePasted 3)
            , test "interpretPastedFile of a stopwatch file with leading and trailing whitespaces is StopwatchFilePasted" <|
                \() ->
                    interpretPastedFile ("   \n   \t  " ++ stopwatchFileContents ++ "   \n  \t   ")
                        |> Expect.equal (StopwatchFilePasted 3)
            , test "interpretPastedFile of a barcode scanner file is StopwatchFilePasted" <|
                \() ->
                    interpretPastedFile barcodeScannerFileContents
                        |> Expect.equal (BarcodeScannerFilePasted 4)
            , test "interpretPastedFile of a barcode scanner file with leading and trailing whitespaces is StopwatchFilePasted" <|
                \() ->
                    interpretPastedFile ("   \n   \t  " ++ barcodeScannerFileContents ++ "   \n  \t   ")
                        |> Expect.equal (BarcodeScannerFilePasted 4)
            , test "interpretPastedFile of an unrecognised file is UnrecognisedFilePasted" <|
                \() ->
                    interpretPastedFile "This is not a recognised format of file"
                        |> Expect.equal UnrecognisedFilePasted
            ]
        ]
