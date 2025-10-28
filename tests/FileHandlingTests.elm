module FileHandlingTests exposing (suite)

import Expect
import FileHandling exposing (deduceNameFromFilename)
import Test exposing (Test, describe, test)


makePrefixTest : String -> Test
makePrefixTest prefix =
    test ("deduceNameFromFilename removes '" ++ prefix ++ "' prefix") <|
        \() ->
            Expect.equal "Name" (deduceNameFromFilename (prefix ++ "Name"))


suite : Test
suite =
    describe "FileHandling tests"
        [ describe "deduceNameFromFilename tests"
            [ describe "Prefix trimming tests"
                (List.map
                    makePrefixTest
                    [ "parkrun_timer_", "parkrun_barcode_", "vv_Stopwatch_", "vv_Scanner_" ]
                )
            ]
        , test "Makes no changes to something that already looks like a name" <|
            \() ->
                Expect.equal "Name" (deduceNameFromFilename "Name")
        , test "Removes .txt extension" <|
            \() ->
                Expect.equal "Name" (deduceNameFromFilename "Name.txt")
        , test "Removes .csv extension" <|
            \() ->
                Expect.equal "Name" (deduceNameFromFilename "Name.csv")
        , test "Removes trailing numbers" <|
            \() ->
                Expect.equal "Name" (deduceNameFromFilename "Name1234567")
        , test "Trims whitespace" <|
            \() ->
                Expect.equal "Name" (deduceNameFromFilename "   Name   ")
        , test "removes 'junsd_stopwatch" <|
            \() ->
                Expect.equal "Name" (deduceNameFromFilename "Name_junsd_stopwatch")
        , test "adds space between name parts" <|
            \() ->
                Expect.equal "Name Surname" (deduceNameFromFilename "NameSurname")
        , test "adds space between multiple name parts" <|
            \() ->
                Expect.equal "Name Othername Surname" (deduceNameFromFilename "NameOthernameSurname")
        , test "adjusts a realistic timer file name with barcode number" <|
            \() ->
                Expect.equal "Joe BLOGGS" (deduceNameFromFilename "vv_Stopwatch_JoeBLOGGS_1234567_20210419110843.csv")
        , test "adjusts a realistic timer file name with barcode number with A" <|
            \() ->
                Expect.equal "Joe BLOGGS" (deduceNameFromFilename "vv_Stopwatch_JoeBLOGGS_A1234567_20210419110843.csv")
        , test "adjusts a realistic timer file name without barcode number" <|
            \() ->
                Expect.equal "Jane Doe" (deduceNameFromFilename "vv_Stopwatch_JaneDoe__20210419110843.csv")
        , test "adjusts a realistic scanner file name without barcode number" <|
            \() ->
                Expect.equal "John Smith" (deduceNameFromFilename "vv_Scanner_JohnSmith__20210419110843.csv")
        , test "adjusts an old timer file name" <|
            \() ->
                Expect.equal "13042019101835" (deduceNameFromFilename "parkrun_timer_13042019101835_junsd_stopwatch.txt")
        , test "adjusts an old scanner file name" <|
            \() ->
                Expect.equal "13042019101640" (deduceNameFromFilename "parkrun_barcode_13042019101640.txt")
        , test "adjusts an old Virtual Volunteer file name" <|
            \() ->
                Expect.equal "Timer Results - 2021-09-16 10-07-54" (deduceNameFromFilename "parkrun Virtual Volunteer - Timer Results - 2021-09-16_10-07-54.csv")
        ]
