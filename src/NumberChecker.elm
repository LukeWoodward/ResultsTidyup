module NumberChecker exposing (NumberCheckerEntry, AnnotatedNumberCheckerEntry, parseNumberCheckerFile, annotate)

import Result.Extra
import Error exposing (Error)


type alias NumberCheckerEntry =
    { stopwatch1 : Int
    , stopwatch2 : Int
    , finishTokens : Int
    }


type alias AnnotatedNumberCheckerEntry =
    { stopwatch1 : Int
    , stopwatch1Delta : Int
    , stopwatch2 : Int
    , stopwatch2Delta : Int
    , finishTokens : Int
    , finishTokensDelta : Int
    }


parseNumberCheckerFileLine : String -> Result Error NumberCheckerEntry
parseNumberCheckerFileLine line =
    let
        bits : List String
        bits =
            line
                |> String.split ","
                |> List.map String.trim

        numbers : Result String (List Int)
        numbers =
            bits
                |> List.map String.toInt
                |> Result.Extra.combine
    in
        case numbers of
            Ok [ sw1, sw2, ftoks ] ->
                if sw1 > 0 && sw2 > 0 && ftoks > 0 then
                    NumberCheckerEntry sw1 sw2 ftoks
                        |> Ok
                else
                    Error
                        "ZERO_OR_NEGATIVE_ENTRY"
                        ("One or more numbers read from the line '"
                            ++ (toString line)
                            ++ "' was zero or negative"
                        )
                        |> Err

            Ok someOtherList ->
                Error
                    "WRONG_PART_COUNT"
                    ("Unexpected number of parts: expected three, got "
                        ++ (toString (List.length someOtherList))
                        ++ " instead"
                    )
                    |> Err

            Err _ ->
                Error
                    "INVALID_NUMBER"
                    ("Unrecognised numeric value in line '" ++ line ++ "'")
                    |> Err


failIfEmpty : List NumberCheckerEntry -> Result Error (List NumberCheckerEntry)
failIfEmpty list =
    if List.isEmpty list then
        Error "EMPTY_FILE" "Number checker file was empty"
            |> Err
    else
        Ok list


sortNumberCheckerEntries : List NumberCheckerEntry -> List NumberCheckerEntry
sortNumberCheckerEntries entries =
    List.sortBy .finishTokens entries


parseNumberCheckerFile : String -> Result Error (List NumberCheckerEntry)
parseNumberCheckerFile fileText =
    fileText
        |> String.lines
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map parseNumberCheckerFileLine
        |> Result.Extra.combine
        |> Result.andThen failIfEmpty
        |> Result.map sortNumberCheckerEntries


annotateEntry : NumberCheckerEntry -> Int -> Int -> Int -> AnnotatedNumberCheckerEntry
annotateEntry { stopwatch1, stopwatch2, finishTokens } stopwatch1Diff stopwatch2Diff finishTokensDiff =
    AnnotatedNumberCheckerEntry stopwatch1 stopwatch1Diff stopwatch2 stopwatch2Diff finishTokens finishTokensDiff


annotateInternal : NumberCheckerEntry -> List NumberCheckerEntry -> List AnnotatedNumberCheckerEntry
annotateInternal previousEntry entries =
    case entries of
        [] ->
            []

        firstEntry :: rest ->
            let
                stopwatch1Diff : Int
                stopwatch1Diff =
                    firstEntry.stopwatch1 - previousEntry.stopwatch1

                stopwatch2Diff : Int
                stopwatch2Diff =
                    firstEntry.stopwatch2 - previousEntry.stopwatch2

                finishTokensDiff : Int
                finishTokensDiff =
                    firstEntry.finishTokens - previousEntry.finishTokens

                firstAnnotatedEntry : AnnotatedNumberCheckerEntry
                firstAnnotatedEntry =
                    if stopwatch1Diff == stopwatch2Diff && stopwatch1Diff == finishTokensDiff then
                        -- Most common case: all agree
                        annotateEntry firstEntry 0 0 0
                    else if stopwatch1Diff == stopwatch2Diff then
                        -- Finish tokens looks to be off...
                        annotateEntry firstEntry 0 0 (finishTokensDiff - stopwatch1Diff)
                    else
                        -- Anything else: take finish tokens to be authoritative
                        annotateEntry firstEntry (stopwatch1Diff - finishTokensDiff) (stopwatch2Diff - finishTokensDiff) 0

                restAnnotatedEntries : List AnnotatedNumberCheckerEntry
                restAnnotatedEntries =
                    annotateInternal firstEntry rest
            in
                firstAnnotatedEntry :: restAnnotatedEntries


annotate : List NumberCheckerEntry -> List AnnotatedNumberCheckerEntry
annotate entries =
    annotateInternal (NumberCheckerEntry 0 0 0) entries
