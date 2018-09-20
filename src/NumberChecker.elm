module NumberChecker exposing (NumberCheckerEntry, AnnotatedNumberCheckerEntry, parseNumberCheckerFile, annotate)

import Maybe.Extra
import Result.Extra
import Error exposing (Error)


type alias NumberCheckerEntry =
    { stopwatch1 : Int
    , stopwatch2 : Int
    , finishTokens : Int
    }


type alias AnnotatedNumberCheckerEntry =
    { entryNumber : Int
    , stopwatch1 : Int
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

        numbers : Maybe (List Int)
        numbers =
            bits
                |> List.map String.toInt
                |> Maybe.Extra.combine
    in
        case numbers of
            Just [ sw1, sw2, ftoks ] ->
                if sw1 > 0 && sw2 > 0 && ftoks > 0 then
                    NumberCheckerEntry sw1 sw2 ftoks
                        |> Ok
                else
                    Error
                        "ZERO_OR_NEGATIVE_ENTRY"
                        ("One or more numbers read from the line '"
                            ++ line
                            ++ "' was zero or negative"
                        )
                        |> Err

            Just someOtherList ->
                Error
                    "WRONG_PART_COUNT"
                    ("Unexpected number of parts: expected three, got "
                        ++ (String.fromInt (List.length someOtherList))
                        ++ " instead"
                    )
                    |> Err

            Nothing ->
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


annotateEntry : NumberCheckerEntry -> Int -> Int -> Int -> Int -> AnnotatedNumberCheckerEntry
annotateEntry { stopwatch1, stopwatch2, finishTokens } entryNumber stopwatch1Diff stopwatch2Diff finishTokensDiff =
    AnnotatedNumberCheckerEntry entryNumber stopwatch1 stopwatch1Diff stopwatch2 stopwatch2Diff finishTokens finishTokensDiff


annotateInternal : Int -> NumberCheckerEntry -> List NumberCheckerEntry -> List AnnotatedNumberCheckerEntry
annotateInternal previousRowNumber previousEntry entries =
    case entries of
        [] ->
            []

        firstEntry :: rest ->
            let
                thisRowNumber : Int
                thisRowNumber =
                    previousRowNumber + 1

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
                        annotateEntry firstEntry thisRowNumber 0 0 0
                    else if stopwatch1Diff == stopwatch2Diff then
                        -- Finish tokens looks to be off...
                        annotateEntry firstEntry thisRowNumber 0 0 (finishTokensDiff - stopwatch1Diff)
                    else
                        -- Anything else: take finish tokens to be authoritative
                        annotateEntry firstEntry thisRowNumber (stopwatch1Diff - finishTokensDiff) (stopwatch2Diff - finishTokensDiff) 0

                restAnnotatedEntries : List AnnotatedNumberCheckerEntry
                restAnnotatedEntries =
                    annotateInternal thisRowNumber firstEntry rest
            in
                firstAnnotatedEntry :: restAnnotatedEntries


annotate : List NumberCheckerEntry -> List AnnotatedNumberCheckerEntry
annotate entries =
    annotateInternal 0 (NumberCheckerEntry 0 0 0) entries
