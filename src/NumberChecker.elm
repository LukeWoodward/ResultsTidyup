module NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, addAndAnnotate, annotate, parseNumberCheckerFile, reannotate)

import Error exposing (Error)
import Maybe.Extra
import Result.Extra


{-| Type alias for a row of number-checker data read from a file.
-}
type alias NumberCheckerEntry =
    { stopwatch1 : Int
    , stopwatch2 : Int
    , finishTokens : Int
    }


type alias NumberCheckerEntryWithActual =
    { stopwatch1 : Int
    , stopwatch2 : Int
    , finishTokens : Int
    , actual : Int
    }


{-| Type alias for an annotated row of the number-checker table.
-}
type alias AnnotatedNumberCheckerEntry =
    { entryNumber : Int
    , stopwatch1 : Int
    , stopwatch1Delta : Int
    , stopwatch2 : Int
    , stopwatch2Delta : Int
    , finishTokens : Int
    , finishTokensDelta : Int
    , actual : Int
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
                    ++ String.fromInt (List.length someOtherList)
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


sortNumberCheckerEntries : List { a | finishTokens : Int } -> List { a | finishTokens : Int }
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


unannotateEntry : AnnotatedNumberCheckerEntry -> NumberCheckerEntry
unannotateEntry entry =
    NumberCheckerEntry entry.stopwatch1 entry.stopwatch2 entry.finishTokens


removeDeltas : AnnotatedNumberCheckerEntry -> NumberCheckerEntryWithActual
removeDeltas entry =
    NumberCheckerEntryWithActual entry.stopwatch1 entry.stopwatch2 entry.finishTokens entry.actual


addActualNumber : NumberCheckerEntry -> Int -> NumberCheckerEntryWithActual
addActualNumber { stopwatch1, stopwatch2, finishTokens } actual =
    NumberCheckerEntryWithActual stopwatch1 stopwatch2 finishTokens actual


getActualNumberFromPrevious : NumberCheckerEntry -> NumberCheckerEntryWithActual -> NumberCheckerEntryWithActual
getActualNumberFromPrevious thisEntry previousEntry =
    let
        stopwatch1Diff : Int
        stopwatch1Diff =
            thisEntry.stopwatch1 - previousEntry.stopwatch1

        stopwatch2Diff : Int
        stopwatch2Diff =
            thisEntry.stopwatch2 - previousEntry.stopwatch2

        finishTokensDiff : Int
        finishTokensDiff =
            thisEntry.finishTokens - previousEntry.finishTokens
    in
    if stopwatch1Diff == stopwatch2Diff && stopwatch1Diff == finishTokensDiff then
        -- Most common case: all agree
        addActualNumber thisEntry (previousEntry.actual + stopwatch1Diff)

    else if stopwatch1Diff == stopwatch2Diff then
        -- Finish tokens looks to be off...
        addActualNumber thisEntry (previousEntry.actual + stopwatch1Diff)

    else
        -- Anything else: take finish tokens to be authoritative
        addActualNumber thisEntry (previousEntry.actual + finishTokensDiff)


addActualNumbersInternal : NumberCheckerEntryWithActual -> List NumberCheckerEntry -> List NumberCheckerEntryWithActual
addActualNumbersInternal previousEntry entries =
    case entries of
        [] ->
            []

        firstEntry :: rest ->
            let
                firstEntryWithActual : NumberCheckerEntryWithActual
                firstEntryWithActual =
                    getActualNumberFromPrevious firstEntry previousEntry

                remainingEntriesWithActual : List NumberCheckerEntryWithActual
                remainingEntriesWithActual =
                    addActualNumbersInternal firstEntryWithActual rest
            in
            firstEntryWithActual :: remainingEntriesWithActual


addActualNumbers : List NumberCheckerEntry -> List NumberCheckerEntryWithActual
addActualNumbers entries =
    addActualNumbersInternal (NumberCheckerEntryWithActual 0 0 0 0) entries


calculateDeltasInternal : Int -> NumberCheckerEntryWithActual -> List NumberCheckerEntryWithActual -> List AnnotatedNumberCheckerEntry
calculateDeltasInternal previousRowNumber previousEntry entries =
    case entries of
        [] ->
            []

        firstEntry :: rest ->
            let
                thisRowNumber : Int
                thisRowNumber =
                    previousRowNumber + 1

                actualDiff : Int
                actualDiff =
                    firstEntry.actual - previousEntry.actual

                firstAnnotatedEntry : AnnotatedNumberCheckerEntry
                firstAnnotatedEntry =
                    { entryNumber = thisRowNumber
                    , stopwatch1 = firstEntry.stopwatch1
                    , stopwatch1Delta = (firstEntry.stopwatch1 - previousEntry.stopwatch1) - actualDiff
                    , stopwatch2 = firstEntry.stopwatch2
                    , stopwatch2Delta = (firstEntry.stopwatch2 - previousEntry.stopwatch2) - actualDiff
                    , finishTokens = firstEntry.finishTokens
                    , finishTokensDelta = (firstEntry.finishTokens - previousEntry.finishTokens) - actualDiff
                    , actual = firstEntry.actual
                    }

                restAnnotatedEntries : List AnnotatedNumberCheckerEntry
                restAnnotatedEntries =
                    calculateDeltasInternal thisRowNumber firstEntry rest
            in
            firstAnnotatedEntry :: restAnnotatedEntries


calculateDeltas : List NumberCheckerEntryWithActual -> List AnnotatedNumberCheckerEntry
calculateDeltas entries =
    calculateDeltasInternal 0 (NumberCheckerEntryWithActual 0 0 0 0) entries


annotate : List NumberCheckerEntry -> List AnnotatedNumberCheckerEntry
annotate entries =
    calculateDeltas (addActualNumbers entries)


getPreviousEntry : List NumberCheckerEntryWithActual -> NumberCheckerEntry -> NumberCheckerEntryWithActual
getPreviousEntry currentEntries newEntry =
    List.filter (\entry -> entry.finishTokens < newEntry.finishTokens) currentEntries
        |> sortNumberCheckerEntries
        |> List.reverse
        |> List.head
        |> Maybe.withDefault (NumberCheckerEntryWithActual 0 0 0 0)


addAndAnnotate : NumberCheckerEntry -> List AnnotatedNumberCheckerEntry -> List AnnotatedNumberCheckerEntry
addAndAnnotate newEntry existingAnnotatedEntries =
    let
        entriesWithoutDeltas : List NumberCheckerEntryWithActual
        entriesWithoutDeltas =
            List.map removeDeltas existingAnnotatedEntries

        previousEntry : NumberCheckerEntryWithActual
        previousEntry =
            getPreviousEntry entriesWithoutDeltas newEntry

        newEntryWithActual : NumberCheckerEntryWithActual
        newEntryWithActual =
            getActualNumberFromPrevious newEntry previousEntry
    in
    sortNumberCheckerEntries (newEntryWithActual :: entriesWithoutDeltas)
        |> sortNumberCheckerEntries
        |> calculateDeltas


reannotate : List AnnotatedNumberCheckerEntry -> List AnnotatedNumberCheckerEntry
reannotate entries =
    List.map removeDeltas entries
        |> sortNumberCheckerEntries
        |> calculateDeltas
