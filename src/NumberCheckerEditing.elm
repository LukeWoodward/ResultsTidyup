module NumberCheckerEditing exposing
    ( addNumberCheckerRow
    , deleteNumberCheckerEntry
    , editNumberCheckerRow
    , handleNumberCheckerFieldChange
    , modifyNumberCheckerRows
    )

import Model exposing (Model, NumberCheckerManualEntryRow, emptyNumberCheckerManualEntryRow)
import Msg exposing (NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, addAndAnnotate, reannotate)
import NumericEntry exposing (NumericEntry, numericEntryFromInt)


addNumberCheckerRow : Model -> ( Model, Bool )
addNumberCheckerRow model =
    let
        manualEntryRow =
            model.numberCheckerManualEntryRow
    in
    case ( manualEntryRow.stopwatch1.parsedValue, manualEntryRow.stopwatch2.parsedValue, manualEntryRow.finishTokens.parsedValue ) of
        ( Just stopwatch1, Just stopwatch2, Just finishTokens ) ->
            let
                newNumberCheckerEntries : List AnnotatedNumberCheckerEntry
                newNumberCheckerEntries =
                    addAndAnnotate (NumberCheckerEntry stopwatch1 stopwatch2 finishTokens) model.numberCheckerEntries
            in
            ( { model
                | numberCheckerEntries = newNumberCheckerEntries
                , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
              }
            , True
            )

        _ ->
            ( model, False )


editNumberCheckerRow : Int -> Model -> Model
editNumberCheckerRow entryNumber model =
    let
        numberCheckerEntryToEdit : Maybe AnnotatedNumberCheckerEntry
        numberCheckerEntryToEdit =
            List.filter (\e -> e.entryNumber == entryNumber) model.numberCheckerEntries
                |> List.head
    in
    case numberCheckerEntryToEdit of
        Just entry ->
            let
                modelWithEntryDeleted : Model
                modelWithEntryDeleted =
                    deleteNumberCheckerEntry entryNumber model
            in
            { modelWithEntryDeleted
                | numberCheckerManualEntryRow =
                    NumberCheckerManualEntryRow
                        (numericEntryFromInt entry.stopwatch1)
                        (numericEntryFromInt entry.stopwatch2)
                        (numericEntryFromInt entry.finishTokens)
            }

        Nothing ->
            model


deleteNumberCheckerEntry : Int -> Model -> Model
deleteNumberCheckerEntry entryNumber model =
    let
        newNumberCheckerEntries : List AnnotatedNumberCheckerEntry
        newNumberCheckerEntries =
            List.filter (\e -> e.entryNumber /= entryNumber) model.numberCheckerEntries
                |> reannotate
    in
    { model | numberCheckerEntries = newNumberCheckerEntries }


ensureNonNegative : Int -> Maybe Int
ensureNonNegative intValue =
    if intValue < 0 then
        Nothing

    else
        Just intValue


handleNumberCheckerFieldChange : NumberCheckerFieldChange -> String -> Model -> Model
handleNumberCheckerFieldChange fieldChange newValue model =
    let
        oldNumberCheckerRow : NumberCheckerManualEntryRow
        oldNumberCheckerRow =
            model.numberCheckerManualEntryRow

        newEntry : NumericEntry
        newEntry =
            String.toInt newValue
                |> Maybe.andThen ensureNonNegative
                |> NumericEntry newValue

        newNumberCheckerManualEntryRow : NumberCheckerManualEntryRow
        newNumberCheckerManualEntryRow =
            case fieldChange of
                Stopwatch1 ->
                    { oldNumberCheckerRow | stopwatch1 = newEntry }

                Stopwatch2 ->
                    { oldNumberCheckerRow | stopwatch2 = newEntry }

                FinishTokens ->
                    { oldNumberCheckerRow | finishTokens = newEntry }
    in
    { model | numberCheckerManualEntryRow = newNumberCheckerManualEntryRow }


modifyNumberCheckerRowsInternal : Bool -> Int -> Int -> List AnnotatedNumberCheckerEntry -> List AnnotatedNumberCheckerEntry
modifyNumberCheckerRowsInternal foundEntry offset entryNumber currentRows =
    case currentRows of
        [] ->
            []

        firstRow :: restRows ->
            if foundEntry || firstRow.entryNumber == entryNumber then
                { firstRow | actual = firstRow.actual + offset } :: modifyNumberCheckerRowsInternal True offset entryNumber restRows

            else
                firstRow :: modifyNumberCheckerRowsInternal False offset entryNumber restRows


modifyNumberCheckerRows : Int -> Int -> Model -> Model
modifyNumberCheckerRows offset entryNumber model =
    { model
        | numberCheckerEntries =
            modifyNumberCheckerRowsInternal False offset entryNumber model.numberCheckerEntries
                |> reannotate
    }
