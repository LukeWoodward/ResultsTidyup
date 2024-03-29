module NumberCheckerView exposing (firstManualEntryCellId, numberCheckerView)

import Bootstrap.Table as Table
import DataEntry exposing (IntegerEntry)
import Html exposing (Attribute, Html, div, h3, input, text)
import Html.Attributes exposing (class, colspan, disabled, id, type_, value)
import Html.Events exposing (keyCode, on, onInput, onMouseEnter, onMouseLeave)
import Json.Decode as Json
import Model exposing (NumberCheckerManualEntryRow)
import Msg exposing (Msg(..), NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import ViewCommon exposing (intCell, smallButton, tableHeaders)


firstManualEntryCellId : String
firstManualEntryCellId =
    "number-checker-timer-1"


minus : String
minus =
    "−"


plusOrMinus : String
plusOrMinus =
    "+/" ++ minus


onEnterKeypress : Msg -> Attribute Msg
onEnterKeypress msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not an Enter keypress"
    in
    on "keydown" (Json.andThen isEnter keyCode)


actionButtonsCell : Int -> Table.Cell Msg
actionButtonsCell entryNumber =
    Table.td
        [ Table.cellAttr (class "delete-button-cell") ]
        [ smallButton (EditNumberCheckerRow entryNumber) [ class "number-checker-command" ] "Edit"
        , smallButton (DeleteNumberCheckerRow entryNumber) [ class "number-checker-command" ] "Delete"
        ]


deltaCell : Int -> Table.Cell Msg
deltaCell delta =
    if delta == 0 then
        Table.td [ Table.cellAttr (class "zero-delta") ] [ text "0" ]

    else
        let
            stringDelta : String
            stringDelta =
                if delta > 0 then
                    "+" ++ String.fromInt delta

                else
                    minus ++ String.fromInt -delta
        in
        Table.td [ Table.cellAttr (class "nonzero-delta") ] [ text stringDelta ]


actualEntryCell : AnnotatedNumberCheckerEntry -> Table.Cell Msg
actualEntryCell entry =
    Table.td []
        [ text (String.fromInt entry.actual)
        , text " "
        , smallButton (IncrementNumberCheckerRowActualCount entry.entryNumber) [ class "number-checker-command" ] "+"
        , smallButton (DecrementNumberCheckerRowActualCount entry.entryNumber) [ class "number-checker-command" ] minus
        ]


numberCheckerRow : AnnotatedNumberCheckerEntry -> Table.Row Msg
numberCheckerRow entry =
    Table.tr
        [ Table.rowAttr (onMouseEnter (MouseEnterNumberCheckerRow entry.entryNumber))
        , Table.rowAttr (onMouseLeave (MouseLeaveNumberCheckerRow entry.entryNumber))
        ]
        [ intCell entry.timer1
        , deltaCell entry.timer1Delta
        , intCell entry.timer2
        , deltaCell entry.timer2Delta
        , intCell entry.finishTokens
        , deltaCell entry.finishTokensDelta
        , actualEntryCell entry
        , actionButtonsCell entry.entryNumber
        ]


manualEntryFieldClass : IntegerEntry -> String
manualEntryFieldClass entry =
    if entry.enteredValue /= "" && entry.parsedValue == Nothing then
        "number-checker-manual-entry number-error"

    else
        "number-checker-manual-entry"


isManualEntryAddButtonDisabled : NumberCheckerManualEntryRow -> Bool
isManualEntryAddButtonDisabled manualEntryRow =
    (manualEntryRow.timer1.parsedValue == Nothing)
        || (manualEntryRow.timer2.parsedValue == Nothing)
        || (manualEntryRow.finishTokens.parsedValue == Nothing)


enterNewRow : NumberCheckerManualEntryRow -> Table.Row Msg
enterNewRow manualEntryRow =
    Table.tr [ Table.rowAttr (class "number-checker-manual-entry-row") ]
        [ Table.td [ Table.cellAttr (colspan 2) ]
            [ input
                [ type_ "text"
                , id firstManualEntryCellId
                , class (manualEntryFieldClass manualEntryRow.timer1)
                , value manualEntryRow.timer1.enteredValue
                , onInput (NumberCheckerFieldChanged Timer1)
                , onEnterKeypress AddNumberCheckerRow
                ]
                []
            ]
        , Table.td [ Table.cellAttr (colspan 2) ]
            [ input
                [ type_ "text"
                , class (manualEntryFieldClass manualEntryRow.timer2)
                , value manualEntryRow.timer2.enteredValue
                , onInput (NumberCheckerFieldChanged Timer2)
                , onEnterKeypress AddNumberCheckerRow
                ]
                []
            ]
        , Table.td [ Table.cellAttr (colspan 2) ]
            [ input
                [ type_ "text"
                , class (manualEntryFieldClass manualEntryRow.finishTokens)
                , value manualEntryRow.finishTokens.enteredValue
                , onInput (NumberCheckerFieldChanged FinishTokens)
                , onEnterKeypress AddNumberCheckerRow
                ]
                []
            ]
        , Table.td [] []
        , Table.td []
            [ smallButton
                AddNumberCheckerRow
                [ class "number-checker-command"
                , disabled (isManualEntryAddButtonDisabled manualEntryRow)
                ]
                "Add"
            ]
        ]


numberCheckerTable : List AnnotatedNumberCheckerEntry -> NumberCheckerManualEntryRow -> Html Msg
numberCheckerTable entries manualEntryRow =
    let
        emptyRows : List (Table.Row Msg)
        emptyRows =
            if List.isEmpty entries then
                [ Table.tr []
                    [ Table.td [ Table.cellAttr (colspan 8) ]
                        [ div [ class "no-number-checker-entries" ]
                            [ text "No number-checker data has been loaded." ]
                        ]
                    ]
                ]

            else
                []
    in
    Table.table
        { options = [ Table.bordered, Table.small, Table.hover, Table.attr (class "number-checker-table") ]
        , thead = tableHeaders [ "Timer 1", plusOrMinus, "Timer 2", plusOrMinus, "Finish tokens", plusOrMinus, "Actual", "" ]
        , tbody =
            Table.tbody
                []
                (emptyRows ++ List.map numberCheckerRow entries ++ [ enterNewRow manualEntryRow ])
        }


numberCheckerView : List AnnotatedNumberCheckerEntry -> NumberCheckerManualEntryRow -> Html Msg
numberCheckerView entries manualEntryRow =
    div
        []
        [ h3 [] [ text "Number checker" ]
        , numberCheckerTable entries manualEntryRow
        ]
