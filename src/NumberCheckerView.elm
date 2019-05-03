module NumberCheckerView exposing (firstManualEntryCellId, numberCheckerView)

import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Html exposing (Attribute, Html, div, h3, input, text)
import Html.Attributes exposing (class, colspan, disabled, id, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onMouseEnter, onMouseLeave)
import Json.Decode as Json
import Model exposing (NumberCheckerManualEntryRow)
import Msg exposing (Msg(..), NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import NumericEntry exposing (NumericEntry)
import ViewCommon exposing (intCell, smallButton, tableHeaders)


firstManualEntryCellId : String
firstManualEntryCellId =
    "number-checker-stopwatch-1"


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


numberCheckerUnderlineClass : Int -> Maybe Int -> String
numberCheckerUnderlineClass numberCheckerId highlightedNumberCheckerId =
    let
        highlightClassPrefix : String
        highlightClassPrefix =
            if highlightedNumberCheckerId == Just numberCheckerId then
                "highlighted "

            else
                ""
    in
    highlightClassPrefix ++ "underlined number-checker-row-" ++ String.fromInt numberCheckerId


numberCheckerUnderlineAttributes : Maybe String -> Maybe Int -> Maybe Int -> List (Html.Attribute a)
numberCheckerUnderlineAttributes className numberCheckerId highlightedNumberCheckerId =
    case ( className, numberCheckerId ) of
        ( Just someClass, Just someNumberCheckerId ) ->
            [ class (someClass ++ " " ++ numberCheckerUnderlineClass someNumberCheckerId highlightedNumberCheckerId) ]

        ( Nothing, Just someNumberCheckerId ) ->
            [ class (numberCheckerUnderlineClass someNumberCheckerId highlightedNumberCheckerId) ]

        ( Just someClass, Nothing ) ->
            [ class someClass ]

        ( Nothing, Nothing ) ->
            []


numberCheckerRow : AnnotatedNumberCheckerEntry -> Table.Row Msg
numberCheckerRow entry =
    Table.tr
        [ Table.rowAttr (onMouseEnter (MouseEnterNumberCheckerRow entry.entryNumber))
        , Table.rowAttr (onMouseLeave (MouseLeaveNumberCheckerRow entry.entryNumber))
        ]
        [ intCell entry.stopwatch1
        , deltaCell entry.stopwatch1Delta
        , intCell entry.stopwatch2
        , deltaCell entry.stopwatch2Delta
        , intCell entry.finishTokens
        , deltaCell entry.finishTokensDelta
        , actualEntryCell entry
        , actionButtonsCell entry.entryNumber
        ]


manualEntryFieldClass : NumericEntry -> String
manualEntryFieldClass entry =
    if entry.enteredValue /= "" && entry.parsedValue == Nothing then
        "number-checker-manual-entry number-error"

    else
        "number-checker-manual-entry"


isManualEntryAddButtonDisabled : NumberCheckerManualEntryRow -> Bool
isManualEntryAddButtonDisabled manualEntryRow =
    (manualEntryRow.stopwatch1.parsedValue == Nothing)
        || (manualEntryRow.stopwatch2.parsedValue == Nothing)
        || (manualEntryRow.finishTokens.parsedValue == Nothing)


enterNewRow : NumberCheckerManualEntryRow -> Table.Row Msg
enterNewRow manualEntryRow =
    Table.tr [ Table.rowAttr (class "number-checker-manual-entry-row") ]
        [ Table.td [ Table.cellAttr (colspan 2) ]
            [ input
                [ type_ "text"
                , id firstManualEntryCellId
                , class (manualEntryFieldClass manualEntryRow.stopwatch1)
                , value manualEntryRow.stopwatch1.enteredValue
                , onInput (NumberCheckerFieldChanged Stopwatch1)
                , onEnterKeypress AddNumberCheckerRow
                ]
                []
            ]
        , Table.td [ Table.cellAttr (colspan 2) ]
            [ input
                [ type_ "text"
                , class (manualEntryFieldClass manualEntryRow.stopwatch2)
                , value manualEntryRow.stopwatch2.enteredValue
                , onInput (NumberCheckerFieldChanged Stopwatch2)
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
        , thead = tableHeaders [ "Stopwatch 1", plusOrMinus, "Stopwatch 2", plusOrMinus, "Finish tokens", plusOrMinus, "Actual", "" ]
        , tbody =
            Table.tbody
                []
                (emptyRows ++ List.map numberCheckerRow entries ++ [ enterNewRow manualEntryRow ])
        }


numberCheckerView : List AnnotatedNumberCheckerEntry -> NumberCheckerManualEntryRow -> Maybe Int -> Html Msg
numberCheckerView entries manualEntryRow lastHeight =
    div
        []
        [ h3 [] [ text "Number checker" ]
        , numberCheckerTable entries manualEntryRow
        ]
