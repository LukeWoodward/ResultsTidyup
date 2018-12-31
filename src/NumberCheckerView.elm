module NumberCheckerView exposing (numberCheckerView)

import Html exposing (Attribute, Html, button, div, h3, input, table, tbody, td, text, tr)
import Html.Attributes exposing (class, colspan, disabled, id, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onMouseEnter, onMouseLeave)
import Json.Decode as Json
import Model exposing (NumberCheckerManualEntryRow, NumericEntry)
import Msg exposing (Msg(..), NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import ViewCommon exposing (intCell, tableHeaders)


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


actionButtonsCell : Int -> Html Msg
actionButtonsCell entryNumber =
    td
        [ class "delete-button-cell" ]
        [ button
            [ type_ "button"
            , class "btn btn-primary btn-xs number-checker-command"
            , onClick (EditNumberCheckerRow entryNumber)
            ]
            [ text "Edit" ]
        , button
            [ type_ "button"
            , class "btn btn-primary btn-xs number-checker-command"
            , onClick (DeleteNumberCheckerRow entryNumber)
            ]
            [ text "Delete" ]
        ]


deltaCell : Int -> Html a
deltaCell delta =
    if delta == 0 then
        td [ class "zero-delta" ] [ text "0" ]

    else
        let
            stringDelta : String
            stringDelta =
                if delta > 0 then
                    "+" ++ String.fromInt delta

                else
                    "−" ++ String.fromInt -delta
        in
        td [ class "nonzero-delta" ] [ text stringDelta ]


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


numberCheckerRow : AnnotatedNumberCheckerEntry -> Html Msg
numberCheckerRow entry =
    tr
        [ onMouseEnter (MouseEnterNumberCheckerRow entry.entryNumber)
        , onMouseLeave (MouseLeaveNumberCheckerRow entry.entryNumber)
        ]
        [ intCell entry.stopwatch1
        , deltaCell entry.stopwatch1Delta
        , intCell entry.stopwatch2
        , deltaCell entry.stopwatch2Delta
        , intCell entry.finishTokens
        , deltaCell entry.finishTokensDelta
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


enterNewRow : NumberCheckerManualEntryRow -> Html Msg
enterNewRow manualEntryRow =
    tr [ class "number-checker-manual-entry-row" ]
        [ td [ colspan 2 ]
            [ input
                [ type_ "text"
                , id "number-checker-stopwatch-1"
                , class (manualEntryFieldClass manualEntryRow.stopwatch1)
                , value manualEntryRow.stopwatch1.enteredValue
                , onInput (NumberCheckerFieldChanged Stopwatch1)
                , onEnterKeypress AddNumberCheckerRow
                ]
                []
            ]
        , td [ colspan 2 ]
            [ input
                [ type_ "text"
                , class (manualEntryFieldClass manualEntryRow.stopwatch2)
                , value manualEntryRow.stopwatch2.enteredValue
                , onInput (NumberCheckerFieldChanged Stopwatch2)
                , onEnterKeypress AddNumberCheckerRow
                ]
                []
            ]
        , td [ colspan 2 ]
            [ input
                [ type_ "text"
                , class (manualEntryFieldClass manualEntryRow.finishTokens)
                , value manualEntryRow.finishTokens.enteredValue
                , onInput (NumberCheckerFieldChanged FinishTokens)
                , onEnterKeypress AddNumberCheckerRow
                ]
                []
            ]
        , td []
            [ button
                [ type_ "button"
                , class "btn btn-primary btn-xs"
                , disabled (isManualEntryAddButtonDisabled manualEntryRow)
                , onClick AddNumberCheckerRow
                ]
                [ text "Add" ]
            ]
        ]


numberCheckerTable : List AnnotatedNumberCheckerEntry -> NumberCheckerManualEntryRow -> Html Msg
numberCheckerTable entries manualEntryRow =
    let
        emptyRows : List (Html Msg)
        emptyRows =
            if List.isEmpty entries then
                [ tr []
                    [ td [ colspan 7 ]
                        [ div [ class "no-number-checker-entries" ]
                            [ text "No number-checker data has been loaded." ]
                        ]
                    ]
                ]

            else
                []
    in
    table
        [ class "table table-bordered table-hover number-checker-table" ]
        [ tableHeaders [ "Stopwatch 1", "+/−", "Stopwatch 2", "+/−", "Finish tokens", "+/−", "" ]
        , tbody
            []
            (emptyRows ++ List.map numberCheckerRow entries ++ [ enterNewRow manualEntryRow ])
        ]


numberCheckerView : List AnnotatedNumberCheckerEntry -> NumberCheckerManualEntryRow -> Maybe Int -> Html Msg
numberCheckerView entries manualEntryRow lastHeight =
    div
        []
        [ h3 [] [ text "Number checker" ]
        , numberCheckerTable entries manualEntryRow
        ]
