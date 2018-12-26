module NumberCheckerView exposing (numberCheckerView)

import Html exposing (Html, button, div, h3, table, tbody, td, text, tr)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Msg exposing (Msg(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import ViewCommon exposing (intCell, tableHeaders)


deleteNumberCheckerEntryButtonCell : Int -> Html Msg
deleteNumberCheckerEntryButtonCell entryNumber =
    td
        [ class "delete-button-cell" ]
        [ button
            [ type_ "button"
            , class "btn btn-primary btn-xs"
            , onClick (DeleteNumberCheckerRow entryNumber)
            ]
            [ text "Delete " ]
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
        , deleteNumberCheckerEntryButtonCell entry.entryNumber
        ]


noNumberCheckerData : Html a
noNumberCheckerData =
    div [ class "alert alert-info" ]
        [ text "No number-checker data has been loaded" ]


numberCheckerTable : List AnnotatedNumberCheckerEntry -> Html Msg
numberCheckerTable entries =
    table
        [ class "table table-bordered table-hover number-checker-table" ]
        [ tableHeaders [ "Stopwatch 1", "+/−", "Stopwatch 2", "+/−", "Finish tokens", "+/−", "" ]
        , tbody
            []
            (List.map numberCheckerRow entries)
        ]


numberCheckerView : List AnnotatedNumberCheckerEntry -> Maybe Int -> Html Msg
numberCheckerView entries lastHeight =
    div
        []
        [ h3 [] [ text "Number checker" ]
        , if List.isEmpty entries then
            noNumberCheckerData

          else
            numberCheckerTable entries
        ]
