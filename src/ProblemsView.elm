module ProblemsView exposing (problemsView)

import Html exposing (Html, div, h4, text)
import Html.Attributes exposing (class)
import Msg exposing (Msg)
import Problems exposing (MinorProblem, Problem, ProblemsContainer, minorProblemToString, problemToString)


majorProblemView : Problem -> Html Msg
majorProblemView problem =
    div [] [ text (problemToString problem) ]


majorProblemsView : List Problem -> Html Msg
majorProblemsView problems =
    if List.isEmpty problems then
        div [] []

    else
        let
            problemsHeader : String
            problemsHeader =
                if List.length problems == 1 then
                    "The following problem was found:"

                else
                    "The following problems were found:"
        in
        div [ class "alert alert-danger" ]
            (h4 [] [ text problemsHeader ] :: List.map majorProblemView problems)


minorProblemView : MinorProblem -> Html Msg
minorProblemView minorProblem =
    div [] [ text (minorProblemToString minorProblem) ]


minorProblemsView : List MinorProblem -> Html Msg
minorProblemsView minorProblems =
    if List.isEmpty minorProblems then
        div [] []

    else
        let
            minorProblemsHeader : String
            minorProblemsHeader =
                if List.length minorProblems == 1 then
                    "The following minor problem was found:"

                else
                    "The following minor problems were found:"
        in
        div [ class "alert alert-warning" ]
            (h4 [] [ text minorProblemsHeader ] :: List.map minorProblemView minorProblems)


problemsView : ProblemsContainer -> Html Msg
problemsView problems =
    div []
        [ majorProblemsView problems.problems
        , minorProblemsView problems.minorProblems
        ]
