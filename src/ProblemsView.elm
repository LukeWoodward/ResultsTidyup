module ProblemsView exposing (problemsView)

import DataStructures exposing (ProblemFix(..))
import Html exposing (Html, button, div, h4, li, text, ul)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Msg exposing (Msg)
import Problems exposing (FixableProblem(..), Problem, ProblemsContainer, problemToString)


nonFixableProblemView : Problem -> Html Msg
nonFixableProblemView problem =
    li [] [ text (problemToString problem) ]


nonFixableProblemsView : List Problem -> Html Msg
nonFixableProblemsView problems =
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
            [ h4 [] [ text problemsHeader ]
            , ul [] (List.map nonFixableProblemView problems)
            ]


fixableProblemView : FixableProblem -> Html Msg
fixableProblemView fixableProblem =
    case fixableProblem of
        AthleteInSamePositionMultipleTimes athlete position ->
            li []
                [ text "Athlete barcode "
                , text athlete
                , text " has been scanned with finish token "
                , text (String.fromInt position)
                , text " more than once "
                , button
                    [ type_ "button"
                    , class "btn btn-primary btn-sm"
                    , onClick (Msg.FixProblem (RemoveDuplicateScans position athlete))
                    ]
                    [ text "Remove duplicates" ]
                ]

        AthleteWithAndWithoutPosition athlete position ->
            li []
                [ text "Athlete "
                , text athlete
                , text " has been scanned with finish token "
                , text (String.fromInt position)
                , text " and also without a corresponding finish token "
                , button
                    [ type_ "button"
                    , class "btn btn-primary btn-sm"
                    , onClick (Msg.FixProblem (RemoveUnassociatedAthlete athlete))
                    ]
                    [ text "Remove unassociated athlete barcode scan" ]
                ]

        PositionWithAndWithoutAthlete position athlete ->
            li []
                [ text "Finish token "
                , text (String.fromInt position)
                , text " has been scanned with athlete barcode "
                , text athlete
                , text " and also without a corresponding athlete barcode "
                , button
                    [ type_ "button"
                    , class "btn btn-primary btn-sm"
                    , onClick (Msg.FixProblem (RemoveUnassociatedFinishToken position))
                    ]
                    [ text "Remove unassociated finish token scan" ]
                ]

        BarcodesScannedBeforeEventStart number eventStartTimeMillis eventStart ->
            let
                barcodesString : String
                barcodesString =
                    if number == 1 then
                        "One barcode was"

                    else
                        String.fromInt number ++ " barcodes were"
            in
            li []
                [ text barcodesString
                , text " scanned before the event start ("
                , text eventStart
                , text ") "
                , button
                    [ type_ "button"
                    , class "btn btn-primary btn-sm"
                    , onClick (Msg.FixProblem (RemoveScansBeforeEventStart eventStartTimeMillis))
                    ]
                    [ text "Remove barcodes scanned before event start" ]
                ]


fixableProblemsView : List FixableProblem -> Html Msg
fixableProblemsView fixableProblems =
    if List.isEmpty fixableProblems then
        div [] []

    else
        let
            fixableProblemsHeader : String
            fixableProblemsHeader =
                if List.length fixableProblems == 1 then
                    "The following fixable problem was found:"

                else
                    "The following fixable problems were found:"
        in
        div [ class "alert alert-warning" ]
            [ h4 [] [ text fixableProblemsHeader ]
            , ul [] (List.map fixableProblemView fixableProblems)
            ]


problemsView : ProblemsContainer -> Html Msg
problemsView problems =
    div []
        [ nonFixableProblemsView problems.problems
        , fixableProblemsView problems.fixableProblems
        ]
