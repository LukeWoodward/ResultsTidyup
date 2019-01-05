module ProblemsView exposing (problemsView)

import DataStructures exposing (MinorProblemFix(..))
import Html exposing (Html, button, div, h4, li, text, ul)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Msg exposing (Msg)
import Problems exposing (MinorProblem(..), Problem, ProblemsContainer, problemToString)


majorProblemView : Problem -> Html Msg
majorProblemView problem =
    li [] [ text (problemToString problem) ]


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
            [ h4 [] [ text problemsHeader ]
            , ul [] (List.map majorProblemView problems)
            ]


minorProblemView : MinorProblem -> Html Msg
minorProblemView minorProblem =
    case minorProblem of
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
                    , onClick (Msg.FixMinorProblem (RemoveDuplicateScans position athlete))
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
                    , onClick (Msg.FixMinorProblem (RemoveUnassociatedAthlete athlete))
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
                    , onClick (Msg.FixMinorProblem (RemoveUnassociatedFinishToken position))
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
                    , onClick (Msg.FixMinorProblem (RemoveScansBeforeEventStart eventStartTimeMillis))
                    ]
                    [ text "Remove barcodes scanned before event start" ]
                ]


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
            [ h4 [] [ text minorProblemsHeader ]
            , ul [] (List.map minorProblemView minorProblems)
            ]


problemsView : ProblemsContainer -> Html Msg
problemsView problems =
    div []
        [ majorProblemsView problems.problems
        , minorProblemsView problems.minorProblems
        ]
