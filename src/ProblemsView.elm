module ProblemsView exposing (problemsView)

import Bootstrap.Alert as Alert
import DataStructures exposing (ProblemFix(..), WhichStopwatch(..))
import Html exposing (Html, button, div, h4, li, text, ul)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Msg exposing (Msg)
import Problems exposing (FixableProblem(..), Problem, ProblemsContainer, problemToString)
import ViewCommon exposing (smallButton)


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
        Alert.simpleDanger
            []
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
                , smallButton (Msg.FixProblem (RemoveDuplicateScans position athlete)) [] "Remove duplicates"
                ]

        AthleteWithAndWithoutPosition athlete position ->
            li []
                [ text "Athlete "
                , text athlete
                , text " has been scanned with finish token "
                , text (String.fromInt position)
                , text " and also without a corresponding finish token "
                , smallButton (Msg.FixProblem (RemoveUnassociatedAthlete athlete)) [] "Remove unassociated athlete barcode scan"
                ]

        PositionWithAndWithoutAthlete position athlete ->
            li []
                [ text "Finish token "
                , text (String.fromInt position)
                , text " has been scanned with athlete barcode "
                , text athlete
                , text " and also without a corresponding athlete barcode "
                , smallButton (Msg.FixProblem (RemoveUnassociatedFinishToken position)) [] "Remove unassociated finish token scan"
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
                , smallButton (Msg.FixProblem (RemoveScansBeforeEventStart eventStartTimeMillis)) [] "Remove barcodes scanned before event start"
                ]

        StopwatchTimeOffset offset ->
            let
                offsetDescription : String
                offsetDescription =
                    if abs offset == 1 then
                        "1 second"

                    else
                        String.fromInt (abs offset) ++ " seconds"

                laterStopwatch : String
                laterStopwatch =
                    if offset < 0 then
                        "stopwatch 1"

                    else
                        "stopwatch 2"

                stopwatch1AdjustText : String
                stopwatch1AdjustText =
                    if offset < 0 then
                        "Stopwatch 1 is slow - add " ++ offsetDescription

                    else
                        "Stopwatch 1 is fast - take off " ++ offsetDescription

                stopwatch2AdjustText : String
                stopwatch2AdjustText =
                    if offset < 0 then
                        "Stopwatch 2 is fast - take off " ++ offsetDescription

                    else
                        "Stopwatch 2 is slow - add " ++ offsetDescription
            in
            li []
                [ text "There seems to be a difference of "
                , text offsetDescription
                , text " between the stopwatches, with "
                , text laterStopwatch
                , text " being the one started later."
                , smallButton (Msg.FixProblem (AdjustStopwatch StopwatchOne -offset)) [] stopwatch1AdjustText
                , text " "
                , smallButton (Msg.FixProblem (AdjustStopwatch StopwatchTwo offset)) [] stopwatch2AdjustText
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
        Alert.simpleWarning
            []
            [ h4 [] [ text fixableProblemsHeader ]
            , ul [] (List.map fixableProblemView fixableProblems)
            ]


problemsView : ProblemsContainer -> Html Msg
problemsView problems =
    div []
        [ nonFixableProblemsView problems.problems
        , fixableProblemsView problems.fixableProblems
        ]
