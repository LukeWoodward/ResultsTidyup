module ProblemsView exposing (problemsView)

import Bootstrap.Alert as Alert
import DataStructures exposing (ProblemFix(..), WhichStopwatch(..))
import Html exposing (Html, button, div, h4, li, text, ul)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Msg exposing (Msg)
import Problems exposing (FixableProblem(..), NonFixableProblem(..), Problem(..))
import ViewCommon exposing (smallButton)


nonFixableProblemToString : NonFixableProblem -> String
nonFixableProblemToString problem =
    case problem of
        InconsistentBarcodeScannerDates earlierDate laterDate ->
            "Inconsistent dates were found among the barcode scanner files (" ++ earlierDate ++ " and " ++ laterDate ++ ").  Please check that you have uploaded files from the same date"

        AthleteWithMultiplePositions athlete positions ->
            "Athlete barcode " ++ athlete ++ " has been scanned with more than one finish token: " ++ String.join ", " (List.map String.fromInt positions)

        PositionWithMultipleAthletes position athletes ->
            "Multiple athlete barcodes have been scanned with finish token " ++ String.fromInt position ++ ": " ++ String.join ", " athletes

        PositionOffEndOfTimes numberOfTimes maxPosition ->
            "The highest finish token scanned was " ++ String.fromInt maxPosition ++ " but there are only " ++ String.fromInt numberOfTimes ++ " times recorded on the stopwatch(es)"

        AthleteMissingPosition athlete ->
            "Athlete barcode " ++ athlete ++ " was scanned without a corresponding finish token"

        PositionMissingAthlete position ->
            "Finish token " ++ String.fromInt position ++ " was scanned without a corresponding athlete barcode"

        MisScan misScannedText ->
            "An unrecognised item '" ++ misScannedText ++ "' was scanned"

        UnrecognisedBarcodeScannerLine line ->
            "The line '" ++ line ++ "' in a barcode scanner file was not recognised"

        StopwatchesInconsistentWithNumberChecker ->
            "TODO"

        StopwatchesAndFinishTokensInconsistentWithNumberChecker ->
            "TODO"


nonFixableProblemView : NonFixableProblem -> Html Msg
nonFixableProblemView problem =
    li [] [ text (nonFixableProblemToString problem) ]


nonFixableProblemsView : List (Html Msg) -> Html Msg
nonFixableProblemsView nonFixableProblems =
    if List.isEmpty nonFixableProblems then
        div [] []

    else
        let
            problemsHeader : String
            problemsHeader =
                if List.length nonFixableProblems == 1 then
                    "The following problem was found:"

                else
                    "The following problems were found:"
        in
        Alert.simpleDanger
            []
            [ h4 [] [ text problemsHeader ]
            , ul [] nonFixableProblems
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


fixableProblemsView : List (Html Msg) -> Html Msg
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
            , ul [] fixableProblems
            ]


problemView : Problem -> ( Maybe (Html Msg), Maybe (Html Msg) )
problemView problem =
    case problem of
        Fixable fixableProblem ->
            ( Just (fixableProblemView fixableProblem), Nothing )

        NonFixable nonFixableProblem ->
            ( Nothing, Just (nonFixableProblemView nonFixableProblem) )


problemsView : List Problem -> Html Msg
problemsView problems =
    let
        splitProblems : List ( Maybe (Html Msg), Maybe (Html Msg) )
        splitProblems =
            List.map problemView problems

        fixableProblems : List (Html Msg)
        fixableProblems =
            List.filterMap Tuple.first splitProblems

        nonFixableProblems : List (Html Msg)
        nonFixableProblems =
            List.filterMap Tuple.second splitProblems
    in
    div []
        [ fixableProblemsView fixableProblems
        , nonFixableProblemsView nonFixableProblems
        ]
