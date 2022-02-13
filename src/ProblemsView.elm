module ProblemsView exposing (scannerProblemsView, timerProblemsView)

import Bootstrap.Alert as Alert
import Html exposing (Html, button, div, h4, li, span, text, ul)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Msg exposing (Msg)
import ProblemFixing exposing (ProblemFix(..), ProblemIgnorance(..))
import Problems
    exposing
        ( AthleteAndPositionPair
        , AthleteWithAndWithoutPositionProblem
        , AthleteWithMultiplePositionsProblem
        , BarcodesScannedBeforeEventStartProblem
        , MisScannedAthleteBarcodeProblem
        , PositionAndTime
        , PositionOffEndOfTimesProblem
        , PositionWithMultipleAthletesProblem
        , Problems
        )
import TimeHandling exposing (formatTime)
import Timer exposing (WhichTimer(..))
import ViewCommon exposing (athleteLink, smallButton)


warningAlert : List (Html Msg) -> Html Msg
warningAlert contents =
    Alert.simpleWarning [ class "warning-condensed" ] contents


dangerAlert : List (Html Msg) -> Html Msg
dangerAlert contents =
    Alert.simpleDanger [ class "warning-condensed" ] contents


barcodesScannedBeforeEventStartProblemView : BarcodesScannedBeforeEventStartProblem -> Html Msg
barcodesScannedBeforeEventStartProblemView problem =
    let
        barcodesStringPrefix : String
        barcodesStringPrefix =
            if problem.numberOfScansBeforeEventStart == 1 then
                "One barcode was"

            else
                String.fromInt problem.numberOfScansBeforeEventStart ++ " barcodes were"

        problemText : String
        problemText =
            barcodesStringPrefix
                ++ " scanned before the event start ("
                ++ problem.eventStartTime
                ++ ") "
    in
    warningAlert
        [ text problemText
        , smallButton (Msg.FixProblem (RemoveScansBeforeEventStart problem.eventStartDateTimeMillis)) [] "Remove barcodes scanned before event start"
        ]


generateButton : ProblemFix -> String -> Html Msg
generateButton problemFix buttonLabel =
    smallButton (Msg.FixProblem problemFix) [] buttonLabel


athleteAndPositionRow : (AthleteAndPositionPair -> ProblemFix) -> String -> AthleteAndPositionPair -> Html Msg
athleteAndPositionRow problemFixGenerator buttonLabel pair =
    li []
        [ athleteLink pair.athlete
        , text (" and " ++ String.fromInt pair.position ++ " ")
        , generateButton (problemFixGenerator pair) buttonLabel
        ]


athleteWithAndWithoutPositionRow : (AthleteWithAndWithoutPositionProblem -> ProblemFix) -> (Int -> String) -> AthleteWithAndWithoutPositionProblem -> Html Msg
athleteWithAndWithoutPositionRow problemFixGenerator buttonLabel athleteWithAndWithoutPosition =
    li []
        [ athleteLink athleteWithAndWithoutPosition.athlete
        , text (" and " ++ String.fromInt athleteWithAndWithoutPosition.position ++ " ")
        , generateButton (problemFixGenerator athleteWithAndWithoutPosition) (buttonLabel athleteWithAndWithoutPosition.count)
        ]


athletesInSamePositionMultipleTimesView : List AthleteAndPositionPair -> Html Msg
athletesInSamePositionMultipleTimesView athleteAndPositionPairs =
    let
        buttonLabel : String
        buttonLabel =
            "Remove duplicate scans"

        problemFixGenerator : AthleteAndPositionPair -> ProblemFix
        problemFixGenerator pair =
            RemoveDuplicateScans pair.position pair.athlete
    in
    case athleteAndPositionPairs of
        [ pair ] ->
            warningAlert
                [ text "Athlete "
                , athleteLink pair.athlete
                , text (" has been scanned with finish token " ++ String.fromInt pair.position ++ " more than once. ")
                , generateButton (problemFixGenerator pair) buttonLabel
                ]

        _ ->
            warningAlert
                [ text "The following athlete barcodes have been scanned multiple times with same finish token more than once:"
                , ul [] (List.map (athleteAndPositionRow problemFixGenerator buttonLabel) athleteAndPositionPairs)
                ]


athletesWithAndWithoutPositionView : List AthleteWithAndWithoutPositionProblem -> Html Msg
athletesWithAndWithoutPositionView athletesWithAndWithoutPosition =
    let
        buttonLabel : Int -> String
        buttonLabel count =
            if count == 1 then
                "Remove unassociated athlete scan"

            else
                "Remove unassociated athlete scans (" ++ String.fromInt count ++ ")"

        problemFixGenerator : AthleteWithAndWithoutPositionProblem -> ProblemFix
        problemFixGenerator athleteWithAndWithoutPosition =
            RemoveUnassociatedAthlete athleteWithAndWithoutPosition.athlete
    in
    case athletesWithAndWithoutPosition of
        [ singleItem ] ->
            warningAlert
                [ text "Athlete "
                , athleteLink singleItem.athlete
                , text
                    (" has been scanned with finish token "
                        ++ String.fromInt singleItem.position
                        ++ " and also without a finish token. "
                    )
                , generateButton (problemFixGenerator singleItem) (buttonLabel singleItem.count)
                ]

        _ ->
            warningAlert
                [ text "The following athlete barcodes have been scanned with a finish token and without a finish token:"
                , ul [] (List.map (athleteWithAndWithoutPositionRow problemFixGenerator buttonLabel) athletesWithAndWithoutPosition)
                ]


timerTimeOffsetView : Int -> Maybe (Html Msg)
timerTimeOffsetView offset =
    if offset == 0 then
        Nothing

    else
        let
            offsetDescription : String
            offsetDescription =
                if abs offset == 1 then
                    "1 second"

                else
                    String.fromInt (abs offset) ++ " seconds"

            laterTimer : String
            laterTimer =
                if offset < 0 then
                    "timer 1"

                else
                    "timer 2"

            timer1AdjustText : String
            timer1AdjustText =
                if offset < 0 then
                    "Timer 1 is slow - add " ++ offsetDescription

                else
                    "Timer 1 is fast - take off " ++ offsetDescription

            timer2AdjustText : String
            timer2AdjustText =
                if offset < 0 then
                    "Timer 2 is fast - take off " ++ offsetDescription

                else
                    "Timer 2 is slow - add " ++ offsetDescription
        in
        warningAlert
            [ text "There seems to be a difference of "
            , text offsetDescription
            , text " between the timers, with "
            , text laterTimer
            , text " being the one started later."
            , smallButton (Msg.FixProblem (AdjustTimer TimerOne -offset)) [] timer1AdjustText
            , text " "
            , smallButton (Msg.FixProblem (AdjustTimer TimerTwo offset)) [] timer2AdjustText
            , text " "
            , smallButton (Msg.IgnoreProblem IgnoreTimerTimeOffsets) [] "Ignore timer time offset"
            ]
            |> Just


positionAndTimeToString : PositionAndTime -> String
positionAndTimeToString { position, time } =
    let
        suffix : String
        suffix =
            case time of
                Just someTime ->
                    " (" ++ formatTime someTime ++ ")"

                Nothing ->
                    ""
    in
    String.fromInt position ++ suffix


athletesWithMultiplePositionsView : List AthleteWithMultiplePositionsProblem -> Html Msg
athletesWithMultiplePositionsView athletesWithMultiplePositions =
    let
        commaSeparate : List PositionAndTime -> String
        commaSeparate positionsAndTimes =
            String.join ", " (List.map positionAndTimeToString positionsAndTimes)

        rowGenerator : AthleteWithMultiplePositionsProblem -> Html Msg
        rowGenerator athleteWithMultiplePositions =
            li []
                [ athleteLink athleteWithMultiplePositions.athlete
                , text (" and " ++ commaSeparate athleteWithMultiplePositions.positionsAndTimes)
                ]
    in
    case athletesWithMultiplePositions of
        [ singleAthlete ] ->
            dangerAlert
                [ text "Athlete barcode "
                , athleteLink singleAthlete.athlete
                , text (" has been scanned with more than one finish token: " ++ commaSeparate singleAthlete.positionsAndTimes ++ ".")
                ]

        _ ->
            dangerAlert
                [ text "The following athlete barcodes have been scanned with more than one finish token:"
                , ul [] (List.map rowGenerator athletesWithMultiplePositions)
                ]


positionsWithMultipleAthletesView : List PositionWithMultipleAthletesProblem -> Html Msg
positionsWithMultipleAthletesView positionsWithMultipleAthletes =
    let
        rowGenerator : PositionWithMultipleAthletesProblem -> Html Msg
        rowGenerator positionWithMultipleAthletes =
            li [] [ text (String.fromInt positionWithMultipleAthletes.position ++ " and " ++ String.join ", " positionWithMultipleAthletes.athletes) ]
    in
    case positionsWithMultipleAthletes of
        [ singlePosition ] ->
            dangerAlert
                [ text ("Finish token " ++ String.fromInt singlePosition.position ++ " has been scanned with more than one athlete: " ++ String.join ", " singlePosition.athletes ++ ".") ]

        _ ->
            dangerAlert
                [ text "The following finish tokens have been scanned with more than one athlete:"
                , ul [] (List.map rowGenerator positionsWithMultipleAthletes)
                ]


positionOffEndOfTimesView : PositionOffEndOfTimesProblem -> Html Msg
positionOffEndOfTimesView positionOffEndOfTimes =
    dangerAlert
        [ text
            ("The highest finish token scanned was "
                ++ String.fromInt positionOffEndOfTimes.maxPosition
                ++ " but there are only "
                ++ String.fromInt positionOffEndOfTimes.timerTimeCount
                ++ " times recorded on the timer(s)"
            )
        ]


athletesMissingPositionView : List String -> Html Msg
athletesMissingPositionView athletes =
    case athletes of
        [ singleAthlete ] ->
            dangerAlert
                [ text "Athlete barcode "
                , athleteLink singleAthlete
                , text " was scanned without a finish token."
                ]

        _ ->
            dangerAlert
                [ text "The following athlete barcodes have been scanned without finish tokens: "
                , span [] (List.map athleteLink athletes |> List.intersperse (text ", "))
                , text "."
                ]


misScannedAthleteBarcodesView : List MisScannedAthleteBarcodeProblem -> Html Msg
misScannedAthleteBarcodesView misScannedAthleteBarcodes =
    let
        generateRow : MisScannedAthleteBarcodeProblem -> Html Msg
        generateRow misScannedAthlete =
            li []
                [ text (misScannedAthlete.scannedBarcode ++ " (likely to be " ++ misScannedAthlete.similarBarcode ++ ")")
                , generateButton (RemoveUnassociatedAthlete misScannedAthlete.scannedBarcode) "Remove"
                ]
    in
    case misScannedAthleteBarcodes of
        [ singleMisScannedAthlete ] ->
            warningAlert
                [ text
                    ("Athlete barcode "
                        ++ singleMisScannedAthlete.scannedBarcode
                        ++ " is too long to be real but is similar to "
                    )
                , athleteLink singleMisScannedAthlete.similarBarcode
                , text " so is likely to have been mis-scanned. "
                , generateButton (RemoveUnassociatedAthlete singleMisScannedAthlete.scannedBarcode) "Remove"
                ]

        _ ->
            warningAlert
                [ text
                    ("The following athlete barcodes are too long to be real "
                        ++ "but are similar to other scanned barcodes so are likely to have been mis-scanned:"
                    )
                , ul [] (List.map generateRow misScannedAthleteBarcodes)
                ]


hideIfEmpty : (List a -> Html Msg) -> List a -> Maybe (Html Msg)
hideIfEmpty viewer list =
    if List.isEmpty list then
        Nothing

    else
        Just (viewer list)


misScannedItemsView : List String -> Html Msg
misScannedItemsView misScans =
    let
        generateRow : String -> Html Msg
        generateRow misScanText =
            li [] [ text misScanText ]
    in
    case misScans of
        [ singleMisScan ] ->
            dangerAlert
                [ text ("An unrecognised item  '" ++ singleMisScan ++ "' was scanned.") ]

        _ ->
            dangerAlert
                [ text "The following unrecognised items were scanned:"
                , ul [] (List.map generateRow misScans)
                ]


unrecognisedBarcodeScannerLinesView : List String -> Html Msg
unrecognisedBarcodeScannerLinesView unrecognisedBarcodeScannerLines =
    let
        generateRow : String -> Html Msg
        generateRow unrecognisedLine =
            li [] [ text unrecognisedLine ]
    in
    case unrecognisedBarcodeScannerLines of
        [ singleLine ] ->
            dangerAlert
                [ text ("The line '" ++ singleLine ++ "' in a barcode scanner file was not recognised.") ]

        _ ->
            dangerAlert
                [ text "The following lines in barcode scanner files were not recognised:"
                , ul [] (List.map generateRow unrecognisedBarcodeScannerLines)
                ]


timerProblemsView : Problems -> Html Msg
timerProblemsView problems =
    let
        problemViewSections : List (Maybe (Html Msg))
        problemViewSections =
            [ Maybe.andThen timerTimeOffsetView problems.timerTimeOffset
            , Maybe.map positionOffEndOfTimesView problems.positionOffEndOfTimes
            ]
    in
    div [ class "problems-container" ] (List.filterMap identity problemViewSections)


scannerProblemsView : Problems -> Html Msg
scannerProblemsView problems =
    let
        problemViewSections : List (Maybe (Html Msg))
        problemViewSections =
            [ Maybe.map barcodesScannedBeforeEventStartProblemView problems.barcodesScannedBeforeEventStart
            , hideIfEmpty athletesInSamePositionMultipleTimesView problems.athletesInSamePositionMultipleTimes
            , hideIfEmpty athletesWithAndWithoutPositionView problems.athletesWithAndWithoutPosition
            , hideIfEmpty athletesWithMultiplePositionsView problems.athletesWithMultiplePositions
            , hideIfEmpty positionsWithMultipleAthletesView problems.positionsWithMultipleAthletes
            , Maybe.map positionOffEndOfTimesView problems.positionOffEndOfTimes
            , hideIfEmpty athletesMissingPositionView problems.athletesMissingPosition
            , hideIfEmpty misScannedAthleteBarcodesView problems.misScannedAthleteBarcodes
            , hideIfEmpty misScannedItemsView problems.misScans
            , hideIfEmpty unrecognisedBarcodeScannerLinesView problems.unrecognisedBarcodeScannerLines
            ]
    in
    div [ class "problems-container" ] (List.filterMap identity problemViewSections)
