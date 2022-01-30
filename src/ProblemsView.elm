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
        , AthleteWithMultiplePositionsProblem
        , BarcodesScannedBeforeEventStartProblem
        , BarcodesScannedTheWrongWayAroundProblem
        , InconsistentBarcodeScannerDatesProblem
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


athletesWithAndWithoutPositionView : List AthleteAndPositionPair -> Html Msg
athletesWithAndWithoutPositionView athleteAndPositionPairs =
    let
        buttonLabel : String
        buttonLabel =
            "Remove unassociated athlete scan"

        problemFixGenerator : AthleteAndPositionPair -> ProblemFix
        problemFixGenerator pair =
            RemoveUnassociatedAthlete pair.athlete
    in
    case athleteAndPositionPairs of
        [ pair ] ->
            warningAlert
                [ text "Athlete "
                , athleteLink pair.athlete
                , text
                    (" has been scanned with finish token "
                        ++ String.fromInt pair.position
                        ++ " and also without a finish token. "
                    )
                , generateButton (problemFixGenerator pair) buttonLabel
                ]

        _ ->
            warningAlert
                [ text "The following athlete barcodes have been scanned multiple times with a finish token and without a finish token:"
                , ul [] (List.map (athleteAndPositionRow problemFixGenerator buttonLabel) athleteAndPositionPairs)
                ]


positionsWithAndWithoutAthleteView : List AthleteAndPositionPair -> Html Msg
positionsWithAndWithoutAthleteView athleteAndPositionPairs =
    let
        buttonLabel : String
        buttonLabel =
            "Remove unassociated finish token scan"

        problemFixGenerator : AthleteAndPositionPair -> ProblemFix
        problemFixGenerator pair =
            RemoveUnassociatedFinishToken pair.position
    in
    case athleteAndPositionPairs of
        [ pair ] ->
            warningAlert
                [ text
                    ("Finish token "
                        ++ String.fromInt pair.position
                        ++ " has been scanned with athlete "
                        ++ pair.athlete
                        ++ " and also without an athlete. "
                    )
                , generateButton (problemFixGenerator pair) buttonLabel
                ]

        _ ->
            warningAlert
                [ text "The following finish tokens have been scanned multiple times with an athlete and without an athlete:"
                , ul [] (List.map (athleteAndPositionRow problemFixGenerator buttonLabel) athleteAndPositionPairs)
                ]


barcodesScannedTheWrongWayAroundView : List BarcodesScannedTheWrongWayAroundProblem -> Html Msg
barcodesScannedTheWrongWayAroundView barcodesScannedTheWrongWayAround =
    let
        makeButton : BarcodesScannedTheWrongWayAroundProblem -> Html Msg
        makeButton range =
            smallButton (Msg.FixProblem (SwapBarcodes range.filename range.startLineNumber range.endLineNumber)) [] "Swap over"

        pairsIntro : BarcodesScannedTheWrongWayAroundProblem -> String
        pairsIntro range =
            if range.endLineNumber == range.startLineNumber + 1 then
                "One pair"

            else
                String.fromInt (range.endLineNumber - range.startLineNumber) ++ " pairs"

        haveOrHas : BarcodesScannedTheWrongWayAroundProblem -> String
        haveOrHas range =
            if range.endLineNumber == range.startLineNumber + 1 then
                "has"

            else
                "have"

        generateRow : BarcodesScannedTheWrongWayAroundProblem -> Html Msg
        generateRow range =
            li
                []
                [ text
                    (pairsIntro range
                        ++ ", in file "
                        ++ range.filename
                        ++ ", lines "
                        ++ String.fromInt range.startLineNumber
                        ++ " to "
                        ++ String.fromInt range.endLineNumber
                        ++ ". "
                    )
                , makeButton range
                ]
    in
    case barcodesScannedTheWrongWayAround of
        [ singleRange ] ->
            warningAlert
                [ text (pairsIntro singleRange)
                , text " of barcodes in file "
                , text singleRange.filename
                , text " "
                , text (haveOrHas singleRange)
                , text " been scanned the wrong way around between lines "
                , text (String.fromInt singleRange.startLineNumber)
                , text " and "
                , text (String.fromInt singleRange.endLineNumber)
                , text ". "
                , makeButton singleRange
                ]

        _ ->
            warningAlert
                [ text "The following ranges of barcodes may have been scanned the wrong way around:"
                , ul [] (List.map generateRow barcodesScannedTheWrongWayAround)
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


inconsistentBarcodeScannerDatesView : InconsistentBarcodeScannerDatesProblem -> Html Msg
inconsistentBarcodeScannerDatesView inconsistentBarcodeScannerDatesProblem =
    dangerAlert
        [ text
            ("Inconsistent dates were found among the barcode scanner files ("
                ++ inconsistentBarcodeScannerDatesProblem.scannerDate1
                ++ " and "
                ++ inconsistentBarcodeScannerDatesProblem.scannerDate2
                ++ ").  Please check that you have uploaded files from the same date."
            )
        ]


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
                ++ " times recorded on the timer(es)"
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


hideIfEmpty : (List a -> Html Msg) -> List a -> Maybe (Html Msg)
hideIfEmpty viewer list =
    if List.isEmpty list then
        Nothing

    else
        Just (viewer list)


positionsMissingAthleteView : List Int -> Html Msg
positionsMissingAthleteView positions =
    case positions of
        [ singlePosition ] ->
            dangerAlert
                [ text ("Finish token " ++ String.fromInt singlePosition ++ " was scanned without an athlete barcode.") ]

        _ ->
            dangerAlert
                [ text "The following finish tokens have been scanned without athlete barcodes: "
                , span [] (List.map (text << String.fromInt) positions |> List.intersperse (text ", "))
                , text "."
                ]


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


identicalTimerTimesView : Bool -> Maybe (Html Msg)
identicalTimerTimesView identicalTimerTimes =
    if identicalTimerTimes then
        dangerAlert
            [ text
                ("Both timer files have identical times.  It is very unlikely in practice for two timers to contain the same times.  "
                    ++ "Please check you haven't downloaded times from the same timer twice."
                )
            ]
            |> Just

    else
        Nothing


timerProblemsView : Problems -> Html Msg
timerProblemsView problems =
    let
        problemViewSections : List (Maybe (Html Msg))
        problemViewSections =
            [ Maybe.andThen timerTimeOffsetView problems.timerTimeOffset
            , Maybe.map positionOffEndOfTimesView problems.positionOffEndOfTimes
            , identicalTimerTimesView problems.identicalTimerTimes
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
            , hideIfEmpty positionsWithAndWithoutAthleteView problems.positionsWithAndWithoutAthlete
            , hideIfEmpty barcodesScannedTheWrongWayAroundView problems.barcodesScannedTheWrongWayAround
            , Maybe.map inconsistentBarcodeScannerDatesView problems.inconsistentBarcodeScannerDates
            , hideIfEmpty athletesWithMultiplePositionsView problems.athletesWithMultiplePositions
            , hideIfEmpty positionsWithMultipleAthletesView problems.positionsWithMultipleAthletes
            , Maybe.map positionOffEndOfTimesView problems.positionOffEndOfTimes
            , hideIfEmpty athletesMissingPositionView problems.athletesMissingPosition
            , hideIfEmpty positionsMissingAthleteView problems.positionsMissingAthlete
            , hideIfEmpty misScannedItemsView problems.misScans
            , hideIfEmpty unrecognisedBarcodeScannerLinesView problems.unrecognisedBarcodeScannerLines
            ]
    in
    div [ class "problems-container" ] (List.filterMap identity problemViewSections)
