module ParkrunStopwatch exposing (..)

import Html exposing (Html, program, div, text, table, tbody, thead, tr, td, th, h1, h3, input, label, br, button, small)
import Html.Attributes exposing (class, checked, type_, id, for)
import Html.Events exposing (onClick)
import Regex exposing (Regex, regex)
import Error exposing (Error)
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import Merger exposing (merge, MergeEntry(..))
import MergedTable exposing (MergedTableRow, generateInitialTable, toggleRowInTable, deleteStopwatchFromTable, flipTable, underlineTable)
import NumberChecker exposing (NumberCheckerEntry, AnnotatedNumberCheckerEntry, parseNumberCheckerFile, annotate)
import DataStructures exposing (WhichStopwatch(..))
import TimeHandling exposing (formatTime)
import Ports exposing (fileDrop, DroppedFile)


maxNearMatchDistance : Int
maxNearMatchDistance =
    1


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model, Update, View.


type Stopwatches
    = None
    | Single String (List Int)
    | Double String String (List MergedTableRow)


type alias Model =
    { stopwatches : Stopwatches
    , lastError : Maybe String
    , numberCheckerEntries : List AnnotatedNumberCheckerEntry
    }


initModel : Model
initModel =
    { stopwatches = None
    , lastError = Nothing
    , numberCheckerEntries = []
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type Msg
    = FileDropped DroppedFile
    | ToggleTableRow Int
    | DeleteStopwatch WhichStopwatch
    | FlipStopwatches


hasFileAlreadyBeenUploaded : String -> Stopwatches -> Bool
hasFileAlreadyBeenUploaded newFileName stopwatches =
    case stopwatches of
        None ->
            False

        Single existingFilename _ ->
            newFileName == existingFilename

        Double existingFilename1 existingFilename2 _ ->
            newFileName == existingFilename1 || newFileName == existingFilename2


underlineStopwatches : Stopwatches -> List AnnotatedNumberCheckerEntry -> Stopwatches
underlineStopwatches stopwatches numberCheckerEntries =
    if List.isEmpty numberCheckerEntries then
        stopwatches
    else
        case stopwatches of
            None ->
                stopwatches

            Single _ _ ->
                stopwatches

            Double fileName1 fileName2 mergedTable ->
                Double fileName1 fileName2 (underlineTable numberCheckerEntries mergedTable)


handleStopwatchFileDrop : String -> String -> Model -> Model
handleStopwatchFileDrop fileName fileText model =
    case readStopwatchData fileText of
        Ok (StopwatchData newStopwatch) ->
            if hasFileAlreadyBeenUploaded fileName model.stopwatches then
                { model
                    | lastError = Just ("File '" ++ fileName ++ "' has already been uploaded")
                }
            else
                let
                    newStopwatches =
                        case model.stopwatches of
                            None ->
                                Single fileName newStopwatch

                            Single existingFilename firstStopwatch ->
                                let
                                    mergedDetails : List MergeEntry
                                    mergedDetails =
                                        merge maxNearMatchDistance firstStopwatch newStopwatch

                                    mergedTable : List MergedTableRow
                                    mergedTable =
                                        generateInitialTable mergedDetails
                                in
                                    Double existingFilename fileName mergedTable

                            Double _ _ _ ->
                                model.stopwatches
                in
                    { model
                        | stopwatches = underlineStopwatches newStopwatches model.numberCheckerEntries
                        , lastError = Nothing
                    }

        Err error ->
            { model | lastError = Just error.message }


numberCheckerRegex : Regex
numberCheckerRegex =
    regex "^[0-9\x0D\n,]+$"


isPossibleNumberCheckerFile : String -> Bool
isPossibleNumberCheckerFile fileText =
    Regex.contains numberCheckerRegex fileText


handleNumberCheckerFileDrop : String -> Model -> Model
handleNumberCheckerFileDrop fileText model =
    let
        result : Result Error (List NumberCheckerEntry)
        result =
            parseNumberCheckerFile fileText
    in
        case result of
            Ok entries ->
                let
                    annotatedEntries : List AnnotatedNumberCheckerEntry
                    annotatedEntries =
                        annotate entries
                in
                    { model
                        | numberCheckerEntries = annotatedEntries
                        , stopwatches = underlineStopwatches model.stopwatches annotatedEntries
                    }

            Err error ->
                { model
                    | lastError = Just error.message
                }


handleFileDrop : String -> String -> Model -> Model
handleFileDrop fileName fileText model =
    if String.contains "STARTOFEVENT" fileText then
        handleStopwatchFileDrop fileName fileText model
    else if isPossibleNumberCheckerFile fileText then
        handleNumberCheckerFileDrop fileText model
    else
        { model
            | lastError = Just "Unrecognised file dropped"
        }


toggleTableRow : Int -> Model -> Model
toggleTableRow index model =
    case model.stopwatches of
        None ->
            model

        Single _ _ ->
            model

        Double fileName1 fileName2 currentMergedTable ->
            let
                newMergedTable : List MergedTableRow
                newMergedTable =
                    currentMergedTable
                        |> toggleRowInTable index
                        |> underlineTable model.numberCheckerEntries
            in
                { model
                    | stopwatches = Double fileName1 fileName2 newMergedTable
                }


deleteStopwatch : WhichStopwatch -> Model -> Model
deleteStopwatch which model =
    case ( model.stopwatches, which ) of
        ( None, _ ) ->
            model

        ( Single _ _, StopwatchOne ) ->
            { model | stopwatches = None }

        ( Single _ _, StopwatchTwo ) ->
            model

        ( Double fileName1 fileName2 mergedRows, _ ) ->
            let
                fileNameToKeep : String
                fileNameToKeep =
                    case which of
                        StopwatchOne ->
                            fileName2

                        StopwatchTwo ->
                            fileName1
            in
                { model
                    | stopwatches =
                        deleteStopwatchFromTable which mergedRows
                            |> Single fileNameToKeep
                }


flipStopwatches : Model -> Model
flipStopwatches model =
    case model.stopwatches of
        None ->
            model

        Single _ _ ->
            model

        Double filename1 filename2 mergedRows ->
            let
                newStopwatches : List MergedTableRow
                newStopwatches =
                    mergedRows
                        |> flipTable
                        |> underlineTable model.numberCheckerEntries
            in
                { model
                    | stopwatches = Double filename2 filename1 newStopwatches
                }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileDropped { fileName, fileText } ->
            ( handleFileDrop fileName fileText model, Cmd.none )

        ToggleTableRow index ->
            ( toggleTableRow index model, Cmd.none )

        DeleteStopwatch which ->
            ( deleteStopwatch which model, Cmd.none )

        FlipStopwatches ->
            ( flipStopwatches model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileDrop FileDropped


errorView : Maybe String -> Html a
errorView maybeString =
    case maybeString of
        Just string ->
            div [ class "alert alert-danger" ]
                [ text ("Unable to read in the stopwatch data: " ++ string) ]

        Nothing ->
            text ""


stopwatchInfoMessage : Stopwatches -> Html a
stopwatchInfoMessage stopwatches =
    let
        message : Maybe String
        message =
            case stopwatches of
                None ->
                    Just "No stopwatch files have been uploaded"

                Single _ _ ->
                    Just "Please upload another stopwatch file to enable comparison/merging"

                Double _ _ _ ->
                    Nothing
    in
        case message of
            Just messageText ->
                div [ class "alert alert-info" ]
                    [ text messageText ]

            Nothing ->
                text ""


cell : String -> Bool -> Html a
cell contents underlined =
    let
        attributes : List (Html.Attribute a)
        attributes =
            if underlined then
                [ class "underlined" ]
            else
                []
    in
        td attributes [ text contents ]


intCell : Int -> Html a
intCell contents =
    cell (toString contents) False


timeCell : String -> Int -> Bool -> Html a
timeCell className time underlined =
    let
        adjustedClassName : String
        adjustedClassName =
            if underlined then
                className ++ " underlined"
            else
                className
    in
        td [ class adjustedClassName ] [ text (formatTime time) ]


deltaCell : Int -> Html a
deltaCell delta =
    if delta == 0 then
        td [ class "zero-delta" ] [ text "0" ]
    else
        let
            stringDelta : String
            stringDelta =
                if delta > 0 then
                    "+" ++ (toString delta)
                else
                    "−" ++ (toString -delta)
        in
            td [ class "nonzero-delta" ] [ text stringDelta ]


stopwatchRow : Int -> Int -> Html a
stopwatchRow index time =
    tr []
        [ cell (toString (index + 1)) False
        , cell (formatTime time) False
        ]


emptyNumberCell : Html a
emptyNumberCell =
    td [ class "empty-cell" ] [ text "–" ]


checkboxCell : Int -> Int -> Bool -> Bool -> Html Msg
checkboxCell time index included underlined =
    let
        idText : String
        idText =
            "toggle_checkbox_" ++ (toString index)

        adjustedCellClassName : String
        adjustedCellClassName =
            if underlined then
                "mismatch underlined"
            else
                "mismatch"

        labelClassName : String
        labelClassName =
            if included then
                "stopwatch-time-label"
            else
                "stopwatch-time-label excluded"
    in
        td
            [ class adjustedCellClassName ]
            [ input
                [ type_ "checkbox"
                , checked included
                , id idText
                , onClick (ToggleTableRow index)
                ]
                []
            , label
                [ for idText
                , class labelClassName
                ]
                [ text (formatTime time) ]
            ]


mergedStopwatchRow : MergedTableRow -> Html Msg
mergedStopwatchRow row =
    let
        indexCell =
            case row.rowNumber of
                Just num ->
                    cell (toString num) row.underlines.position

                Nothing ->
                    emptyNumberCell
    in
        case row.entry of
            ExactMatch time ->
                tr
                    []
                    [ indexCell
                    , timeCell "exact-match" time row.underlines.stopwatch1
                    , timeCell "exact-match" time row.underlines.stopwatch2
                    ]

            NearMatch time1 time2 ->
                tr
                    []
                    [ indexCell
                    , timeCell "near-match" time1 row.underlines.stopwatch1
                    , timeCell "near-match" time2 row.underlines.stopwatch2
                    ]

            Watch1Only time1 ->
                tr
                    []
                    [ indexCell
                    , checkboxCell time1 row.index row.included row.underlines.stopwatch1
                    , cell "" False
                    ]

            Watch2Only time2 ->
                tr
                    []
                    [ indexCell
                    , cell "" False
                    , checkboxCell time2 row.index row.included row.underlines.stopwatch2
                    ]


tableHeader : String -> Html a
tableHeader headerText =
    th [] [ text headerText ]


type alias TableHeaderButton =
    { change : Msg
    , buttonText : String
    }


type alias TableHeaderWithButton =
    { headerText : String
    , buttonData : Maybe TableHeaderButton
    }


deleteButton : WhichStopwatch -> Maybe TableHeaderButton
deleteButton which =
    Just
        { change = DeleteStopwatch which
        , buttonText = "Delete "
        }


tableHeaderWithButtons : TableHeaderWithButton -> Html Msg
tableHeaderWithButtons { headerText, buttonData } =
    let
        textElement : Html Msg
        textElement =
            text headerText
    in
        case buttonData of
            Just { change, buttonText } ->
                th
                    [ class "stopwatch-header" ]
                    [ button
                        [ type_ "button"
                        , onClick change
                        , class "btn btn-primary btn-xs"
                        ]
                        [ text buttonText ]
                    , br [] []
                    , textElement
                    ]

            Nothing ->
                th [] [ textElement ]


tableHeaders : List String -> Html a
tableHeaders headerTexts =
    thead
        []
        [ tr
            []
            (List.map tableHeader headerTexts)
        ]


tableHeadersWithButtons : List TableHeaderWithButton -> Html Msg
tableHeadersWithButtons headerTexts =
    thead
        []
        [ tr
            []
            (List.map tableHeaderWithButtons headerTexts)
        ]


stopwatchTable : Stopwatches -> Html Msg
stopwatchTable stopwatches =
    case stopwatches of
        None ->
            text ""

        Single _ stopwatchTimes ->
            table
                [ class "table table-condensed table-bordered stopwatch-times" ]
                [ tableHeadersWithButtons
                    [ TableHeaderWithButton "Position" Nothing
                    , TableHeaderWithButton "Stopwatch 1" (deleteButton StopwatchOne)
                    ]
                , tbody [] (List.indexedMap stopwatchRow stopwatchTimes)
                ]

        Double _ _ mergedTable ->
            table
                [ class "table table-condensed table-bordered stopwatch-times" ]
                [ tableHeadersWithButtons
                    [ TableHeaderWithButton "Position" Nothing
                    , TableHeaderWithButton "Stopwatch 1" (deleteButton StopwatchOne)
                    , TableHeaderWithButton "Stopwatch 2" (deleteButton StopwatchTwo)
                    ]
                , tbody
                    []
                    (List.map mergedStopwatchRow mergedTable)
                ]


stopwatchButtonsContent : Stopwatches -> List (Html Msg)
stopwatchButtonsContent stopwatches =
    case stopwatches of
        None ->
            []

        Single _ _ ->
            []

        Double _ _ _ ->
            [ button
                [ class "btn btn-primary btn-large"
                , onClick FlipStopwatches
                ]
                [ text "Flip"
                , br [] []
                , small [] [ text "stopwatches" ]
                ]
            ]


stopwatchesView : Stopwatches -> Html Msg
stopwatchesView stopwatches =
    div [ class "stopwatch-view" ]
        [ h3 [] [ text "Stopwatches" ]
        , stopwatchInfoMessage stopwatches
        , stopwatchTable stopwatches
        , div [ class "stopwatch-buttons" ] (stopwatchButtonsContent stopwatches)
        ]


numberCheckerRow : AnnotatedNumberCheckerEntry -> Html a
numberCheckerRow entry =
    tr
        []
        [ intCell entry.stopwatch1
        , deltaCell entry.stopwatch1Delta
        , intCell entry.stopwatch2
        , deltaCell entry.stopwatch2Delta
        , intCell entry.finishTokens
        , deltaCell entry.finishTokensDelta
        ]


noNumberCheckerData : Html a
noNumberCheckerData =
    div [ class "alert alert-info" ]
        [ text "No number-checker data has been loaded" ]


numberCheckerTable : List AnnotatedNumberCheckerEntry -> Html a
numberCheckerTable entries =
    table
        [ class "table table-bordered number-checker-table" ]
        [ tableHeaders [ "Stopwatch 1", "+/−", "Stopwatch 2", "+/−", "Finish tokens", "+/−" ]
        , tbody
            []
            (List.map numberCheckerRow entries)
        ]


numberCheckerView : List AnnotatedNumberCheckerEntry -> Html a
numberCheckerView entries =
    div
        [ class "number-checker-view" ]
        [ h3 [] [ text "Number checker" ]
        , if List.isEmpty entries then
            noNumberCheckerData
          else
            numberCheckerTable entries
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "Parkrun stopwatch comparison/merging" ]
        , errorView model.lastError
        , stopwatchesView model.stopwatches
        , numberCheckerView model.numberCheckerEntries
        ]
