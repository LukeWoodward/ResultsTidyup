module ParkrunStopwatch exposing (..)

import Html exposing (Html, program, div, text, table, tbody, thead, tr, td, th, h1, input, label)
import Html.Attributes exposing (class, checked, type_, id, for)
import Html.Events exposing (onClick)
import Regex exposing (Regex, regex)
import Error exposing (Error)
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import Merger exposing (merge, MergeEntry(..))
import MergedTable exposing (MergedTableRow, generateInitialTable, toggleRowInTable)
import NumberChecker exposing (NumberCheckerEntry, AnnotatedNumberCheckerEntry, parseNumberCheckerFile, annotate)
import TimeHandling exposing (formatTime)
import Ports exposing (fileDrop)


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
    | Single (List Int)
    | Double (List MergedTableRow)


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
    = FileDropped String
    | ToggleTableRow Int


handleStopwatchFileDrop : String -> Model -> Model
handleStopwatchFileDrop fileText model =
    case readStopwatchData fileText of
        Ok (StopwatchData newStopwatch) ->
            let
                newStopwatches =
                    case model.stopwatches of
                        None ->
                            Single newStopwatch

                        Single firstStopwatch ->
                            let
                                mergedDetails : List MergeEntry
                                mergedDetails =
                                    merge maxNearMatchDistance firstStopwatch newStopwatch

                                mergedTable : List MergedTableRow
                                mergedTable =
                                    generateInitialTable mergedDetails
                            in
                                Double mergedTable

                        Double _ ->
                            model.stopwatches
            in
                { model
                    | stopwatches = newStopwatches
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
                { model
                    | numberCheckerEntries = annotate entries
                }

            Err error ->
                { model
                    | lastError = Just error.message
                }


handleFileDrop : String -> Model -> Model
handleFileDrop fileText model =
    if String.contains "STARTOFEVENT" fileText then
        handleStopwatchFileDrop fileText model
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

        Single _ ->
            model

        Double currentMergedTable ->
            { model
                | stopwatches = Double (toggleRowInTable index currentMergedTable)
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileDropped fileText ->
            ( handleFileDrop fileText model, Cmd.none )

        ToggleTableRow index ->
            ( toggleTableRow index model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileDrop FileDropped


errorView : Maybe String -> List (Html a)
errorView maybeString =
    case maybeString of
        Just string ->
            [ div [ class "alert alert-danger" ]
                [ text ("Unable to read in the stopwatch data: " ++ string) ]
            ]

        Nothing ->
            []


noStopwatchesUploadedMessage : Stopwatches -> List (Html a)
noStopwatchesUploadedMessage stopwatches =
    case stopwatches of
        None ->
            [ div [ class "alert alert-info" ]
                [ text "No stopwatch files have been uploaded" ]
            ]

        Single _ ->
            [ div [ class "alert alert-info" ]
                [ text "Please upload another stopwatch file to enable comparison/merging" ]
            ]

        Double _ ->
            []


cell : String -> Html a
cell contents =
    td [] [ text contents ]


intCell : Int -> Html a
intCell contents =
    cell (toString contents)


timeCell : String -> Int -> Html a
timeCell className time =
    td [ class className ] [ text (formatTime time) ]


deltaCell : Int -> Html a
deltaCell delta =
    if delta == 0 then
        td [] []
    else
        let
            stringDelta : String
            stringDelta =
                if delta > 0 then
                    "+" ++ (toString delta)
                else
                    toString delta
        in
            td [ class "nonzero-delta" ] [ text stringDelta ]


stopwatchRow : Int -> Int -> Html a
stopwatchRow index time =
    tr []
        [ cell (toString (index + 1))
        , cell (formatTime time)
        ]


emptyNumberCell : Html a
emptyNumberCell =
    td [ class "empty-cell" ] [ text "–" ]


checkboxCell : Int -> Int -> Bool -> Html Msg
checkboxCell time index included =
    let
        idText : String
        idText =
            "toggle_checkbox_" ++ (toString index)
    in
        td
            [ class "mismatch" ]
            [ input
                [ type_ "checkbox"
                , checked included
                , id idText
                , onClick (ToggleTableRow index)
                ]
                []
            , label
                [ for idText
                , class "stopwatch-time-label"
                ]
                [ text (formatTime time) ]
            ]


mergedStopwatchRow : MergedTableRow -> Html Msg
mergedStopwatchRow row =
    let
        indexCell =
            case row.rowNumber of
                Just num ->
                    cell (toString num)

                Nothing ->
                    emptyNumberCell
    in
        case row.entry of
            ExactMatch time ->
                tr
                    []
                    [ indexCell
                    , timeCell "exact-match" time
                    , timeCell "exact-match" time
                    ]

            NearMatch time1 time2 ->
                tr
                    []
                    [ indexCell
                    , timeCell "near-match" time1
                    , timeCell "near-match" time2
                    ]

            Watch1Only time1 ->
                tr
                    []
                    [ indexCell
                    , checkboxCell time1 row.index row.included
                    , cell ""
                    ]

            Watch2Only time2 ->
                tr
                    []
                    [ indexCell
                    , cell ""
                    , checkboxCell time2 row.index row.included
                    ]


tableHeader : String -> Html a
tableHeader headerText =
    th [] [ text headerText ]


tableHeaders : List String -> Html a
tableHeaders headerTexts =
    thead
        []
        [ tr
            []
            (List.map tableHeader headerTexts)
        ]


stopwatchView : Stopwatches -> List (Html Msg)
stopwatchView stopwatches =
    case stopwatches of
        None ->
            []

        Single stopwatchTimes ->
            [ table
                [ class "table stopwatch-times" ]
                [ tableHeaders [ "Position", "Stopwatch 1" ]
                , tbody [] (List.indexedMap stopwatchRow stopwatchTimes)
                ]
            ]

        Double mergedTable ->
            [ table
                [ class "table stopwatch-times" ]
                [ tableHeaders [ "Position", "Stopwatch 1", "Stopwatch 2" ]
                , tbody
                    []
                    (List.map mergedStopwatchRow mergedTable)
                ]
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


numberCheckerView : List AnnotatedNumberCheckerEntry -> List (Html a)
numberCheckerView entries =
    [ table
        [ class "table number-checker-table" ]
        [ tableHeaders [ "Stopwatch 1", "+/-", "Stopwatch 2", "+/-", "Finish tokens", "+/-" ]
        , tbody
            []
            (List.map numberCheckerRow entries)
        ]
    ]


view : Model -> Html Msg
view model =
    div
        []
        (h1 [] [ text "Parkrun stopwatch comparison/merging" ]
            :: (errorView model.lastError)
            ++ (noStopwatchesUploadedMessage model.stopwatches)
            ++ (stopwatchView model.stopwatches)
            ++ (numberCheckerView model.numberCheckerEntries)
        )
