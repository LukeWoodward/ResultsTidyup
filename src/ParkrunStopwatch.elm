module ParkrunStopwatch exposing (..)

import Html exposing (Html, program, div, text, table, tbody, thead, tr, td, th, h1, h3, input, label, br, button, small)
import Html.Attributes exposing (class, checked, type_, id, for, style)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Regex exposing (Regex, regex)
import Error exposing (Error)
import Date exposing (Date)
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import Merger exposing (merge, MergeEntry(..))
import MergedTable exposing (MergedTableRow, generateInitialTable, toggleRowInTable, deleteStopwatchFromTable, flipTable, underlineTable, outputMergedTable)
import NumberChecker exposing (NumberCheckerEntry, AnnotatedNumberCheckerEntry, parseNumberCheckerFile, annotate)
import DataStructures exposing (WhichStopwatch(..))
import TimeHandling exposing (formatTime)
import DateHandling exposing (generateDownloadFilenameDatePart)
import Task exposing (Task)
import Ports exposing (getInitialHeight, fileDrop, InteropFile, downloadMergedTimesToFile, heightUpdated)


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
    , lastHeight : Maybe Int
    , highlightedNumberCheckerId : Maybe Int
    }


initModel : Model
initModel =
    { stopwatches = None
    , lastError = Nothing
    , numberCheckerEntries = []
    , lastHeight = Nothing
    , highlightedNumberCheckerId = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, getInitialHeight () )


type Msg
    = FileDropped InteropFile
    | ToggleTableRow Int
    | DeleteStopwatch WhichStopwatch
    | FlipStopwatches
    | GetCurrentDateForDownloadFile
    | DownloadMergedStopwatchData Date
    | ContainerHeightChanged Int
    | MouseEnterNumberCheckerRow Int
    | MouseLeaveNumberCheckerRow Int
    | DeleteNumberCheckerRow Int


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


downloadMergedStopwatchData : Date -> Model -> ( Model, Cmd Msg )
downloadMergedStopwatchData date model =
    case model.stopwatches of
        None ->
            ( model, Cmd.none )

        Single _ _ ->
            ( model, Cmd.none )

        Double _ _ mergedTableRows ->
            let
                fileContents : String
                fileContents =
                    outputMergedTable mergedTableRows

                fileName : String
                fileName =
                    "parkrun_timer_" ++ (generateDownloadFilenameDatePart date) ++ ".txt"

                file : InteropFile
                file =
                    InteropFile fileName fileContents
            in
                ( model, downloadMergedTimesToFile file )


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

        GetCurrentDateForDownloadFile ->
            ( model, Task.perform DownloadMergedStopwatchData Date.now )

        DownloadMergedStopwatchData date ->
            downloadMergedStopwatchData date model

        ContainerHeightChanged newHeight ->
            ( { model | lastHeight = Just newHeight }, Cmd.none )

        MouseEnterNumberCheckerRow highlightRow ->
            ( { model | highlightedNumberCheckerId = Just highlightRow }, Cmd.none )

        MouseLeaveNumberCheckerRow unhighlightRow ->
            let
                newModel : Model
                newModel =
                    if model.highlightedNumberCheckerId == Just unhighlightRow then
                        { model | highlightedNumberCheckerId = Nothing }
                    else
                        -- Ignore an un-highlight command when the row to
                        -- unhighlight isn't the highlighted one.
                        model
            in
                ( newModel, Cmd.none )

        DeleteNumberCheckerRow entryNumber ->
            let
                newNumberCheckerEntries : List AnnotatedNumberCheckerEntry
                newNumberCheckerEntries =
                    List.filter (\e -> e.entryNumber /= entryNumber) model.numberCheckerEntries
            in
                case model.stopwatches of
                    Double filename1 filename2 oldMergedTable ->
                        let
                            newMergedTable : List MergedTableRow
                            newMergedTable =
                                underlineTable newNumberCheckerEntries oldMergedTable
                        in
                            ( { model
                                | numberCheckerEntries = newNumberCheckerEntries
                                , stopwatches = Double filename1 filename2 newMergedTable
                              }
                            , Cmd.none
                            )

                    Single _ _ ->
                        ( { model | numberCheckerEntries = newNumberCheckerEntries }, Cmd.none )

                    None ->
                        ( { model | numberCheckerEntries = newNumberCheckerEntries }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ fileDrop FileDropped
        , heightUpdated ContainerHeightChanged
        ]


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
        highlightClassPrefix ++ "underlined number-checker-row-" ++ (toString numberCheckerId)


numberCheckerUnderlineAttributes : Maybe String -> Maybe Int -> Maybe Int -> List (Html.Attribute a)
numberCheckerUnderlineAttributes className numberCheckerId highlightedNumberCheckerId =
    case ( className, numberCheckerId ) of
        ( Just someClass, Just someNumberCheckerId ) ->
            [ class (someClass ++ " " ++ (numberCheckerUnderlineClass someNumberCheckerId highlightedNumberCheckerId)) ]

        ( Nothing, Just someNumberCheckerId ) ->
            [ class (numberCheckerUnderlineClass someNumberCheckerId highlightedNumberCheckerId) ]

        ( Just someClass, Nothing ) ->
            [ class someClass ]

        ( Nothing, Nothing ) ->
            []


cell : String -> Maybe Int -> Maybe Int -> Html a
cell contents numberCheckerId highlightedNumberCheckerId =
    td (numberCheckerUnderlineAttributes Nothing numberCheckerId highlightedNumberCheckerId) [ text contents ]


intCell : Int -> Html a
intCell contents =
    cell (toString contents) Nothing Nothing


timeCell : String -> Int -> Maybe Int -> Maybe Int -> Html a
timeCell className time numberCheckerId highlightedNumberCheckerId =
    td (numberCheckerUnderlineAttributes (Just className) numberCheckerId highlightedNumberCheckerId) [ text (formatTime time) ]


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
        [ cell (toString (index + 1)) Nothing Nothing
        , cell (formatTime time) Nothing Nothing
        ]


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


emptyNumberCell : Html a
emptyNumberCell =
    td [ class "empty-cell" ] [ text "–" ]


checkboxCell : Int -> Int -> Bool -> Maybe Int -> Maybe Int -> Html Msg
checkboxCell time index included numberCheckerId highlightedNumberCheckerId =
    let
        idText : String
        idText =
            "toggle_checkbox_" ++ (toString index)

        labelClassName : String
        labelClassName =
            if included then
                "stopwatch-time-label"
            else
                "stopwatch-time-label excluded"
    in
        td
            (numberCheckerUnderlineAttributes (Just "mismatch") numberCheckerId highlightedNumberCheckerId)
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


mergedStopwatchRow : Maybe Int -> MergedTableRow -> Html Msg
mergedStopwatchRow highlightedNumberCheckerId row =
    let
        indexCell =
            case row.rowNumber of
                Just num ->
                    cell (toString num) row.underlines.position highlightedNumberCheckerId

                Nothing ->
                    emptyNumberCell
    in
        case row.entry of
            ExactMatch time ->
                tr
                    []
                    [ indexCell
                    , timeCell "exact-match" time row.underlines.stopwatch1 highlightedNumberCheckerId
                    , timeCell "exact-match" time row.underlines.stopwatch2 highlightedNumberCheckerId
                    ]

            NearMatch time1 time2 ->
                tr
                    []
                    [ indexCell
                    , timeCell "near-match" time1 row.underlines.stopwatch1 highlightedNumberCheckerId
                    , timeCell "near-match" time2 row.underlines.stopwatch2 highlightedNumberCheckerId
                    ]

            OneWatchOnly StopwatchOne time1 ->
                tr
                    []
                    [ indexCell
                    , checkboxCell time1 row.index row.included row.underlines.stopwatch1 highlightedNumberCheckerId
                    , cell "" Nothing Nothing
                    ]

            OneWatchOnly StopwatchTwo time2 ->
                tr
                    []
                    [ indexCell
                    , cell "" Nothing Nothing
                    , checkboxCell time2 row.index row.included row.underlines.stopwatch2 highlightedNumberCheckerId
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


stopwatchTable : Stopwatches -> Maybe Int -> Html Msg
stopwatchTable stopwatches highlightedNumberCheckerId =
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
                    (List.map (mergedStopwatchRow highlightedNumberCheckerId) mergedTable)
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
            , br [] []
            , br [] []
            , button
                [ class "btn btn-primary btn-large"
                , onClick GetCurrentDateForDownloadFile
                ]
                [ text "Download"
                , br [] []
                , small [] [ text "merged times" ]
                ]
            ]


getHeightAttribute : Maybe Int -> List (Html.Attribute a)
getHeightAttribute lastHeight =
    case lastHeight of
        Just someHeight ->
            [ style [ ( "height", (toString someHeight) ++ "px" ) ] ]

        Nothing ->
            []


stopwatchesView : Stopwatches -> Maybe Int -> Maybe Int -> Html Msg
stopwatchesView stopwatches lastHeight highlightedNumberCheckerId =
    div (class "stopwatch-view" :: getHeightAttribute lastHeight)
        [ h3 [] [ text "Stopwatches" ]
        , stopwatchInfoMessage stopwatches
        , stopwatchTable stopwatches highlightedNumberCheckerId
        , div [ class "stopwatch-buttons" ] (stopwatchButtonsContent stopwatches)
        ]


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
        (class "number-checker-view" :: getHeightAttribute lastHeight)
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
        [ h1 [ id "header" ] [ text "Parkrun stopwatch comparison/merging" ]
        , errorView model.lastError
        , stopwatchesView model.stopwatches model.lastHeight model.highlightedNumberCheckerId
        , numberCheckerView model.numberCheckerEntries model.lastHeight
        ]
