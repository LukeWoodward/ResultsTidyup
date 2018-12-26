module ParkrunStopwatch exposing (Model, main, update, view)

import BarcodeScanner exposing (BarcodeScannerData, mergeScannerData, readBarcodeScannerData)
import Browser
import DataStructures exposing (WhichStopwatch(..))
import DateHandling exposing (generateDownloadFilenameDatePart)
import Dict exposing (Dict)
import Error exposing (Error)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import MergedTable exposing (MergedTableRow, Stopwatches(..), deleteStopwatchFromTable, flipTable, generateInitialTable, outputMergedTable, toggleRowInTable, underlineTable)
import Merger exposing (MergeEntry(..), merge)
import Msg exposing (Msg(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, annotate, parseNumberCheckerFile)
import NumberCheckerView exposing (numberCheckerView)
import Ports exposing (InteropFile, downloadMergedTimesToFile, fileDrop, getInitialHeight, heightUpdated)
import Problems exposing (ProblemsContainer, identifyProblems)
import ProblemsView exposing (problemsView)
import Regex exposing (Regex)
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import StopwatchesView exposing (stopwatchesView)
import Task exposing (Task)
import Time exposing (Posix, Zone)
import TimeHandling exposing (formatTime)
import ViewCommon exposing (intCell, plainCell, tableHeaders)


maxNearMatchDistance : Int
maxNearMatchDistance =
    1


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model, Update, View.


type alias Model =
    { stopwatches : Stopwatches
    , lastError : Maybe String
    , numberCheckerEntries : List AnnotatedNumberCheckerEntry
    , lastHeight : Maybe Int
    , highlightedNumberCheckerId : Maybe Int
    , barcodeScannerFiles : List String
    , barcodeScannerData : BarcodeScannerData
    , problems : ProblemsContainer
    }


initModel : Model
initModel =
    { stopwatches = None
    , lastError = Nothing
    , numberCheckerEntries = []
    , lastHeight = Nothing
    , highlightedNumberCheckerId = Nothing
    , barcodeScannerFiles = []
    , barcodeScannerData = BarcodeScanner.empty
    , problems = Problems.empty
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, getInitialHeight () )


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


identifyProblemsIn : Model -> Model
identifyProblemsIn model =
    { model
        | problems = identifyProblems model.stopwatches model.barcodeScannerData
    }


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

                    underlinedStopwatches =
                        underlineStopwatches newStopwatches model.numberCheckerEntries
                in
                identifyProblemsIn
                    { model
                        | stopwatches = underlinedStopwatches
                        , lastError = Nothing
                    }

        Err error ->
            { model | lastError = Just error.message }


numberCheckerRegex : Regex
numberCheckerRegex =
    Regex.fromString "^[0-9\u{000D}\n,]+$"
        |> Maybe.withDefault Regex.never


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
            identifyProblemsIn
                { model
                    | numberCheckerEntries = annotatedEntries
                    , stopwatches = underlineStopwatches model.stopwatches annotatedEntries
                }

        Err error ->
            { model
                | lastError = Just error.message
            }


barcodeScannerRegex : Regex
barcodeScannerRegex =
    Regex.fromString "^A[0-9]+,P[0-9]+"
        |> Maybe.withDefault Regex.never


isPossibleBarcodeScannerFile : String -> Bool
isPossibleBarcodeScannerFile fileText =
    Regex.contains barcodeScannerRegex fileText


handleBarcodeScannerFileDrop : String -> String -> Model -> Model
handleBarcodeScannerFileDrop fileName fileText model =
    if List.member fileName model.barcodeScannerFiles then
        { model
            | lastError = Just "That barcode scanner file has already been loaded"
        }

    else
        let
            result : Result Error BarcodeScannerData
            result =
                readBarcodeScannerData fileText
        in
        case result of
            Ok scannerData ->
                identifyProblemsIn
                    { model
                        | barcodeScannerFiles = fileName :: model.barcodeScannerFiles
                        , barcodeScannerData = mergeScannerData model.barcodeScannerData scannerData
                        , lastError = Nothing
                    }

            Err error ->
                { model | lastError = Just error.message }


handleFileDrop : String -> String -> Model -> Model
handleFileDrop fileName fileText model =
    if String.contains "STARTOFEVENT" fileText then
        handleStopwatchFileDrop fileName fileText model

    else if isPossibleNumberCheckerFile fileText then
        handleNumberCheckerFileDrop fileText model

    else if isPossibleBarcodeScannerFile fileText then
        handleBarcodeScannerFileDrop fileName fileText model

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
            identifyProblemsIn { model | stopwatches = None }

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
            identifyProblemsIn
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


clearBarcodeScannerData : Model -> Model
clearBarcodeScannerData model =
    identifyProblemsIn
        { model
            | barcodeScannerData = BarcodeScanner.empty
            , barcodeScannerFiles = []
        }


downloadMergedStopwatchData : Zone -> Posix -> Model -> ( Model, Cmd Msg )
downloadMergedStopwatchData zone time model =
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
                    "parkrun_timer_" ++ generateDownloadFilenameDatePart zone time ++ ".txt"

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

        ClearBarcodeScannerData ->
            ( clearBarcodeScannerData model, Cmd.none )

        GetCurrentDateForDownloadFile ->
            ( model
            , Task.perform identity (Task.map2 DownloadMergedStopwatchData Time.here Time.now)
            )

        DownloadMergedStopwatchData zone time ->
            downloadMergedStopwatchData zone time model

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
                [ text ("Unable to read in the dropped file: " ++ string) ]

        Nothing ->
            text ""


getHeightAttribute : Maybe Int -> List (Html.Attribute a)
getHeightAttribute lastHeight =
    case lastHeight of
        Just someHeight ->
            [ style "height" (String.fromInt someHeight ++ "px") ]

        Nothing ->
            []


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ h1 [ id "header" ] [ text "Parkrun stopwatch comparison/merging" ]
        , errorView model.lastError
        , div [ class "row" ]
            [ div (class "col-xs-6" :: getHeightAttribute model.lastHeight)
                [ stopwatchesView model.stopwatches model.barcodeScannerData model.lastHeight model.highlightedNumberCheckerId ]
            , div (class "col-xs-6" :: getHeightAttribute model.lastHeight)
                [ problemsView model.problems
                , numberCheckerView model.numberCheckerEntries model.lastHeight
                ]
            ]
        ]
