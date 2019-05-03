module UpdateLogic exposing (createMergedStopwatchDataFile, createSingleStopwatchDataFile, update)

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , deleteBarcodeScannerLine
        , generateDownloadText
        , regenerate
        , updateBarcodeScannerLine
        )
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails, updateEditDetails)
import Bootstrap.Tab as Tab
import Browser.Dom
import DateHandling exposing (generateDownloadFilenameDatePart)
import Dict
import EventDateAndTime exposing (EventDateAndTime)
import EventDateAndTimeEditing exposing (handleEventDateChange, handleEventTimeChange)
import File.Download as Download
import FileDropHandling exposing (handleFilesDropped)
import FileHandling exposing (InteropFile)
import Model exposing (DialogDetails(..), Model, NumberCheckerManualEntryRow, ProblemEntry, emptyNumberCheckerManualEntryRow, initModel)
import Msg exposing (Msg(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry)
import NumberCheckerEditing
    exposing
        ( addNumberCheckerRow
        , deleteNumberCheckerEntry
        , editNumberCheckerRow
        , handleNumberCheckerFieldChange
        , modifyNumberCheckerRows
        )
import Ports exposing (recordEventStartTime)
import ProblemFixing exposing (fixProblem)
import Problems exposing (Problem, identifyProblems)
import Stopwatch
    exposing
        ( DoubleStopwatchData
        , MergedTableRow
        , Stopwatches(..)
        , WhichStopwatch(..)
        , flipTable
        , outputMergedTable
        , outputSingleStopwatchData
        , toggleRowInTable
        , underlineTable
        )
import Task exposing (Task)
import Time exposing (Posix, Zone)


focus : String -> Cmd Msg
focus elementId =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus elementId)


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

            Double doubleStopwatchData ->
                Double
                    { doubleStopwatchData
                        | mergedTableRows = underlineTable numberCheckerEntries doubleStopwatchData.mergedTableRows
                    }


{-| Creates a list of ProblemEntry items from the given list of problems,
transferring ignored problems from the given list of current problem entries.
-}
transferIgnoredProblems : List ProblemEntry -> List Problem -> List ProblemEntry
transferIgnoredProblems currentProblemEntries newProblems =
    let
        currentlyIgnoredProblems : List Problem
        currentlyIgnoredProblems =
            List.filter .ignored currentProblemEntries
                |> List.map .problem
    in
    List.indexedMap
        (\index problem -> ProblemEntry problem index (List.member problem currentlyIgnoredProblems))
        newProblems


identifyProblemsIn : Model -> Model
identifyProblemsIn model =
    let
        newProblems : List Problem
        newProblems =
            identifyProblems model.stopwatches model.barcodeScannerData model.eventDateAndTime
    in
    { model
        | problems = transferIgnoredProblems model.problems newProblems
    }


toggleTableRow : Int -> Model -> Model
toggleTableRow index model =
    case model.stopwatches of
        None ->
            model

        Single _ _ ->
            model

        Double doubleStopwatchData ->
            let
                newMergedTable : List MergedTableRow
                newMergedTable =
                    doubleStopwatchData.mergedTableRows
                        |> toggleRowInTable index
                        |> underlineTable model.numberCheckerEntries
            in
            { model
                | stopwatches = Double { doubleStopwatchData | mergedTableRows = newMergedTable }
            }


createSingleStopwatchDataFile : WhichStopwatch -> Zone -> Posix -> Model -> Maybe InteropFile
createSingleStopwatchDataFile whichStopwatch zone time model =
    let
        downloadText : Maybe String
        downloadText =
            case ( model.stopwatches, whichStopwatch ) of
                ( None, _ ) ->
                    Nothing

                ( Single _ times, StopwatchOne ) ->
                    Just (outputSingleStopwatchData times)

                ( Single _ _, StopwatchTwo ) ->
                    Nothing

                ( Double doubleStopwatchData, StopwatchOne ) ->
                    Just (outputSingleStopwatchData doubleStopwatchData.times1)

                ( Double doubleStopwatchData, StopwatchTwo ) ->
                    Just (outputSingleStopwatchData doubleStopwatchData.times2)
    in
    Maybe.map (createStopwatchFileForDownload zone time) downloadText


deleteStopwatch : WhichStopwatch -> Model -> Model
deleteStopwatch which model =
    case ( model.stopwatches, which ) of
        ( None, _ ) ->
            model

        ( Single _ _, StopwatchOne ) ->
            identifyProblemsIn { model | stopwatches = None }

        ( Single _ _, StopwatchTwo ) ->
            model

        ( Double doubleStopwatchData, StopwatchOne ) ->
            let
                newStopwatches : Stopwatches
                newStopwatches =
                    Single doubleStopwatchData.filename2 doubleStopwatchData.times2
            in
            identifyProblemsIn
                { model | stopwatches = newStopwatches }

        ( Double doubleStopwatchData, StopwatchTwo ) ->
            let
                newStopwatches : Stopwatches
                newStopwatches =
                    Single doubleStopwatchData.filename1 doubleStopwatchData.times1
            in
            identifyProblemsIn
                { model | stopwatches = newStopwatches }


flipStopwatches : Model -> Model
flipStopwatches model =
    case model.stopwatches of
        None ->
            model

        Single _ _ ->
            model

        Double oldDoubleStopwatchData ->
            let
                newMergedTableRows : List MergedTableRow
                newMergedTableRows =
                    oldDoubleStopwatchData.mergedTableRows
                        |> flipTable
                        |> underlineTable model.numberCheckerEntries

                newDoubleStopwatchData : DoubleStopwatchData
                newDoubleStopwatchData =
                    { times1 = oldDoubleStopwatchData.times2
                    , times2 = oldDoubleStopwatchData.times1
                    , filename1 = oldDoubleStopwatchData.filename2
                    , filename2 = oldDoubleStopwatchData.filename1
                    , mergedTableRows = newMergedTableRows
                    }
            in
            { model | stopwatches = Double newDoubleStopwatchData }


clearAllData : Model -> Model
clearAllData model =
    { model
        | stopwatches = None
        , lastErrors = []
        , numberCheckerEntries = []
        , lastHeight = Nothing
        , highlightedNumberCheckerId = Nothing
        , barcodeScannerData = BarcodeScanner.empty
        , problems = []
        , eventDateAndTime = EventDateAndTime "" Nothing model.eventDateAndTime.enteredTime model.eventDateAndTime.validatedTime
        , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
        , barcodeScannerTab = Tab.initialState
        , dialogDetails = NoDialog
    }


createStopwatchFileForDownload : Zone -> Posix -> String -> InteropFile
createStopwatchFileForDownload zone time fileContents =
    let
        fileName : String
        fileName =
            "results_tidyup_timer_" ++ generateDownloadFilenameDatePart zone time ++ ".txt"
    in
    InteropFile fileName fileContents


downloadFileMaybe : Maybe InteropFile -> Cmd Msg
downloadFileMaybe interopFileMaybe =
    case interopFileMaybe of
        Just interopFile ->
            Download.string interopFile.fileName "text/csv" interopFile.fileText

        Nothing ->
            Cmd.none


createMergedStopwatchDataFile : Zone -> Posix -> Model -> Maybe InteropFile
createMergedStopwatchDataFile zone time model =
    case model.stopwatches of
        None ->
            Nothing

        Single _ _ ->
            Nothing

        Double doubleStopwatchData ->
            createStopwatchFileForDownload zone time (outputMergedTable doubleStopwatchData.mergedTableRows)
                |> Just


reunderlineStopwatchTable : Model -> Model
reunderlineStopwatchTable model =
    case model.stopwatches of
        Double doubleStopwatchData ->
            let
                newMergedTable : List MergedTableRow
                newMergedTable =
                    underlineTable model.numberCheckerEntries doubleStopwatchData.mergedTableRows
            in
            { model
                | stopwatches = Double { doubleStopwatchData | mergedTableRows = newMergedTable }
            }

        Single _ _ ->
            model

        None ->
            model


createSingleBarcodeScannerData : String -> List BarcodeScannerFile -> Zone -> Posix -> Maybe InteropFile
createSingleBarcodeScannerData fileName files zone time =
    let
        fileToDownload : Maybe BarcodeScannerFile
        fileToDownload =
            List.filter (\file -> file.name == fileName) files
                |> List.head
    in
    case fileToDownload of
        Just someFile ->
            let
                downloadFileContents : String
                downloadFileContents =
                    generateDownloadText someFile

                downloadFileName : String
                downloadFileName =
                    "results_tidyup_barcode_" ++ generateDownloadFilenameDatePart zone time ++ ".txt"
            in
            Just (InteropFile downloadFileName downloadFileContents)

        Nothing ->
            -- No file with that name was found.
            Nothing


deleteBarcodeScannerFileWithName : String -> Model -> Model
deleteBarcodeScannerFileWithName fileName model =
    let
        barcodeScannerData : BarcodeScannerData
        barcodeScannerData =
            model.barcodeScannerData
    in
    identifyProblemsIn
        { model
            | barcodeScannerData = regenerate { barcodeScannerData | files = List.filter (\file -> file.name /= fileName) model.barcodeScannerData.files }
        }


ignoreProblem : Int -> Model -> Model
ignoreProblem problemIndex model =
    let
        ignoreProblemIfIndexMatches : ProblemEntry -> ProblemEntry
        ignoreProblemIfIndexMatches problem =
            if problem.index == problemIndex then
                { problem | ignored = True }

            else
                problem

        newProblems : List ProblemEntry
        newProblems =
            List.map ignoreProblemIfIndexMatches model.problems
    in
    { model | problems = newProblems }


select : Bool -> a -> a -> a
select condition trueValue falseValue =
    if condition then
        trueValue

    else
        falseValue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FilesDropped files ->
            ( handleFilesDropped files model
                |> identifyProblemsIn
                |> reunderlineStopwatchTable
            , Cmd.none
            )

        ToggleTableRow index ->
            ( toggleTableRow index model |> identifyProblemsIn, Cmd.none )

        DownloadStopwatch which zone time ->
            ( model, downloadFileMaybe (createSingleStopwatchDataFile which zone time model) )

        DeleteStopwatch which ->
            ( deleteStopwatch which model, Cmd.none )

        FlipStopwatches ->
            ( flipStopwatches model, Cmd.none )

        ClearAllData ->
            ( clearAllData model, Cmd.none )

        GetCurrentDateForDownloadFile operation ->
            ( model
            , Task.perform identity (Task.map2 operation Time.here Time.now)
            )

        DownloadMergedStopwatchData zone time ->
            ( model, downloadFileMaybe (createMergedStopwatchDataFile zone time model) )

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

        EditNumberCheckerRow entryNumber ->
            ( reunderlineStopwatchTable (editNumberCheckerRow entryNumber model), Cmd.none )

        DeleteNumberCheckerRow entryNumber ->
            ( reunderlineStopwatchTable (deleteNumberCheckerEntry entryNumber model), Cmd.none )

        EventDateChanged newEventDate ->
            ( identifyProblemsIn (handleEventDateChange newEventDate model), Cmd.none )

        EventTimeChanged newEventTime ->
            let
                modelWithNewTime : Model
                modelWithNewTime =
                    handleEventTimeChange newEventTime model

                command : Cmd Msg
                command =
                    Maybe.map recordEventStartTime modelWithNewTime.eventDateAndTime.validatedTime
                        |> Maybe.withDefault Cmd.none
            in
            ( identifyProblemsIn modelWithNewTime, command )

        NumberCheckerFieldChanged fieldChange newValue ->
            ( handleNumberCheckerFieldChange fieldChange newValue model, Cmd.none )

        AddNumberCheckerRow ->
            let
                ( addedToModel, issueFocusCommand ) =
                    addNumberCheckerRow model

                command : Cmd Msg
                command =
                    select issueFocusCommand (focus "number-checker-stopwatch-1") Cmd.none
            in
            ( reunderlineStopwatchTable addedToModel, command )

        IncrementNumberCheckerRowActualCount entryNumber ->
            ( reunderlineStopwatchTable (modifyNumberCheckerRows 1 entryNumber model), Cmd.none )

        DecrementNumberCheckerRowActualCount entryNumber ->
            ( reunderlineStopwatchTable (modifyNumberCheckerRows -1 entryNumber model), Cmd.none )

        FixProblem problemFix ->
            ( fixProblem problemFix model
                |> identifyProblemsIn
                |> reunderlineStopwatchTable
            , Cmd.none
            )

        ChangeSecondTab newSecondTab ->
            ( { model | secondTab = newSecondTab }, Cmd.none )

        ChangeBarcodeScannerTab newBarcodeScannerTab ->
            ( { model | barcodeScannerTab = newBarcodeScannerTab }, Cmd.none )

        ClearErrors ->
            ( { model | lastErrors = [] }, Cmd.none )

        DownloadBarcodeScannerFile index zone time ->
            ( model, downloadFileMaybe (createSingleBarcodeScannerData index model.barcodeScannerData.files zone time) )

        DeleteBarcodeScannerFile fileName ->
            ( deleteBarcodeScannerFileWithName fileName model, Cmd.none )

        IgnoreProblem problemIndex ->
            ( ignoreProblem problemIndex model, Cmd.none )

        ShowBarcodeScannerEditModal location contents isDeleted ->
            ( { model
                | dialogDetails =
                    BarcodeScannerRowEditDialog (BarcodeScannerEditing.startEditing location contents isDeleted)
              }
            , focus (BarcodeScannerEditing.elementToFocusWhenOpening contents)
            )

        BarcodeScannerEdit editChange ->
            let
                newEditDetails : DialogDetails
                newEditDetails =
                    case model.dialogDetails of
                        BarcodeScannerRowEditDialog barcodeScannerRowEditDetails ->
                            updateEditDetails editChange barcodeScannerRowEditDetails
                                |> BarcodeScannerRowEditDialog

                        _ ->
                            model.dialogDetails
            in
            ( { model | dialogDetails = newEditDetails }, Cmd.none )

        UpdateRowFromBarcodeScannerEditModal location athlete finishPosition ->
            ( { model
                | dialogDetails = NoDialog
                , barcodeScannerData = updateBarcodeScannerLine location.fileName location.lineNumber athlete finishPosition model.barcodeScannerData
              }
                |> identifyProblemsIn
            , Cmd.none
            )

        DeleteRowFromBarcodeScannerEditModal location ->
            ( { model
                | dialogDetails = NoDialog
                , barcodeScannerData = deleteBarcodeScannerLine location.fileName location.lineNumber model.barcodeScannerData
              }
                |> identifyProblemsIn
            , Cmd.none
            )

        CloseModal ->
            ( { model | dialogDetails = NoDialog }, Cmd.none )
