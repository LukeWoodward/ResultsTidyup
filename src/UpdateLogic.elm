module UpdateLogic exposing (createStopwatchFileForDownload, update)

import BarcodeScanner exposing (BarcodeScannerData, BarcodeScannerFile, generateDownloadText, regenerate)
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails, updateEditDetails)
import Browser.Dom
import DataStructures exposing (EventDateAndTime, InteropFile, WhichStopwatch(..))
import DateHandling exposing (generateDownloadFilenameDatePart)
import Dict
import EventDateAndTimeEditing exposing (handleEventDateChange, handleEventTimeChange)
import File.Download as Download
import FileDropHandling exposing (handleFilesDropped)
import MergedTable
    exposing
        ( DoubleStopwatchData
        , MergedTableRow
        , Stopwatches(..)
        , flipTable
        , outputMergedTable
        , toggleRowInTable
        , underlineTable
        )
import Model exposing (Model, NumberCheckerManualEntryRow, ProblemEntry, initModel)
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
    { initModel
        | secondTab = model.secondTab
        , eventDateAndTime = EventDateAndTime "" Nothing model.eventDateAndTime.enteredTime model.eventDateAndTime.validatedTime
    }


createStopwatchFileForDownload : Zone -> Posix -> List MergedTableRow -> InteropFile
createStopwatchFileForDownload zone time mergedTableRows =
    let
        fileContents : String
        fileContents =
            outputMergedTable mergedTableRows

        fileName : String
        fileName =
            "results_tidyup_timer_" ++ generateDownloadFilenameDatePart zone time ++ ".txt"
    in
    InteropFile fileName fileContents


downloadFile : InteropFile -> Cmd Msg
downloadFile interopFile =
    Download.string interopFile.fileName "text/csv" interopFile.fileText


downloadMergedStopwatchDataCommand : Zone -> Posix -> Model -> Cmd Msg
downloadMergedStopwatchDataCommand zone time model =
    case model.stopwatches of
        None ->
            Cmd.none

        Single _ _ ->
            Cmd.none

        Double doubleStopwatchData ->
            createStopwatchFileForDownload zone time doubleStopwatchData.mergedTableRows
                |> downloadFile


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


downloadSingleBarcodeScannerData : Int -> List BarcodeScannerFile -> Zone -> Posix -> Cmd Msg
downloadSingleBarcodeScannerData index files zone time =
    case ( index, files ) of
        ( _, [] ) ->
            Cmd.none

        ( 0, first :: _ ) ->
            let
                fileContents : String
                fileContents =
                    generateDownloadText first

                fileName : String
                fileName =
                    "results_tidyup_barcode_" ++ generateDownloadFilenameDatePart zone time ++ ".txt"
            in
            InteropFile fileName fileContents
                |> downloadFile

        ( _, _ :: rest ) ->
            downloadSingleBarcodeScannerData (index - 1) rest zone time


deleteAtIndex : Int -> List a -> List a
deleteAtIndex index items =
    case ( index, items ) of
        ( _, [] ) ->
            []

        ( 0, _ :: rest ) ->
            rest

        ( _, first :: rest ) ->
            first :: deleteAtIndex (index - 1) rest


deleteBarcodeScannerFileAtIndex : Int -> Model -> Model
deleteBarcodeScannerFileAtIndex index model =
    let
        barcodeScannerData : BarcodeScannerData
        barcodeScannerData =
            model.barcodeScannerData
    in
    identifyProblemsIn
        { model
            | barcodeScannerData = regenerate { barcodeScannerData | files = deleteAtIndex index model.barcodeScannerData.files }
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
            ( toggleTableRow index model, Cmd.none )

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
            ( model, downloadMergedStopwatchDataCommand zone time model )

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
            ( model, downloadSingleBarcodeScannerData index model.barcodeScannerData.files zone time )

        DeleteBarcodeScannerFile index ->
            ( deleteBarcodeScannerFileAtIndex index model, Cmd.none )

        IgnoreProblem problemIndex ->
            ( ignoreProblem problemIndex model, Cmd.none )

        ShowBarcodeScannerEditModal location contents ->
            ( { model
                | barcodeScannerRowEditDetails =
                    Just (BarcodeScannerEditing.startEditing location contents)
              }
            , Cmd.none
            )

        BarcodeScannerEdit editChange ->
            let
                newEditDetails : Maybe BarcodeScannerRowEditDetails
                newEditDetails =
                    Maybe.map (updateEditDetails editChange) model.barcodeScannerRowEditDetails
            in
            ( { model | barcodeScannerRowEditDetails = newEditDetails }, Cmd.none )

        CloseBarcodeScannerEditModal ->
            ( { model | barcodeScannerRowEditDetails = Nothing }, Cmd.none )
