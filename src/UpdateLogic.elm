module UpdateLogic exposing (barcodeScannerFileMimeType, stopwatchFileMimeType, update)

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , deleteBarcodeScannerLine
        , generateDownloadText
        , lastTokenUsed
        , regenerate
        , updateBarcodeScannerLine
        )
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails)
import Bootstrap.Tab as Tab
import Commands exposing (Command(..), ElementToFocus(..))
import DateHandling exposing (generateDownloadFilenameDatePart)
import Dict
import EventDateAndTime exposing (EventDateAndTime)
import EventDateAndTimeEditing exposing (handleEventDateChange, handleEventTimeChange)
import File exposing (File)
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
import ProblemFixing exposing (fixProblem)
import Problems exposing (Problem, identifyProblems)
import Stopwatch
    exposing
        ( DoubleStopwatchData
        , MergedTableRow
        , Stopwatches(..)
        , WhichStopwatch(..)
        , flipMatchSummary
        , flipTable
        , outputMergedTable
        , outputSingleStopwatchData
        , toggleRowInTable
        , underlineTable
        )
import Task exposing (Task)
import Time exposing (Posix, Zone)
import TokenOperations


stopwatchFileMimeType : String
stopwatchFileMimeType =
    "text/csv"


barcodeScannerFileMimeType : String
barcodeScannerFileMimeType =
    "text/csv"


{-| Maximum size of a file that we allow to be uploaded.
-}
maxFileSize : Int
maxFileSize =
    1048576


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


createSingleStopwatchDataFile : WhichStopwatch -> Zone -> Posix -> Model -> Command
createSingleStopwatchDataFile whichStopwatch zone time model =
    let
        downloadTextMaybe : Maybe String
        downloadTextMaybe =
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
    case downloadTextMaybe of
        Just downloadText ->
            createStopwatchFileForDownload zone time downloadText
                |> DownloadFile stopwatchFileMimeType

        Nothing ->
            NoCommand


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
                    , matchSummary = flipMatchSummary oldDoubleStopwatchData.matchSummary
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


createMergedStopwatchDataFile : Zone -> Posix -> Model -> Command
createMergedStopwatchDataFile zone time model =
    case model.stopwatches of
        None ->
            NoCommand

        Single _ _ ->
            NoCommand

        Double doubleStopwatchData ->
            createStopwatchFileForDownload zone time (outputMergedTable doubleStopwatchData.mergedTableRows)
                |> DownloadFile stopwatchFileMimeType


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


createSingleBarcodeScannerData : String -> List BarcodeScannerFile -> Zone -> Posix -> Command
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
            DownloadFile barcodeScannerFileMimeType (InteropFile downloadFileName downloadFileContents)

        Nothing ->
            -- No file with that name was found.
            NoCommand


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


updateRowFromBarcodeScannerEditModal : BarcodeScannerRowEditDetails -> Model -> Model
updateRowFromBarcodeScannerEditModal rowEditDetails model =
    let
        athlete : String
        athlete =
            case rowEditDetails.athleteEntered.parsedValue of
                Just athleteNum ->
                    "A" ++ String.fromInt athleteNum

                Nothing ->
                    ""
    in
    { model
        | dialogDetails = NoDialog
        , barcodeScannerData =
            updateBarcodeScannerLine
                rowEditDetails.location.fileName
                rowEditDetails.location.lineNumber
                athlete
                rowEditDetails.finishPositionEntered.parsedValue
                model.barcodeScannerData
    }
        |> identifyProblemsIn


update : Msg -> Model -> ( Model, Command )
update msg model =
    case msg of
        NoOp ->
            ( model, NoCommand )

        FilesDropped files ->
            ( handleFilesDropped files model
                |> identifyProblemsIn
                |> reunderlineStopwatchTable
            , NoCommand
            )

        ToggleTableRow index ->
            ( toggleTableRow index model |> identifyProblemsIn, NoCommand )

        DownloadStopwatch which zone time ->
            ( model, createSingleStopwatchDataFile which zone time model )

        DeleteStopwatch which ->
            ( deleteStopwatch which model, NoCommand )

        FlipStopwatches ->
            ( flipStopwatches model, NoCommand )

        ClearAllData ->
            ( clearAllData model, NoCommand )

        GetCurrentDateForDownloadFile operation ->
            ( model, GetCurrentDateAndTime operation )

        DownloadMergedStopwatchData zone time ->
            ( model, createMergedStopwatchDataFile zone time model )

        ContainerHeightChanged newHeight ->
            ( { model | lastHeight = Just newHeight }, NoCommand )

        MouseEnterNumberCheckerRow highlightRow ->
            ( { model | highlightedNumberCheckerId = Just highlightRow }, NoCommand )

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
            ( newModel, NoCommand )

        EditNumberCheckerRow entryNumber ->
            ( reunderlineStopwatchTable (editNumberCheckerRow entryNumber model), NoCommand )

        DeleteNumberCheckerRow entryNumber ->
            ( reunderlineStopwatchTable (deleteNumberCheckerEntry entryNumber model), NoCommand )

        EventDateChanged newEventDate ->
            ( identifyProblemsIn (handleEventDateChange newEventDate model), NoCommand )

        EventTimeChanged newEventTime ->
            let
                modelWithNewTime : Model
                modelWithNewTime =
                    handleEventTimeChange newEventTime model

                command : Command
                command =
                    Maybe.map SaveEventStartTime modelWithNewTime.eventDateAndTime.validatedTime
                        |> Maybe.withDefault NoCommand
            in
            ( identifyProblemsIn modelWithNewTime, command )

        NumberCheckerFieldChanged fieldChange newValue ->
            ( handleNumberCheckerFieldChange fieldChange newValue model, NoCommand )

        AddNumberCheckerRow ->
            let
                ( addedToModel, issueFocusCommand ) =
                    addNumberCheckerRow model

                command : Command
                command =
                    select issueFocusCommand (FocusElement NumberCheckerManualEntryRowFirstCell) NoCommand
            in
            ( reunderlineStopwatchTable addedToModel, command )

        IncrementNumberCheckerRowActualCount entryNumber ->
            ( reunderlineStopwatchTable (modifyNumberCheckerRows 1 entryNumber model), NoCommand )

        DecrementNumberCheckerRowActualCount entryNumber ->
            ( reunderlineStopwatchTable (modifyNumberCheckerRows -1 entryNumber model), NoCommand )

        FixProblem problemFix ->
            ( fixProblem problemFix model
                |> identifyProblemsIn
                |> reunderlineStopwatchTable
            , NoCommand
            )

        ChangeSecondTab newSecondTab ->
            ( { model | secondTab = newSecondTab }, NoCommand )

        ChangeBarcodeScannerTab newBarcodeScannerTab ->
            ( { model | barcodeScannerTab = newBarcodeScannerTab }, NoCommand )

        ClearErrors ->
            ( { model | lastErrors = [] }, NoCommand )

        DownloadBarcodeScannerFile index zone time ->
            ( model, createSingleBarcodeScannerData index model.barcodeScannerData.files zone time )

        DeleteBarcodeScannerFile fileName ->
            ( deleteBarcodeScannerFileWithName fileName model, NoCommand )

        IgnoreProblem problemIndex ->
            ( ignoreProblem problemIndex model, NoCommand )

        ShowBarcodeScannerEditModal location contents isDeleted ->
            ( { model
                | dialogDetails =
                    BarcodeScannerRowEditDialog (BarcodeScannerEditing.startEditing location contents isDeleted)
              }
            , FocusElement (BarcodeScannerEditing.elementToFocusWhenOpening contents)
            )

        BarcodeScannerEdit editChange ->
            let
                newEditDetails : DialogDetails
                newEditDetails =
                    case model.dialogDetails of
                        BarcodeScannerRowEditDialog barcodeScannerRowEditDetails ->
                            BarcodeScannerEditing.updateEditDetails editChange barcodeScannerRowEditDetails
                                |> BarcodeScannerRowEditDialog

                        _ ->
                            model.dialogDetails
            in
            ( { model | dialogDetails = newEditDetails }, NoCommand )

        UpdateRowFromBarcodeScannerEditModal rowEditDetails ->
            ( updateRowFromBarcodeScannerEditModal rowEditDetails model, NoCommand )

        DeleteRowFromBarcodeScannerEditModal location ->
            ( { model
                | dialogDetails = NoDialog
                , barcodeScannerData = deleteBarcodeScannerLine location.fileName location.lineNumber model.barcodeScannerData
              }
                |> identifyProblemsIn
            , NoCommand
            )

        ShowTokenOperationsModal ->
            ( { model | dialogDetails = TokenOperationsDialog TokenOperations.emptyEditDetails }
            , NoCommand
            )

        TokenOperationEdit editChange ->
            let
                lastToken : Int
                lastToken =
                    lastTokenUsed model.barcodeScannerData

                newEditDetails : DialogDetails
                newEditDetails =
                    case model.dialogDetails of
                        TokenOperationsDialog tokenOperationsEditDetails ->
                            TokenOperations.updateEditDetails lastToken editChange tokenOperationsEditDetails
                                |> TokenOperationsDialog

                        _ ->
                            model.dialogDetails
            in
            ( { model | dialogDetails = newEditDetails }, NoCommand )

        ApplyTokenOperation _ ->
            -- TODO
            ( model, NoCommand )

        CloseModal ->
            ( { model | dialogDetails = NoDialog }, NoCommand )

        OpenUploadFileDialog ->
            ( model, SelectFileForUpload )

        FilesUploaded firstFile otherFiles ->
            let
                allFiles : List File
                allFiles =
                    List.filter (\f -> File.size f <= maxFileSize) (firstFile :: otherFiles)
            in
            ( model, ReadFiles allFiles )

        ReturnKeyPressed ->
            case model.dialogDetails of
                NoDialog ->
                    ( model, NoCommand )

                BarcodeScannerRowEditDialog rowEditDetails ->
                    if rowEditDetails.validationError == Nothing then
                        ( updateRowFromBarcodeScannerEditModal rowEditDetails model, NoCommand )

                    else
                        ( model, NoCommand )

                TokenOperationsDialog tokenOperationEditDetails ->
                    if tokenOperationEditDetails.validationError == Nothing then
                        -- TODO
                        ( model, NoCommand )

                    else
                        ( model, NoCommand )
