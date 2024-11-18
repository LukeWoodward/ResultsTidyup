module UpdateLogic exposing (barcodeScannerFileMimeType, timerFileMimeType, update)

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , deleteBarcodeScannerLine
        , generateDownloadText
        , generateDownloadTextForAllScanners
        , regenerate
        )
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails, BarcodeScannerValidationError, tryUpdateBarcodeScannerLine)
import Bootstrap.Tab as Tab
import Commands exposing (Command(..), ElementToFocus(..))
import DateHandling exposing (generateFilenameDatePart, generateNameOfPastedFile)
import File exposing (File)
import FileDropHandling exposing (handleFilesAdded)
import FileHandling exposing (AddedFile, InteropFile, deduceNameFromFilename)
import Model exposing (DialogDetails(..), Model, emptyNumberCheckerManualEntryRow)
import Msg exposing (Msg(..))
import NumberCheckerEditing
    exposing
        ( addNumberCheckerRow
        , deleteNumberCheckerEntry
        , editNumberCheckerRow
        , handleNumberCheckerFieldChange
        , modifyNumberCheckerRows
        )
import PastedFile exposing (PastedFileDetails, interpretPastedFile)
import ProblemFixing exposing (fixProblem, ignoreProblem)
import Problems exposing (Problems, identifyProblems, noIgnoredProblems, noProblems)
import Time exposing (Posix, Zone)
import Timer
    exposing
        ( DoubleTimerData
        , MergedTableRow
        , Timers(..)
        , WhichTimer(..)
        , flipMatchSummary
        , flipTable
        , outputMergedTable
        , outputSingleTimerData
        , toggleRowInTable
        , underlineTable
        )
import TimerOperations exposing (TimerOperationEditDetails, tryApplyOperationToTimerData)
import TokenOperations exposing (TokenOperationEditDetails, TokenOperationValidationError(..), tryApplyTokenOperationToBarcodeScannerData)


timerFileMimeType : String
timerFileMimeType =
    "text/csv"


barcodeScannerFileMimeType : String
barcodeScannerFileMimeType =
    "text/csv"


{-| Maximum size of a file that we allow to be uploaded.
-}
maxFileSize : Int
maxFileSize =
    1048576


identifyProblemsIn : Model -> Model
identifyProblemsIn model =
    let
        newProblems : Problems
        newProblems =
            identifyProblems model.timers model.barcodeScannerData model.ignoredProblems
    in
    { model | problems = newProblems }


toggleTableRow : Int -> Model -> Model
toggleTableRow index model =
    case model.timers of
        None ->
            model

        Single _ _ ->
            model

        Double doubleTimerData ->
            let
                newMergedTable : List MergedTableRow
                newMergedTable =
                    doubleTimerData.mergedTableRows
                        |> toggleRowInTable index
                        |> underlineTable model.numberCheckerEntries
            in
            { model
                | timers = Double { doubleTimerData | mergedTableRows = newMergedTable }
            }


createSingleTimerDataFile : WhichTimer -> Zone -> Posix -> Model -> Command
createSingleTimerDataFile whichTimer zone time model =
    let
        downloadTextMaybe : Maybe String
        downloadTextMaybe =
            case ( model.timers, whichTimer ) of
                ( None, _ ) ->
                    Nothing

                ( Single _ times, TimerOne ) ->
                    Just (outputSingleTimerData times)

                ( Single _ _, TimerTwo ) ->
                    Nothing

                ( Double doubleTimerData, TimerOne ) ->
                    Just (outputSingleTimerData doubleTimerData.times1)

                ( Double doubleTimerData, TimerTwo ) ->
                    Just (outputSingleTimerData doubleTimerData.times2)
    in
    case downloadTextMaybe of
        Just downloadText ->
            createTimerFileForDownload zone time downloadText
                |> DownloadFile timerFileMimeType

        Nothing ->
            NoCommand


removeTimer : WhichTimer -> Model -> Model
removeTimer which model =
    case ( model.timers, which ) of
        ( None, _ ) ->
            model

        ( Single _ _, TimerOne ) ->
            identifyProblemsIn { model | timers = None }

        ( Single _ _, TimerTwo ) ->
            model

        ( Double doubleTimerData, TimerOne ) ->
            let
                newTimers : Timers
                newTimers =
                    Single doubleTimerData.file2 doubleTimerData.times2
            in
            identifyProblemsIn
                { model | timers = newTimers }

        ( Double doubleTimerData, TimerTwo ) ->
            let
                newTimers : Timers
                newTimers =
                    Single doubleTimerData.file1 doubleTimerData.times1
            in
            identifyProblemsIn
                { model | timers = newTimers }


flipTimers : Model -> Model
flipTimers model =
    case model.timers of
        None ->
            model

        Single _ _ ->
            model

        Double oldDoubleTimerData ->
            let
                newMergedTableRows : List MergedTableRow
                newMergedTableRows =
                    oldDoubleTimerData.mergedTableRows
                        |> flipTable
                        |> underlineTable model.numberCheckerEntries

                newDoubleTimerData : DoubleTimerData
                newDoubleTimerData =
                    { times1 = oldDoubleTimerData.times2
                    , times2 = oldDoubleTimerData.times1
                    , file1 = oldDoubleTimerData.file2
                    , file2 = oldDoubleTimerData.file1
                    , mergedTableRows = newMergedTableRows
                    , matchSummary = flipMatchSummary oldDoubleTimerData.matchSummary
                    }
            in
            identifyProblemsIn { model | timers = Double newDoubleTimerData }


clearAllData : Model -> Model
clearAllData model =
    { model
        | timers = None
        , lastErrors = []
        , numberCheckerEntries = []
        , highlightedNumberCheckerId = Nothing
        , barcodeScannerData = BarcodeScanner.empty
        , problems = noProblems
        , ignoredProblems = noIgnoredProblems
        , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
        , barcodeScannerTab = Tab.initialState
        , dialogDetails = NoDialog
    }


createTimerFileForDownload : Zone -> Posix -> String -> InteropFile
createTimerFileForDownload zone time fileContents =
    let
        fileName : String
        fileName =
            "results_tidyup_timer_" ++ generateFilenameDatePart zone time ++ ".txt"
    in
    InteropFile fileName fileContents


createMergedTimerDataFile : Zone -> Posix -> Model -> Command
createMergedTimerDataFile zone time model =
    case model.timers of
        None ->
            NoCommand

        Single _ _ ->
            NoCommand

        Double doubleTimerData ->
            createTimerFileForDownload zone time (outputMergedTable doubleTimerData.mergedTableRows)
                |> DownloadFile timerFileMimeType


reunderlineTimerTable : Model -> Model
reunderlineTimerTable model =
    case model.timers of
        Double doubleTimerData ->
            let
                newMergedTable : List MergedTableRow
                newMergedTable =
                    underlineTable model.numberCheckerEntries doubleTimerData.mergedTableRows
            in
            { model
                | timers = Double { doubleTimerData | mergedTableRows = newMergedTable }
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
            List.filter (\file -> file.filename == fileName) files
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
                    "results_tidyup_barcode_" ++ generateFilenameDatePart zone time ++ ".txt"
            in
            DownloadFile barcodeScannerFileMimeType (InteropFile downloadFileName downloadFileContents)

        Nothing ->
            -- No file with that name was found.
            NoCommand


createAllBarcodeScannerData : List BarcodeScannerFile -> Zone -> Posix -> Command
createAllBarcodeScannerData files zone time =
    let
        downloadFileContents : String
        downloadFileContents =
            generateDownloadTextForAllScanners files

        downloadFileName : String
        downloadFileName =
            "results_tidyup_barcode_" ++ generateFilenameDatePart zone time ++ ".txt"
    in
    DownloadFile barcodeScannerFileMimeType (InteropFile downloadFileName downloadFileContents)


removeBarcodeScannerFileWithName : String -> Model -> Model
removeBarcodeScannerFileWithName fileName model =
    let
        barcodeScannerData : BarcodeScannerData
        barcodeScannerData =
            model.barcodeScannerData
    in
    identifyProblemsIn
        { model
            | barcodeScannerData = regenerate { barcodeScannerData | files = List.filter (\file -> file.filename /= fileName) model.barcodeScannerData.files }
        }


select : Bool -> a -> a -> a
select condition trueValue falseValue =
    if condition then
        trueValue

    else
        falseValue


tryUpdateRowFromBarcodeScannerEditModal : BarcodeScannerRowEditDetails -> Model -> Model
tryUpdateRowFromBarcodeScannerEditModal rowEditDetails model =
    let
        updateBarcodeScannerLineResult : Result BarcodeScannerValidationError BarcodeScannerData
        updateBarcodeScannerLineResult =
            tryUpdateBarcodeScannerLine rowEditDetails model.barcodeScannerData
    in
    case updateBarcodeScannerLineResult of
        Ok barcodeScannerData ->
            { model | dialogDetails = NoDialog, barcodeScannerData = barcodeScannerData }
                |> identifyProblemsIn

        Err validationError ->
            { model | dialogDetails = BarcodeScannerRowEditDialog { rowEditDetails | validationError = Just validationError } }


tryApplyTimerOperation : TimerOperationEditDetails -> Model -> Model
tryApplyTimerOperation timerOperationEditDetails model =
    case tryApplyOperationToTimerData timerOperationEditDetails model.timers of
        Ok updatedTimers ->
            identifyProblemsIn
                { model
                    | timers = updatedTimers
                    , dialogDetails = NoDialog
                }

        Err validationError ->
            { model
                | dialogDetails = TimerOperationsDialog { timerOperationEditDetails | validationError = validationError }
            }


tryApplyTokenOperation : TokenOperationEditDetails -> Model -> Model
tryApplyTokenOperation tokenOperationEditDetails model =
    case tryApplyTokenOperationToBarcodeScannerData tokenOperationEditDetails model.barcodeScannerData of
        Ok updatedBarcodeScannerData ->
            identifyProblemsIn
                { model
                    | barcodeScannerData = updatedBarcodeScannerData
                    , dialogDetails = NoDialog
                }

        Err validationError ->
            { model
                | dialogDetails = TokenOperationsDialog { tokenOperationEditDetails | validationError = validationError }
            }


update : Msg -> Model -> ( Model, Command )
update msg model =
    case msg of
        NoOp ->
            ( model, NoCommand )

        FilesDropped files ->
            let
                mapFile : InteropFile -> AddedFile
                mapFile interopFile =
                    AddedFile interopFile.fileName (deduceNameFromFilename interopFile.fileName) interopFile.fileText

                addedFiles : List AddedFile
                addedFiles =
                    List.map mapFile files
            in
            ( handleFilesAdded addedFiles model
                |> identifyProblemsIn
                |> reunderlineTimerTable
            , NoCommand
            )

        FilesAdded files ->
            ( handleFilesAdded files model
                |> identifyProblemsIn
                |> reunderlineTimerTable
            , NoCommand
            )

        ToggleTableRow index ->
            ( toggleTableRow index model |> identifyProblemsIn, NoCommand )

        DownloadTimer which zone time ->
            ( model, createSingleTimerDataFile which zone time model )

        RemoveTimer which ->
            ( removeTimer which model, NoCommand )

        FlipTimers ->
            ( flipTimers model, NoCommand )

        ClearAllData ->
            ( clearAllData model, NoCommand )

        RequestCurrentDateAndTime operation ->
            ( model, GetCurrentDateAndTime operation )

        DownloadMergedTimerData zone time ->
            ( model, createMergedTimerDataFile zone time model )

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
            ( reunderlineTimerTable (editNumberCheckerRow entryNumber model), NoCommand )

        DeleteNumberCheckerRow entryNumber ->
            ( reunderlineTimerTable (deleteNumberCheckerEntry entryNumber model), NoCommand )

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
            ( reunderlineTimerTable addedToModel, command )

        IncrementNumberCheckerRowActualCount entryNumber ->
            ( reunderlineTimerTable (modifyNumberCheckerRows 1 entryNumber model), NoCommand )

        DecrementNumberCheckerRowActualCount entryNumber ->
            ( reunderlineTimerTable (modifyNumberCheckerRows -1 entryNumber model), NoCommand )

        FixProblem problemFix ->
            ( fixProblem problemFix model
                |> identifyProblemsIn
                |> reunderlineTimerTable
            , NoCommand
            )

        IgnoreProblem problemIgnorance ->
            ( identifyProblemsIn { model | ignoredProblems = ignoreProblem problemIgnorance model.ignoredProblems }, NoCommand )

        ChangeSecondTab newSecondTab ->
            ( { model | secondTab = newSecondTab }, NoCommand )

        ChangeBarcodeScannerTab newBarcodeScannerTab ->
            ( { model | barcodeScannerTab = newBarcodeScannerTab }, NoCommand )

        ClearErrors ->
            ( { model | lastErrors = [] }, NoCommand )

        DownloadBarcodeScannerFile filename zone time ->
            ( model, createSingleBarcodeScannerData filename model.barcodeScannerData.files zone time )

        DownloadAllBarcodeScannerData zone time ->
            ( model, createAllBarcodeScannerData model.barcodeScannerData.files zone time )

        RemoveBarcodeScannerFile fileName ->
            ( removeBarcodeScannerFileWithName fileName model, NoCommand )

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
            ( tryUpdateRowFromBarcodeScannerEditModal rowEditDetails model, NoCommand )

        DeleteRowFromBarcodeScannerEditModal location ->
            ( { model
                | dialogDetails = NoDialog
                , barcodeScannerData = deleteBarcodeScannerLine location.fileName location.lineNumber model.barcodeScannerData
              }
                |> identifyProblemsIn
            , NoCommand
            )

        ShowTimerOperationsModal ->
            ( { model | dialogDetails = TimerOperationsDialog (TimerOperations.emptyEditDetailsFromTimers model.timers) }
            , NoCommand
            )

        TimerOperationEdit editChange ->
            let
                newEditDetails : DialogDetails
                newEditDetails =
                    case model.dialogDetails of
                        TimerOperationsDialog editDetails ->
                            TimerOperations.updateEditDetails editChange editDetails
                                |> TimerOperationsDialog

                        _ ->
                            model.dialogDetails
            in
            ( { model | dialogDetails = newEditDetails }, NoCommand )

        ApplyTimerOperation editDetails ->
            ( tryApplyTimerOperation editDetails model, NoCommand )

        ShowTokenOperationsModal ->
            ( { model | dialogDetails = TokenOperationsDialog TokenOperations.emptyEditDetails }
            , NoCommand
            )

        TokenOperationEdit editChange ->
            let
                newEditDetails : DialogDetails
                newEditDetails =
                    case model.dialogDetails of
                        TokenOperationsDialog tokenOperationsEditDetails ->
                            TokenOperations.updateEditDetails editChange tokenOperationsEditDetails
                                |> TokenOperationsDialog

                        _ ->
                            model.dialogDetails
            in
            ( { model | dialogDetails = newEditDetails }, NoCommand )

        ApplyTokenOperation tokenOperationEditDetails ->
            ( tryApplyTokenOperation tokenOperationEditDetails model, NoCommand )

        CloseModal ->
            ( { model | dialogDetails = NoDialog }, NoCommand )

        OpenUploadFileDialog ->
            ( model, SelectFileForUpload )

        OpenPasteFileDialog ->
            ( { model | dialogDetails = PasteFileDialog PastedFile.empty }
            , FocusElement PasteFileDialogTextArea
            )

        OpenConfirmClearEverythingDialog ->
            ( { model | dialogDetails = ConfirmClearEverythingDialog }
            , NoCommand
            )

        PastedFileChanged newContents ->
            ( { model | dialogDetails = PasteFileDialog (PastedFileDetails newContents (interpretPastedFile newContents)) }
            , NoCommand
            )

        PastedFileUploaded contents zone time ->
            let
                fileName : String
                fileName =
                    "pasted_file_" ++ generateFilenameDatePart zone time ++ ".txt"

                name : String
                name =
                    "File pasted at " ++ generateNameOfPastedFile zone time
            in
            ( handleFilesAdded [ AddedFile fileName name contents ] { model | dialogDetails = NoDialog }
                |> identifyProblemsIn
                |> reunderlineTimerTable
            , NoCommand
            )

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
                        ( tryUpdateRowFromBarcodeScannerEditModal rowEditDetails model, NoCommand )

                    else
                        ( model, NoCommand )

                TimerOperationsDialog timerOperationEditDetails ->
                    ( tryApplyTimerOperation timerOperationEditDetails model, NoCommand )

                TokenOperationsDialog tokenOperationEditDetails ->
                    ( tryApplyTokenOperation tokenOperationEditDetails model, NoCommand )

                PasteFileDialog _ ->
                    ( model, NoCommand )

                ConfirmClearEverythingDialog ->
                    ( model, NoCommand )
