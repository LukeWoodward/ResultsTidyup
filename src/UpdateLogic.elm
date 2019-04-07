module UpdateLogic exposing (createStopwatchFileForDownload, regenerateWithWrongWayArounds, update)

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        , WrongWayAroundStatus(..)
        , generateDownloadText
        , mergeScannerData
        , readBarcodeScannerData
        , regenerate
        )
import BarcodeScannerEditing exposing (BarcodeScannerRowEditDetails, updateEditDetails)
import Browser.Dom
import DataStructures exposing (EventDateAndTime, InteropFile, ProblemFix(..), WhichStopwatch(..))
import DateHandling exposing (dateStringToPosix, dateToString, generateDownloadFilenameDatePart)
import Dict
import Error exposing (Error, FileError, mapError)
import EventDateAndTimeEditing exposing (handleEventDateChange, handleEventTimeChange)
import File.Download as Download
import MergedTable
    exposing
        ( DoubleStopwatchData
        , MergedTableRow
        , Stopwatches(..)
        , createMergedTable
        , flipTable
        , generateInitialTable
        , outputMergedTable
        , toggleRowInTable
        , underlineTable
        )
import Merger exposing (MergeEntry, merge)
import Model exposing (Model, NumberCheckerManualEntryRow, ProblemEntry, initModel)
import Msg exposing (Msg(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, annotate, parseNumberCheckerFile, reannotate)
import NumberCheckerEditing
    exposing
        ( addNumberCheckerRow
        , deleteNumberCheckerEntry
        , editNumberCheckerRow
        , handleNumberCheckerFieldChange
        , modifyNumberCheckerRows
        )
import Parser exposing ((|.), Parser, chompIf, chompWhile, end, int, run, symbol)
import Parsers exposing (digitsRange)
import Ports exposing (recordEventStartTime)
import ProblemFixing exposing (fixProblem)
import Problems exposing (Problem, identifyProblems)
import Regex exposing (Regex)
import Result.Extra
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import Task exposing (Task)
import Time exposing (Posix, Zone)
import WrongWayAround exposing (identifyBarcodesScannedTheWrongWayAround)


focus : String -> Cmd Msg
focus elementId =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus elementId)


hasFileAlreadyBeenUploaded : String -> Stopwatches -> Bool
hasFileAlreadyBeenUploaded newFileName stopwatches =
    case stopwatches of
        None ->
            False

        Single existingFilename _ ->
            newFileName == existingFilename

        Double doubleStopwatchData ->
            newFileName == doubleStopwatchData.filename1 || newFileName == doubleStopwatchData.filename2


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


setEventDateAndTimeIn : Model -> Model
setEventDateAndTimeIn model =
    let
        newEventDate : Maybe String
        newEventDate =
            model.barcodeScannerData.lastScanDate
                |> Maybe.map dateToString
    in
    case ( newEventDate, model.eventDateAndTime.validatedDate ) of
        ( Just _, Just _ ) ->
            -- Already have an event date so leave it.
            model

        ( Just newDateString, Nothing ) ->
            -- We've got an event date now and we didn't before.
            handleEventDateChange newDateString model

        ( Nothing, _ ) ->
            -- No event date read so leave things as they were.
            model


mergeProblems : List ProblemEntry -> List Problem -> List ProblemEntry
mergeProblems currentProblemEntries newProblems =
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
        | problems = mergeProblems model.problems newProblems
    }


handleStopwatchFileDrop : String -> String -> Model -> Model
handleStopwatchFileDrop fileName fileText model =
    case readStopwatchData fileText of
        Ok (StopwatchData newStopwatch) ->
            if hasFileAlreadyBeenUploaded fileName model.stopwatches then
                { model
                    | lastErrors =
                        model.lastErrors
                            ++ [ FileError
                                    "STOPWATCH_FILE_ALREADY_LOADED"
                                    "That stopwatch data file has already been loaded"
                                    fileName
                               ]
                }

            else
                let
                    newStopwatches =
                        case model.stopwatches of
                            None ->
                                Single fileName newStopwatch

                            Single existingFilename firstStopwatch ->
                                createMergedTable firstStopwatch newStopwatch existingFilename fileName

                            Double _ ->
                                model.stopwatches

                    underlinedStopwatches =
                        underlineStopwatches newStopwatches model.numberCheckerEntries
                in
                identifyProblemsIn
                    { model
                        | stopwatches = underlinedStopwatches
                        , lastErrors = []
                    }

        Err error ->
            { model | lastErrors = model.lastErrors ++ [ mapError fileName error ] }


isNumberCheckerDigit : Char -> Bool
isNumberCheckerDigit c =
    Char.isDigit c || c == '\u{000D}' || c == '\n' || c == ','


numberCheckerParser : Parser ()
numberCheckerParser =
    chompIf isNumberCheckerDigit
        |. chompWhile isNumberCheckerDigit
        |. end


isPossibleNumberCheckerFile : String -> Bool
isPossibleNumberCheckerFile fileText =
    Result.Extra.isOk (run numberCheckerParser fileText)


handleNumberCheckerFileDrop : String -> String -> Model -> Model
handleNumberCheckerFileDrop fileName fileText model =
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
                | lastErrors = model.lastErrors ++ [ mapError fileName error ]
            }


barcodeScannerRegex : Regex
barcodeScannerRegex =
    Regex.fromString "A[0-9]+,P[0-9]+,"
        |> Maybe.withDefault Regex.never


isPossibleBarcodeScannerFile : String -> Bool
isPossibleBarcodeScannerFile fileText =
    Regex.contains barcodeScannerRegex fileText


handleBarcodeScannerFileDrop : String -> String -> Model -> Model
handleBarcodeScannerFileDrop fileName fileText model =
    if List.any (\file -> file.name == fileName) model.barcodeScannerData.files then
        { model
            | lastErrors =
                model.lastErrors
                    ++ [ FileError
                            "BARCODE_DATA_ALREADY_LOADED"
                            "That barcode scanner file has already been loaded"
                            fileName
                       ]
        }

    else
        let
            result : Result Error BarcodeScannerData
            result =
                readBarcodeScannerData fileName fileText
        in
        case result of
            Ok scannerData ->
                { model
                    | barcodeScannerData = identifyBarcodesScannedTheWrongWayAround (mergeScannerData model.barcodeScannerData scannerData)
                }
                    |> setEventDateAndTimeIn
                    |> identifyProblemsIn

            Err error ->
                { model | lastErrors = model.lastErrors ++ [ mapError fileName error ] }


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


handleFileDropped : InteropFile -> Model -> Model
handleFileDropped { fileName, fileText } model =
    if String.startsWith "STARTOFEVENT" fileText then
        handleStopwatchFileDrop fileName fileText model

    else if isPossibleNumberCheckerFile fileText then
        handleNumberCheckerFileDrop fileName fileText model

    else if isPossibleBarcodeScannerFile fileText then
        handleBarcodeScannerFileDrop fileName fileText model

    else
        { model
            | lastErrors = model.lastErrors ++ [ FileError "UNRECOGNISED_FILE" "File was unrecognised" fileName ]
        }


handleFilesDropped : List InteropFile -> Model -> Model
handleFilesDropped files model =
    let
        sortedFiles : List InteropFile
        sortedFiles =
            List.sortBy .fileName files
                |> List.reverse

        modelWithNoErrors : Model
        modelWithNoErrors =
            { model | lastErrors = [] }
    in
    List.foldr handleFileDropped modelWithNoErrors sortedFiles


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


regenerateWithWrongWayArounds : BarcodeScannerData -> BarcodeScannerData
regenerateWithWrongWayArounds barcodeScannerData =
    identifyBarcodesScannedTheWrongWayAround barcodeScannerData
        |> regenerate


regenerateWithWrongWayAroundsIn : Model -> Model
regenerateWithWrongWayAroundsIn model =
    { model | barcodeScannerData = regenerateWithWrongWayArounds model.barcodeScannerData }


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
            | barcodeScannerData = regenerateWithWrongWayArounds { barcodeScannerData | files = deleteAtIndex index model.barcodeScannerData.files }
        }


swapBarcodesAroundInLines : Int -> Int -> List BarcodeScannerFileLine -> List BarcodeScannerFileLine
swapBarcodesAroundInLines first last lines =
    case lines of
        [] ->
            []

        singleLine :: [] ->
            if singleLine.lineNumber == last then
                [ { singleLine | deletionStatus = Deleted EndOfWrongWayAroundSection, wrongWayAroundStatus = NotWrongWayAround } ]

            else
                [ singleLine ]

        firstLine :: secondLine :: remainingLines ->
            if firstLine.lineNumber == last then
                { firstLine | deletionStatus = Deleted EndOfWrongWayAroundSection, wrongWayAroundStatus = NotWrongWayAround } :: secondLine :: remainingLines

            else if first <= firstLine.lineNumber && firstLine.lineNumber < last then
                case ( firstLine.contents, secondLine.contents ) of
                    ( Ordinary _ thisPosition, Ordinary nextAthlete _ ) ->
                        { firstLine
                            | contents = Ordinary nextAthlete thisPosition
                            , wrongWayAroundStatus = NotWrongWayAround
                        }
                            :: swapBarcodesAroundInLines first last (secondLine :: remainingLines)

                    _ ->
                        -- Mis-scans?  Unexpected.
                        firstLine :: swapBarcodesAroundInLines first last (secondLine :: remainingLines)

            else
                firstLine :: swapBarcodesAroundInLines first last (secondLine :: remainingLines)


swapBarcodesAroundInFile : Int -> Int -> BarcodeScannerFile -> BarcodeScannerFile
swapBarcodesAroundInFile first last file =
    { file | lines = swapBarcodesAroundInLines first last file.lines }


swapBarcodesAround : String -> Int -> Int -> Model -> Model
swapBarcodesAround fileName first last model =
    let
        matchingFileMaybe : Maybe BarcodeScannerFile
        matchingFileMaybe =
            List.filter (\f -> f.name == fileName) model.barcodeScannerData.files
                |> List.head
    in
    case matchingFileMaybe of
        Just matchingFile ->
            let
                swappedFile : BarcodeScannerFile
                swappedFile =
                    swapBarcodesAroundInFile first last matchingFile

                swapFileBackIn : BarcodeScannerFile -> BarcodeScannerFile
                swapFileBackIn file =
                    if file.name == fileName then
                        swappedFile

                    else
                        file

                originalBarcodeScannerData : BarcodeScannerData
                originalBarcodeScannerData =
                    model.barcodeScannerData
            in
            identifyProblemsIn
                { model
                    | barcodeScannerData =
                        regenerateWithWrongWayArounds
                            { originalBarcodeScannerData
                                | files = List.map swapFileBackIn originalBarcodeScannerData.files
                            }
                }

        Nothing ->
            model


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
            ( handleFilesDropped files model, Cmd.none )

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
                |> regenerateWithWrongWayAroundsIn
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

        SwapBarcodes fileName first last ->
            ( swapBarcodesAround fileName first last model, Cmd.none )

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
