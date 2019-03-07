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
import Browser.Dom
import DataStructures exposing (EventDateAndTime, InteropFile, ProblemFix(..), WhichStopwatch(..))
import DateHandling exposing (dateStringToPosix, dateToString, generateDownloadFilenameDatePart)
import Dict
import Error exposing (Error, FileError, mapError)
import File.Download as Download
import MergedTable
    exposing
        ( DoubleStopwatchData
        , MergedTableRow
        , Stopwatches(..)
        , flipTable
        , generateInitialTable
        , outputMergedTable
        , toggleRowInTable
        , underlineTable
        )
import Merger exposing (MergeEntry, merge)
import Model exposing (Model, NumberCheckerManualEntryRow, NumericEntry, ProblemEntry, emptyNumberCheckerManualEntryRow, initModel)
import Msg exposing (Msg(..), NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, addAndAnnotate, annotate, parseNumberCheckerFile, reannotate)
import Parser exposing ((|.), Parser, chompIf, chompWhile, end, int, run, symbol)
import Parsers exposing (digitsRange)
import Ports exposing (recordEventStartTime)
import Problems exposing (Problem, identifyProblems)
import Regex exposing (Regex)
import Result.Extra
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import Task exposing (Task)
import Time exposing (Posix, Zone)
import TimeHandling exposing (parseHoursAndMinutes)
import WrongWayAround exposing (identifyBarcodesScannedTheWrongWayAround)


maxNearMatchDistance : Int
maxNearMatchDistance =
    1


type alias Scanned a =
    { a
        | scanTime : String
    }


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


handleEventDateChange : String -> Model -> Model
handleEventDateChange newEventDate model =
    let
        newParsedDate : Maybe Posix
        newParsedDate =
            newEventDate
                ++ " 00:00:00"
                |> dateStringToPosix

        oldEventDateAndTime : EventDateAndTime
        oldEventDateAndTime =
            model.eventDateAndTime

        newEventDateAndTime : EventDateAndTime
        newEventDateAndTime =
            { oldEventDateAndTime
                | enteredDate = newEventDate
                , validatedDate = newParsedDate
            }
    in
    identifyProblemsIn { model | eventDateAndTime = newEventDateAndTime }


handleEventTimeChange : String -> Model -> ( Model, Cmd Msg )
handleEventTimeChange newEventTime model =
    let
        newParsedTime : Maybe Int
        newParsedTime =
            parseHoursAndMinutes newEventTime
                |> Result.toMaybe

        oldEventDateAndTime : EventDateAndTime
        oldEventDateAndTime =
            model.eventDateAndTime

        newEventDateAndTime : EventDateAndTime
        newEventDateAndTime =
            { oldEventDateAndTime
                | enteredTime = newEventTime
                , validatedTime = newParsedTime
            }

        command : Cmd Msg
        command =
            Maybe.map recordEventStartTime newParsedTime
                |> Maybe.withDefault Cmd.none
    in
    ( identifyProblemsIn { model | eventDateAndTime = newEventDateAndTime }, command )


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


createMergedTable : List Int -> List Int -> String -> String -> Stopwatches
createMergedTable times1 times2 filename1 filename2 =
    let
        mergedDetails : List MergeEntry
        mergedDetails =
            merge maxNearMatchDistance times1 times2

        mergedTable : List MergedTableRow
        mergedTable =
            generateInitialTable mergedDetails
    in
    Double
        { times1 = times1
        , times2 = times2
        , filename1 = filename1
        , filename2 = filename2
        , mergedTableRows = mergedTable
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


ensureNonNegative : Int -> Maybe Int
ensureNonNegative intValue =
    if intValue < 0 then
        Nothing

    else
        Just intValue


handleNumberCheckerFieldChange : NumberCheckerFieldChange -> String -> Model -> Model
handleNumberCheckerFieldChange fieldChange newValue model =
    let
        oldNumberCheckerRow : NumberCheckerManualEntryRow
        oldNumberCheckerRow =
            model.numberCheckerManualEntryRow

        newEntry : NumericEntry
        newEntry =
            String.toInt newValue
                |> Maybe.andThen ensureNonNegative
                |> NumericEntry newValue

        newNumberCheckerManualEntryRow : NumberCheckerManualEntryRow
        newNumberCheckerManualEntryRow =
            case fieldChange of
                Stopwatch1 ->
                    { oldNumberCheckerRow | stopwatch1 = newEntry }

                Stopwatch2 ->
                    { oldNumberCheckerRow | stopwatch2 = newEntry }

                FinishTokens ->
                    { oldNumberCheckerRow | finishTokens = newEntry }
    in
    { model | numberCheckerManualEntryRow = newNumberCheckerManualEntryRow }


addNumberCheckerRow : Model -> ( Model, Cmd Msg )
addNumberCheckerRow model =
    let
        manualEntryRow =
            model.numberCheckerManualEntryRow
    in
    case ( manualEntryRow.stopwatch1.parsedValue, manualEntryRow.stopwatch2.parsedValue, manualEntryRow.finishTokens.parsedValue ) of
        ( Just stopwatch1, Just stopwatch2, Just finishTokens ) ->
            ( { model
                | numberCheckerEntries =
                    addAndAnnotate (NumberCheckerEntry stopwatch1 stopwatch2 finishTokens) model.numberCheckerEntries
                , numberCheckerManualEntryRow = emptyNumberCheckerManualEntryRow
              }
            , focus "number-checker-stopwatch-1"
            )

        _ ->
            ( model, Cmd.none )


deleteNumberCheckerEntry : Int -> Model -> Model
deleteNumberCheckerEntry entryNumber model =
    let
        newNumberCheckerEntries : List AnnotatedNumberCheckerEntry
        newNumberCheckerEntries =
            List.filter (\e -> e.entryNumber /= entryNumber) model.numberCheckerEntries
                |> reannotate
    in
    case model.stopwatches of
        Double doubleStopwatchData ->
            let
                newMergedTable : List MergedTableRow
                newMergedTable =
                    underlineTable newNumberCheckerEntries doubleStopwatchData.mergedTableRows
            in
            { model
                | numberCheckerEntries = newNumberCheckerEntries
                , stopwatches = Double { doubleStopwatchData | mergedTableRows = newMergedTable }
            }

        Single _ _ ->
            { model | numberCheckerEntries = newNumberCheckerEntries }

        None ->
            { model | numberCheckerEntries = newNumberCheckerEntries }


removeMultipleOccurrencesOf : String -> List AthleteAndTimePair -> List AthleteAndTimePair
removeMultipleOccurrencesOf athlete list =
    case list of
        [] ->
            []

        first :: rest ->
            if first.athlete == athlete then
                first :: List.filter (\athleteAndTimePair -> athleteAndTimePair.athlete /= athlete) rest

            else
                first :: removeMultipleOccurrencesOf athlete rest


deleteWithinFiles : (BarcodeScannerFileLine -> BarcodeScannerFileLine) -> List BarcodeScannerFile -> List BarcodeScannerFile
deleteWithinFiles deleter files =
    let
        deleteWithinFile : BarcodeScannerFile -> BarcodeScannerFile
        deleteWithinFile file =
            { file | lines = List.map deleter file.lines }
    in
    List.map deleteWithinFile files


deleteUnassociatedAthlete : String -> BarcodeScannerFileLine -> BarcodeScannerFileLine
deleteUnassociatedAthlete athlete line =
    case line.contents of
        Ordinary someAthlete Nothing ->
            if athlete == someAthlete then
                { line | deletionStatus = Deleted (AthleteScannedWithFinishTokenElsewhere athlete) }

            else
                line

        Ordinary _ _ ->
            line

        MisScan _ ->
            line


deleteUnassociatedFinishPosition : Int -> BarcodeScannerFileLine -> BarcodeScannerFileLine
deleteUnassociatedFinishPosition position line =
    case line.contents of
        Ordinary "" somePosition ->
            if somePosition == Just position then
                { line | deletionStatus = Deleted (FinishTokenScannedWithAthleteElsewhere position) }

            else
                line

        Ordinary _ _ ->
            line

        MisScan _ ->
            line


deleteBeforeEventStart : Int -> BarcodeScannerFileLine -> BarcodeScannerFileLine
deleteBeforeEventStart eventStartTimeMillis line =
    case Maybe.map Time.posixToMillis (dateStringToPosix line.scanTime) of
        Just scanTimeMillis ->
            if scanTimeMillis < eventStartTimeMillis then
                { line | deletionStatus = Deleted BeforeEventStart }

            else
                line

        Nothing ->
            line



-- The following functions are used to delete duplicate scans (i.e. two identical
-- scans for the same athlete and same finish token).  The parameter 'found'
-- records whether the first match has been found.


deleteDuplicateScansWithinLine : String -> Int -> BarcodeScannerFileLine -> Bool -> ( BarcodeScannerFileLine, Bool )
deleteDuplicateScansWithinLine athlete position line found =
    case line.contents of
        Ordinary someAthlete somePosition ->
            if someAthlete == athlete && somePosition == Just position then
                if found then
                    ( { line | deletionStatus = Deleted (DuplicateScan athlete position) }, True )

                else
                    -- The first matching record has been found.
                    ( line, True )

            else
                ( line, found )

        MisScan _ ->
            ( line, found )


deleteDuplicateScansWithinFile : String -> Int -> BarcodeScannerFile -> Bool -> ( BarcodeScannerFile, Bool )
deleteDuplicateScansWithinFile athlete position file startingFound =
    let
        transformLines : List BarcodeScannerFileLine -> Bool -> ( List BarcodeScannerFileLine, Bool )
        transformLines lines oldFound =
            case lines of
                [] ->
                    ( [], oldFound )

                firstLine :: remainingLines ->
                    let
                        ( newLine, newFound ) =
                            deleteDuplicateScansWithinLine athlete position firstLine oldFound

                        ( transformedRemainingLines, foundOfRemaining ) =
                            transformLines remainingLines newFound
                    in
                    ( newLine :: transformedRemainingLines, foundOfRemaining )

        ( transformedLines, finalFound ) =
            transformLines file.lines startingFound

        newMaxScanDate : Maybe Posix
        newMaxScanDate =
            transformedLines
                |> List.filterMap (\line -> dateStringToPosix line.scanTime)
                |> List.map Time.posixToMillis
                |> List.maximum
                |> Maybe.map Time.millisToPosix
    in
    ( BarcodeScannerFile file.name transformedLines newMaxScanDate, finalFound )


deleteDuplicateScansWithinFilesInternal : String -> Int -> List BarcodeScannerFile -> Bool -> ( List BarcodeScannerFile, Bool )
deleteDuplicateScansWithinFilesInternal athlete position files startingFound =
    case files of
        [] ->
            ( [], startingFound )

        firstFile :: remainingFiles ->
            let
                ( newFile, newFound ) =
                    deleteDuplicateScansWithinFile athlete position firstFile startingFound

                ( transformedRemainingFiles, finalFound ) =
                    deleteDuplicateScansWithinFilesInternal athlete position remainingFiles newFound
            in
            ( newFile :: transformedRemainingFiles, finalFound )


deleteDuplicateScansWithinFiles : String -> Int -> List BarcodeScannerFile -> List BarcodeScannerFile
deleteDuplicateScansWithinFiles athlete position files =
    deleteDuplicateScansWithinFilesInternal athlete position files False
        |> Tuple.first


regenerateWithWrongWayArounds : BarcodeScannerData -> BarcodeScannerData
regenerateWithWrongWayArounds barcodeScannerData =
    identifyBarcodesScannedTheWrongWayAround barcodeScannerData
        |> regenerate


fixProblem : ProblemFix -> Model -> Model
fixProblem problemFix model =
    let
        oldBarcodeScannerData : BarcodeScannerData
        oldBarcodeScannerData =
            model.barcodeScannerData

        newBarcodeScannerData : BarcodeScannerData
        newBarcodeScannerData =
            case problemFix of
                RemoveUnassociatedFinishToken position ->
                    regenerateWithWrongWayArounds
                        { oldBarcodeScannerData
                            | files = deleteWithinFiles (deleteUnassociatedFinishPosition position) oldBarcodeScannerData.files
                        }

                RemoveUnassociatedAthlete athlete ->
                    regenerateWithWrongWayArounds
                        { oldBarcodeScannerData
                            | files = deleteWithinFiles (deleteUnassociatedAthlete athlete) oldBarcodeScannerData.files
                        }

                RemoveDuplicateScans position athlete ->
                    regenerateWithWrongWayArounds
                        { oldBarcodeScannerData
                            | files = deleteDuplicateScansWithinFiles athlete position oldBarcodeScannerData.files
                        }

                RemoveScansBeforeEventStart eventStartTimeMillis ->
                    regenerateWithWrongWayArounds
                        { oldBarcodeScannerData
                            | files = deleteWithinFiles (deleteBeforeEventStart eventStartTimeMillis) oldBarcodeScannerData.files
                        }

                AdjustStopwatch whichStopwatch offset ->
                    -- This problem-fix applies no change to the barcode-scanner data.
                    oldBarcodeScannerData

        oldStopwatches : Stopwatches
        oldStopwatches =
            model.stopwatches

        newStopwatches : Stopwatches
        newStopwatches =
            case ( oldStopwatches, problemFix ) of
                ( Double doubleStopwatchData, AdjustStopwatch whichStopwatch offset ) ->
                    let
                        adjustedStopwatches : DoubleStopwatchData
                        adjustedStopwatches =
                            case whichStopwatch of
                                StopwatchOne ->
                                    { doubleStopwatchData | times1 = List.map (\time -> time + offset) doubleStopwatchData.times1 }

                                StopwatchTwo ->
                                    { doubleStopwatchData | times2 = List.map (\time -> time + offset) doubleStopwatchData.times2 }

                        mergedStopwatches =
                            createMergedTable adjustedStopwatches.times1 adjustedStopwatches.times2 adjustedStopwatches.filename1 adjustedStopwatches.filename2
                    in
                    underlineStopwatches mergedStopwatches model.numberCheckerEntries

                _ ->
                    -- Not two stopwatches or some other problem-fix.
                    oldStopwatches
    in
    { model | barcodeScannerData = newBarcodeScannerData, stopwatches = newStopwatches }
        |> identifyProblemsIn


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
            let
                numberCheckerEntryToEdit : Maybe AnnotatedNumberCheckerEntry
                numberCheckerEntryToEdit =
                    List.filter (\e -> e.entryNumber == entryNumber) model.numberCheckerEntries
                        |> List.head

                makeNumericEntry : Int -> NumericEntry
                makeNumericEntry value =
                    NumericEntry (String.fromInt value) (Just value)
            in
            case numberCheckerEntryToEdit of
                Just entry ->
                    let
                        modelWithEntryDeleted : Model
                        modelWithEntryDeleted =
                            deleteNumberCheckerEntry entryNumber model
                    in
                    ( { modelWithEntryDeleted
                        | numberCheckerManualEntryRow =
                            NumberCheckerManualEntryRow (makeNumericEntry entry.stopwatch1) (makeNumericEntry entry.stopwatch2) (makeNumericEntry entry.finishTokens)
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteNumberCheckerRow entryNumber ->
            ( deleteNumberCheckerEntry entryNumber model, Cmd.none )

        EventDateChanged newEventDate ->
            ( handleEventDateChange newEventDate model, Cmd.none )

        EventTimeChanged newEventTime ->
            handleEventTimeChange newEventTime model

        NumberCheckerFieldChanged fieldChange newValue ->
            ( handleNumberCheckerFieldChange fieldChange newValue model, Cmd.none )

        AddNumberCheckerRow ->
            addNumberCheckerRow model

        FixProblem problemFix ->
            ( fixProblem problemFix model, Cmd.none )

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
