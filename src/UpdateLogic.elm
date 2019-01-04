module UpdateLogic exposing (createFileForDownload, update)

import BarcodeScanner exposing (BarcodeScannerData, mergeScannerData, readBarcodeScannerData)
import Browser.Dom
import DataStructures exposing (EventDateAndTime, InteropFile, WhichStopwatch(..))
import DateHandling exposing (dateStringToPosix, dateToString, generateDownloadFilenameDatePart)
import Error exposing (Error)
import File.Download as Download
import MergedTable
    exposing
        ( MergedTableRow
        , Stopwatches(..)
        , deleteStopwatchFromTable
        , flipTable
        , generateInitialTable
        , outputMergedTable
        , toggleRowInTable
        , underlineTable
        )
import Merger exposing (MergeEntry, merge)
import Model exposing (Model, NumberCheckerManualEntryRow, NumericEntry, emptyNumberCheckerManualEntryRow)
import Msg exposing (Msg(..), NumberCheckerFieldChange(..))
import NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, addAndAnnotate, annotate, parseNumberCheckerFile, reannotate)
import Parser exposing ((|.), Parser, chompIf, chompWhile, end, int, run, symbol)
import Parsers exposing (digits)
import Ports exposing (recordEventStartTime)
import Problems exposing (identifyProblems)
import Result.Extra
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import Task exposing (Task)
import Time exposing (Posix, Zone)
import TimeHandling exposing (parseHoursAndMinutes)


maxNearMatchDistance : Int
maxNearMatchDistance =
    1


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


identifyProblemsIn : Model -> Model
identifyProblemsIn model =
    { model
        | problems = identifyProblems model.stopwatches model.barcodeScannerData model.eventDateAndTime
    }


handleStopwatchFileDrop : String -> String -> Model -> Model
handleStopwatchFileDrop fileName fileText model =
    case readStopwatchData fileText of
        Ok (StopwatchData newStopwatch) ->
            if hasFileAlreadyBeenUploaded fileName model.stopwatches then
                { model
                    | lastError =
                        Just
                            (Error
                                "STOPWATCH_FILE_ALREADY_LOADED"
                                ("Stopwatch data file '"
                                    ++ fileName
                                    ++ "' has already been loaded"
                                )
                            )
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
            { model | lastError = Just error }


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
                | lastError = Just error
            }


barcodeScannerParser : Parser ()
barcodeScannerParser =
    symbol "A"
        |. int
        |. symbol ","
        |. symbol "P"
        |. digits 4


isPossibleBarcodeScannerFile : String -> Bool
isPossibleBarcodeScannerFile fileText =
    Result.Extra.isOk (run barcodeScannerParser fileText)


handleBarcodeScannerFileDrop : String -> String -> Model -> Model
handleBarcodeScannerFileDrop fileName fileText model =
    if List.member fileName model.barcodeScannerFiles then
        { model
            | lastError =
                Just
                    (Error
                        "BARCODE_DATA_ALREADY_LOADED"
                        "That barcode scanner file has already been loaded"
                    )
        }

    else
        let
            result : Result Error BarcodeScannerData
            result =
                readBarcodeScannerData fileText
        in
        case result of
            Ok scannerData ->
                { model
                    | barcodeScannerFiles = fileName :: model.barcodeScannerFiles
                    , barcodeScannerData = mergeScannerData model.barcodeScannerData scannerData
                    , lastError = Nothing
                }
                    |> setEventDateAndTimeIn
                    |> identifyProblemsIn

            Err error ->
                { model | lastError = Just error }


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


createFileForDownload : Zone -> Posix -> List MergedTableRow -> InteropFile
createFileForDownload zone time mergedTableRows =
    let
        fileContents : String
        fileContents =
            outputMergedTable mergedTableRows

        fileName : String
        fileName =
            "parkrun_timer_" ++ generateDownloadFilenameDatePart zone time ++ ".txt"
    in
    InteropFile fileName fileContents


downloadMergedTimesToFile : InteropFile -> Cmd Msg
downloadMergedTimesToFile interopFile =
    Download.string interopFile.fileName "text/csv" interopFile.fileText


downloadMergedStopwatchData : Zone -> Posix -> Model -> ( Model, Cmd Msg )
downloadMergedStopwatchData zone time model =
    case model.stopwatches of
        None ->
            ( model, Cmd.none )

        Single _ _ ->
            ( model, Cmd.none )

        Double _ _ mergedTableRows ->
            ( model
            , createFileForDownload zone time mergedTableRows
                |> downloadMergedTimesToFile
            )


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
            | lastError = Just (Error "UNRECOGNISED_FILE" "Unrecognised file dropped")
        }


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
        Double filename1 filename2 oldMergedTable ->
            let
                newMergedTable : List MergedTableRow
                newMergedTable =
                    underlineTable newNumberCheckerEntries oldMergedTable
            in
            { model
                | numberCheckerEntries = newNumberCheckerEntries
                , stopwatches = Double filename1 filename2 newMergedTable
            }

        Single _ _ ->
            { model | numberCheckerEntries = newNumberCheckerEntries }

        None ->
            { model | numberCheckerEntries = newNumberCheckerEntries }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
