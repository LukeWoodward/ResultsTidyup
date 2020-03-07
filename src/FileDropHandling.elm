module FileDropHandling exposing (handleFilesDropped)

import BarcodeScanner exposing (BarcodeScannerData, mergeScannerData, readBarcodeScannerData)
import DateHandling exposing (posixToDateString)
import Error exposing (Error, FileError, mapError)
import EventDateAndTimeEditing exposing (handleEventDateChange)
import FileHandling exposing (InteropFile)
import Model exposing (Model)
import NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, annotate, parseNumberCheckerFile)
import Parser exposing ((|.), Parser, chompIf, chompWhile, end, int, run, symbol)
import Regex exposing (Regex)
import Result.Extra
import Stopwatch exposing (Stopwatch(..), Stopwatches(..), createMergedTable, readStopwatchData)


hasFileAlreadyBeenUploaded : String -> Stopwatches -> Bool
hasFileAlreadyBeenUploaded newFileName stopwatches =
    case stopwatches of
        None ->
            False

        Single existingFilename _ ->
            newFileName == existingFilename

        Double doubleStopwatchData ->
            newFileName == doubleStopwatchData.filename1 || newFileName == doubleStopwatchData.filename2


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
                                if fileName < existingFilename then
                                    Double (createMergedTable newStopwatch firstStopwatch fileName existingFilename)

                                else
                                    Double (createMergedTable firstStopwatch newStopwatch existingFilename fileName)

                            Double _ ->
                                model.stopwatches
                in
                { model | stopwatches = newStopwatches }

        Err error ->
            { model | lastErrors = model.lastErrors ++ [ mapError fileName error ] }


barcodeScannerRegex : Regex
barcodeScannerRegex =
    Regex.fromString "A[0-9]+,P[0-9]+,"
        |> Maybe.withDefault Regex.never


isPossibleBarcodeScannerFile : String -> Bool
isPossibleBarcodeScannerFile fileText =
    Regex.contains barcodeScannerRegex fileText


setEventDateAndTimeIn : Model -> Model
setEventDateAndTimeIn model =
    let
        newEventDate : Maybe String
        newEventDate =
            model.barcodeScannerData.lastScanDateTime
                |> Maybe.map posixToDateString
    in
    case ( newEventDate, model.eventDateAndTime.date.parsedValue ) of
        ( Just _, Just _ ) ->
            -- Already have an event date so leave it.
            model

        ( Just newDateString, Nothing ) ->
            -- We've got an event date now and we didn't before.
            handleEventDateChange newDateString model

        ( Nothing, _ ) ->
            -- No event date read so leave things as they were.
            model


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
                    | barcodeScannerData = mergeScannerData model.barcodeScannerData scannerData
                }
                    |> setEventDateAndTimeIn

            Err error ->
                { model | lastErrors = model.lastErrors ++ [ mapError fileName error ] }


isNumberCheckerCharacter : Char -> Bool
isNumberCheckerCharacter c =
    Char.isDigit c || c == '\u{000D}' || c == '\n' || c == ','


numberCheckerParser : Parser ()
numberCheckerParser =
    chompIf isNumberCheckerCharacter
        |. chompWhile isNumberCheckerCharacter
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
            { model | numberCheckerEntries = annotatedEntries }

        Err error ->
            { model
                | lastErrors = model.lastErrors ++ [ mapError fileName error ]
            }


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
    in
    List.foldr handleFileDropped model sortedFiles
