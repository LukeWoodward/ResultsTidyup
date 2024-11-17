module FileDropHandling exposing (handleFilesAdded)

import BarcodeScanner exposing (BarcodeScannerData, mergeScannerData, readBarcodeScannerData)
import Error exposing (Error, FileError, mapError)
import FileHandling exposing (AddedFile)
import Model exposing (Model)
import NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, annotate, parseNumberCheckerFile)
import Parser exposing ((|.), Parser, chompIf, chompWhile, end, run)
import Regex exposing (Regex)
import Result.Extra
import Timer exposing (Timer(..), TimerFile, Timers(..), createMergedTable, readTimerData)


hasFileAlreadyBeenUploaded : String -> Timers -> Bool
hasFileAlreadyBeenUploaded newFileName timers =
    case timers of
        None ->
            False

        Single existingFile _ ->
            newFileName == existingFile.filename

        Double doubleTimerData ->
            newFileName == doubleTimerData.file1.filename || newFileName == doubleTimerData.file2.filename


handleTimerFileAdded : AddedFile -> Model -> Model
handleTimerFileAdded addedFile model =
    let
        makeTimerFile : String -> TimerFile
        makeTimerFile filename =
            TimerFile filename addedFile.name
    in
    case readTimerData addedFile.fileText of
        Ok (TimerData newTimer) ->
            if hasFileAlreadyBeenUploaded addedFile.fileName model.timers then
                { model
                    | lastErrors =
                        model.lastErrors
                            ++ [ FileError
                                    "TIMER_FILE_ALREADY_LOADED"
                                    "That timer data file has already been loaded"
                                    addedFile.fileName
                               ]
                }

            else
                let
                    newTimers =
                        case model.timers of
                            None ->
                                Single (makeTimerFile addedFile.fileName) newTimer

                            Single existingFile firstTimer ->
                                if addedFile.fileName < existingFile.filename then
                                    Double (createMergedTable newTimer firstTimer (makeTimerFile addedFile.fileName) existingFile)

                                else
                                    Double (createMergedTable firstTimer newTimer existingFile (makeTimerFile addedFile.fileName))

                            Double _ ->
                                model.timers
                in
                { model | timers = newTimers }

        Err error ->
            { model | lastErrors = model.lastErrors ++ [ mapError addedFile.fileName error ] }


barcodeScannerRegex : Regex
barcodeScannerRegex =
    Regex.fromString "A[0-9]+,P[0-9]+,"
        |> Maybe.withDefault Regex.never


isPossibleBarcodeScannerFile : String -> Bool
isPossibleBarcodeScannerFile fileText =
    Regex.contains barcodeScannerRegex fileText


handleBarcodeScannerFileAdded : AddedFile -> Model -> Model
handleBarcodeScannerFileAdded addedFile model =
    if List.any (\file -> file.filename == addedFile.fileName) model.barcodeScannerData.files then
        { model
            | lastErrors =
                model.lastErrors
                    ++ [ FileError
                            "BARCODE_DATA_ALREADY_LOADED"
                            "That barcode scanner file has already been loaded"
                            addedFile.fileName
                       ]
        }

    else
        let
            result : Result Error BarcodeScannerData
            result =
                readBarcodeScannerData addedFile
        in
        case result of
            Ok scannerData ->
                { model
                    | barcodeScannerData = mergeScannerData model.barcodeScannerData scannerData
                }

            Err error ->
                { model | lastErrors = model.lastErrors ++ [ mapError addedFile.fileName error ] }


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


handleNumberCheckerFileAdded : String -> String -> Model -> Model
handleNumberCheckerFileAdded fileName fileText model =
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


handleFileAdded : AddedFile -> Model -> Model
handleFileAdded addedFile model =
    if String.startsWith "STARTOFEVENT" addedFile.fileText || String.startsWith "I, CP" addedFile.fileText then
        handleTimerFileAdded addedFile model

    else if isPossibleNumberCheckerFile addedFile.fileText then
        handleNumberCheckerFileAdded addedFile.fileName addedFile.fileText model

    else if isPossibleBarcodeScannerFile addedFile.fileText then
        handleBarcodeScannerFileAdded addedFile model

    else
        { model
            | lastErrors = model.lastErrors ++ [ FileError "UNRECOGNISED_FILE" "File was unrecognised" addedFile.fileName ]
        }


handleFilesAdded : List AddedFile -> Model -> Model
handleFilesAdded files model =
    let
        sortedFiles : List AddedFile
        sortedFiles =
            List.sortBy .fileName files
                |> List.reverse
    in
    List.foldr handleFileAdded model sortedFiles
