module FileDropHandling exposing (handleFilesDropped)

import BarcodeScanner exposing (BarcodeScannerData, mergeScannerData, readBarcodeScannerData)
import DateHandling exposing (posixToDateString)
import Error exposing (Error, FileError, mapError)
import EventDateAndTimeEditing exposing (handleEventDateChange)
import FileHandling exposing (InteropFile)
import Model exposing (Model)
import NumberChecker exposing (AnnotatedNumberCheckerEntry, NumberCheckerEntry, annotate, parseNumberCheckerFile)
import Parser exposing ((|.), Parser, chompIf, chompWhile, end, run)
import Regex exposing (Regex)
import Result.Extra
import Timer exposing (Timer(..), Timers(..), createMergedTable, readTimerData)


hasFileAlreadyBeenUploaded : String -> Timers -> Bool
hasFileAlreadyBeenUploaded newFileName timers =
    case timers of
        None ->
            False

        Single existingFilename _ ->
            newFileName == existingFilename

        Double doubleTimerData ->
            newFileName == doubleTimerData.filename1 || newFileName == doubleTimerData.filename2


handleTimerFileDrop : String -> String -> Model -> Model
handleTimerFileDrop fileName fileText model =
    case readTimerData fileText of
        Ok (TimerData newTimer) ->
            if hasFileAlreadyBeenUploaded fileName model.timers then
                { model
                    | lastErrors =
                        model.lastErrors
                            ++ [ FileError
                                    "TIMER_FILE_ALREADY_LOADED"
                                    "That timer data file has already been loaded"
                                    fileName
                               ]
                }

            else
                let
                    newTimers =
                        case model.timers of
                            None ->
                                Single fileName newTimer

                            Single existingFilename firstTimer ->
                                if fileName < existingFilename then
                                    Double (createMergedTable newTimer firstTimer fileName existingFilename)

                                else
                                    Double (createMergedTable firstTimer newTimer existingFilename fileName)

                            Double _ ->
                                model.timers
                in
                { model | timers = newTimers }

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
    if String.startsWith "STARTOFEVENT" fileText || String.startsWith "I, CP" fileText then
        handleTimerFileDrop fileName fileText model

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
