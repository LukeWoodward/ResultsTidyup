module ProblemFixing exposing (ProblemFix(..), ProblemIgnorance(..), fixProblem, ignoreProblem)

import BarcodeScanner
    exposing
        ( BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        , regenerate
        )
import DateHandling exposing (dateTimeStringToPosix)
import Model exposing (Model)
import Problems exposing (IgnoredProblems)
import Time exposing (Posix)
import Timer exposing (DoubleTimerData, Timers(..), WhichTimer(..), createMergedTable)


type ProblemFix
    = RemoveUnassociatedAthlete String
    | RemoveDuplicateScans Int String
    | RemoveScansBeforeEventStart Int
    | AdjustTimer WhichTimer Int


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

        BarcodeScanner.MisScan _ ->
            line


deleteBeforeEventStart : Int -> BarcodeScannerFileLine -> BarcodeScannerFileLine
deleteBeforeEventStart eventStartDateTimeMillis line =
    case Maybe.map Time.posixToMillis (dateTimeStringToPosix line.scanDateTime) of
        Just scanDateTimeMillis ->
            if scanDateTimeMillis < eventStartDateTimeMillis then
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

        BarcodeScanner.MisScan _ ->
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

        newMaxScanDateTime : Maybe Posix
        newMaxScanDateTime =
            transformedLines
                |> List.filterMap (\line -> dateTimeStringToPosix line.scanDateTime)
                |> List.map Time.posixToMillis
                |> List.maximum
                |> Maybe.map Time.millisToPosix
    in
    ( BarcodeScannerFile file.name transformedLines newMaxScanDateTime, finalFound )


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


fixProblem : ProblemFix -> Model -> Model
fixProblem problemFix model =
    let
        oldBarcodeScannerData : BarcodeScannerData
        oldBarcodeScannerData =
            model.barcodeScannerData

        newBarcodeScannerData : BarcodeScannerData
        newBarcodeScannerData =
            case problemFix of
                RemoveUnassociatedAthlete athlete ->
                    regenerate
                        { oldBarcodeScannerData
                            | files = deleteWithinFiles (deleteUnassociatedAthlete athlete) oldBarcodeScannerData.files
                        }

                RemoveDuplicateScans position athlete ->
                    regenerate
                        { oldBarcodeScannerData
                            | files = deleteDuplicateScansWithinFiles athlete position oldBarcodeScannerData.files
                        }

                RemoveScansBeforeEventStart eventStartDateTimeMillis ->
                    regenerate
                        { oldBarcodeScannerData
                            | files = deleteWithinFiles (deleteBeforeEventStart eventStartDateTimeMillis) oldBarcodeScannerData.files
                        }

                AdjustTimer _ _ ->
                    -- This problem-fix applies no change to the barcode-scanner data.
                    oldBarcodeScannerData

        oldTimers : Timers
        oldTimers =
            model.timers

        newTimers : Timers
        newTimers =
            case ( oldTimers, problemFix ) of
                ( Double doubleTimerData, AdjustTimer whichTimer offset ) ->
                    let
                        adjustedTimers : DoubleTimerData
                        adjustedTimers =
                            case whichTimer of
                                TimerOne ->
                                    { doubleTimerData | times1 = List.map (\time -> time + offset) doubleTimerData.times1 }

                                TimerTwo ->
                                    { doubleTimerData | times2 = List.map (\time -> time + offset) doubleTimerData.times2 }
                    in
                    Double (createMergedTable adjustedTimers.times1 adjustedTimers.times2 adjustedTimers.filename1 adjustedTimers.filename2)

                _ ->
                    -- Not two timers or some other problem-fix.
                    oldTimers
    in
    { model | barcodeScannerData = newBarcodeScannerData, timers = newTimers }


type ProblemIgnorance
    = IgnoreTimerTimeOffsets


ignoreProblem : ProblemIgnorance -> IgnoredProblems -> IgnoredProblems
ignoreProblem ignorance ignoredProblems =
    case ignorance of
        IgnoreTimerTimeOffsets ->
            { ignoredProblems | ignoreTimerTimeOffsets = True }
