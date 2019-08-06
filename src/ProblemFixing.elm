module ProblemFixing exposing (ProblemFix(..), fixProblem)

import BarcodeScanner
    exposing
        ( AthleteAndTimePair
        , BarcodeScannerData
        , BarcodeScannerFile
        , BarcodeScannerFileLine
        , DeletionReason(..)
        , DeletionStatus(..)
        , LineContents(..)
        , PositionAndTimePair
        , regenerate
        )
import DateHandling exposing (dateStringToPosix)
import Model exposing (Model)
import Problems exposing (FixableProblem(..), NonFixableProblem(..))
import Stopwatch exposing (DoubleStopwatchData, Stopwatches(..), WhichStopwatch(..), createMergedTable)
import Time exposing (Posix)


type ProblemFix
    = RemoveUnassociatedFinishToken Int
    | RemoveUnassociatedAthlete String
    | RemoveDuplicateScans Int String
    | RemoveScansBeforeEventStart Int
    | AdjustStopwatch WhichStopwatch Int
    | SwapBarcodes String Int Int


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

        BarcodeScanner.MisScan _ ->
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


swapBarcodesAroundInLines : Int -> Int -> List BarcodeScannerFileLine -> List BarcodeScannerFileLine
swapBarcodesAroundInLines first last lines =
    case lines of
        [] ->
            []

        singleLine :: [] ->
            if singleLine.lineNumber == last then
                [ { singleLine | deletionStatus = Deleted EndOfWrongWayAroundSection } ]

            else
                [ singleLine ]

        firstLine :: secondLine :: remainingLines ->
            if firstLine.lineNumber == last then
                { firstLine | deletionStatus = Deleted EndOfWrongWayAroundSection } :: secondLine :: remainingLines

            else if first <= firstLine.lineNumber && firstLine.lineNumber < last then
                case ( firstLine.contents, secondLine.contents ) of
                    ( Ordinary _ thisPosition, Ordinary nextAthlete _ ) ->
                        { firstLine | contents = Ordinary nextAthlete thisPosition }
                            :: swapBarcodesAroundInLines first last (secondLine :: remainingLines)

                    _ ->
                        -- Mis-scans?  Unexpected.
                        firstLine :: swapBarcodesAroundInLines first last (secondLine :: remainingLines)

            else
                firstLine :: swapBarcodesAroundInLines first last (secondLine :: remainingLines)


swapBarcodesAroundInFile : Int -> Int -> BarcodeScannerFile -> BarcodeScannerFile
swapBarcodesAroundInFile first last file =
    { file | lines = swapBarcodesAroundInLines first last file.lines }


swapBarcodesAround : String -> Int -> Int -> List BarcodeScannerFile -> List BarcodeScannerFile
swapBarcodesAround fileName first last files =
    let
        matchingFileMaybe : Maybe BarcodeScannerFile
        matchingFileMaybe =
            List.filter (\f -> f.name == fileName) files
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
            in
            List.map swapFileBackIn files

        Nothing ->
            files


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
                    regenerate
                        { oldBarcodeScannerData
                            | files = deleteWithinFiles (deleteUnassociatedFinishPosition position) oldBarcodeScannerData.files
                        }

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

                RemoveScansBeforeEventStart eventStartTimeMillis ->
                    regenerate
                        { oldBarcodeScannerData
                            | files = deleteWithinFiles (deleteBeforeEventStart eventStartTimeMillis) oldBarcodeScannerData.files
                        }

                SwapBarcodes fileName first last ->
                    regenerate
                        { oldBarcodeScannerData
                            | files = swapBarcodesAround fileName first last oldBarcodeScannerData.files
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
                    in
                    createMergedTable adjustedStopwatches.times1 adjustedStopwatches.times2 adjustedStopwatches.filename1 adjustedStopwatches.filename2

                _ ->
                    -- Not two stopwatches or some other problem-fix.
                    oldStopwatches
    in
    { model | barcodeScannerData = newBarcodeScannerData, stopwatches = newStopwatches }
