module WrongWayAround exposing (identifyBarcodesScannedTheWrongWayAround)

import BarcodeScanner exposing (BarcodeScannerData, BarcodeScannerFile, BarcodeScannerFileLine, LineContents(..), WrongWayAroundStatus(..))


{-| We've found the start of a possible wrong-way-around section. See if we can
find the end.
-}
findEndOfWrongWayAroundSection : Int -> Int -> List BarcodeScannerFileLine -> WrongWayAroundStatus
findEndOfWrongWayAroundSection startLineNumber prevFinishToken lines =
    case lines of
        [] ->
            -- Hit the end: nothing wrong-way-around.
            NotWrongWayAround

        first :: rest ->
            case first.contents of
                MisScan text ->
                    -- Hit a mis-scan: abandon the search.
                    NotWrongWayAround

                Ordinary "" _ ->
                    -- Another position-only record.  Abandon the search.
                    NotWrongWayAround

                Ordinary athlete Nothing ->
                    -- Athlete-only record.  Stop things here.
                    FirstWrongWayAround startLineNumber first.lineNumber

                Ordinary athlete (Just thisFinishToken) ->
                    if thisFinishToken == prevFinishToken then
                        if startLineNumber + 1 == first.lineNumber then
                            -- We have a complete record immediately following a finish-token-only
                            -- record with the same token.  Ignore the former record: there's no
                            -- sequence of reversed scans.
                            NotWrongWayAround

                        else
                            -- End things here on the previous row: this is an intact record, and
                            -- the finish token in the previous record will be an error.
                            FirstWrongWayAround startLineNumber (first.lineNumber - 1)

                    else
                        findEndOfWrongWayAroundSection startLineNumber thisFinishToken rest


markSubsequentLines : Int -> List BarcodeScannerFileLine -> ( List BarcodeScannerFileLine, List BarcodeScannerFileLine )
markSubsequentLines endLineNumber lines =
    case lines of
        [] ->
            ( [], [] )

        first :: rest ->
            if first.lineNumber <= endLineNumber then
                let
                    ( subsubsequentLines, afterLines ) =
                        markSubsequentLines endLineNumber rest

                    newFirstLine : BarcodeScannerFileLine
                    newFirstLine =
                        { first | wrongWayAroundStatus = SubsequentWrongWayAround }
                in
                ( newFirstLine :: subsubsequentLines, afterLines )

            else
                ( [], lines )


identifyWrongWayArounds : List BarcodeScannerFileLine -> List BarcodeScannerFileLine
identifyWrongWayArounds lines =
    case lines of
        [] ->
            []

        first :: rest ->
            case first.contents of
                Ordinary "" (Just finishToken) ->
                    -- Finish token with no athlete: here's the start of a possible run of
                    -- reversed scans.
                    let
                        wrongWayAroundStatus : WrongWayAroundStatus
                        wrongWayAroundStatus =
                            findEndOfWrongWayAroundSection first.lineNumber finishToken rest

                        newFirstLine : BarcodeScannerFileLine
                        newFirstLine =
                            { first | wrongWayAroundStatus = wrongWayAroundStatus }
                    in
                    case wrongWayAroundStatus of
                        FirstWrongWayAround startLineNumber endLineNumber ->
                            let
                                ( subsequentLines, remainingLines ) =
                                    markSubsequentLines endLineNumber rest
                            in
                            (newFirstLine :: subsequentLines) ++ identifyWrongWayArounds remainingLines

                        _ ->
                            -- No interesting status was found.
                            newFirstLine :: identifyWrongWayArounds rest

                _ ->
                    -- Anything else: scan remaining lines.
                    first :: identifyWrongWayArounds rest


identifyBarcodesScannedTheWrongWayAroundInFile : BarcodeScannerFile -> BarcodeScannerFile
identifyBarcodesScannedTheWrongWayAroundInFile file =
    { file | lines = identifyWrongWayArounds file.lines }


identifyBarcodesScannedTheWrongWayAround : BarcodeScannerData -> BarcodeScannerData
identifyBarcodesScannedTheWrongWayAround barcodeScannerData =
    { barcodeScannerData
        | files = List.map identifyBarcodesScannedTheWrongWayAroundInFile barcodeScannerData.files
    }
