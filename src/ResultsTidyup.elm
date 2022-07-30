module ResultsTidyup exposing (main)

import BarcodeScanner
import BarcodeScannerEditModal
import BarcodeScannerView exposing (barcodeScannersView)
import Bootstrap.Alert as Alert
import Bootstrap.Badge as Badge
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Browser
import Browser.Dom
import Commands exposing (Command(..), ElementToFocus(..))
import DataEntry exposing (IntegerEntry, emptyEntry)
import Error exposing (FileError)
import EventDateAndTime exposing (EventDateAndTime)
import EventDateAndTimeView exposing (eventDateAndTimeView)
import File exposing (File)
import File.Download as Download
import File.Select
import FileHandling exposing (InteropFile)
import Html exposing (Html, a, div, h1, span, text)
import Html.Attributes exposing (class, href, id, target)
import Html.Events exposing (on, onClick)
import Json.Decode exposing (Decoder, andThen, fail, field, int, succeed)
import Modals exposing (showModalDialog)
import Model exposing (Model, initModel)
import Msg exposing (Msg(..))
import NumberCheckerView
import PasteFileModal
import Ports exposing (filesDropped, recordEventStartTime)
import Task exposing (Task)
import Time
import TimeHandling exposing (formatHoursAndMinutes)
import Timer exposing (Timers(..))
import TimersView exposing (timersView)
import UpdateLogic exposing (update)
import ViewCommon exposing (normalButton)


type alias FlagsRecord =
    { startTime : Maybe Int
    , isBeta : Bool
    }


aboutUrl : String
aboutUrl =
    "https://github.com/LukeWoodward/ResultsTidyup#results-tidyup"


main : Program FlagsRecord Model Msg
main =
    Browser.element
        { init = init
        , update = updateAndMapCommand
        , view = view
        , subscriptions = subscriptions
        }


init : FlagsRecord -> ( Model, Cmd Msg )
init { startTime, isBeta } =
    let
        startTimeAsString : String
        startTimeAsString =
            Maybe.map formatHoursAndMinutes startTime
                |> Maybe.withDefault ""

        initialEventDateAndTime : EventDateAndTime
        initialEventDateAndTime =
            EventDateAndTime emptyEntry (IntegerEntry startTimeAsString startTime)
    in
    ( { initModel
        | isBeta = isBeta
        , eventDateAndTime = initialEventDateAndTime
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ filesDropped FilesDropped ]


errorView : FileError -> Html a
errorView error =
    div [] [ text ("Error with file '" ++ error.fileName ++ "': " ++ error.message) ]


errorsView : List FileError -> Html Msg
errorsView errors =
    if List.isEmpty errors then
        text ""

    else
        Alert.simpleDanger
            []
            (span [ class "close", onClick ClearErrors ] [ text "×" ]
                :: List.map errorView errors
            )


focus : ElementToFocus -> Cmd Msg
focus elementToFocus =
    let
        elementId : String
        elementId =
            case elementToFocus of
                NumberCheckerManualEntryRowFirstCell ->
                    NumberCheckerView.firstManualEntryCellId

                BarcodeScannerEditingAthleteInput ->
                    BarcodeScannerEditModal.athleteInputId

                BarcodeScannerEditingAthleteRadioButton ->
                    BarcodeScannerEditModal.athleteRadioButtonId

                PasteFileDialogTextArea ->
                    PasteFileModal.pasteFileDialogTextAreaId
    in
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus elementId)


getDownloadOperation : Commands.CurrentDateAndTimeOperation -> (Time.Zone -> Time.Posix -> Msg)
getDownloadOperation downloadOperation =
    case downloadOperation of
        Commands.DownloadSingleTimer which ->
            DownloadTimer which

        Commands.DownloadMergedTimers ->
            DownloadMergedTimerData

        Commands.DownloadAllBarcodeScannerData ->
            DownloadAllBarcodeScannerData

        Commands.DownloadBarcodeScannerFile filename ->
            DownloadBarcodeScannerFile filename

        Commands.UploadPastedFile contents ->
            PastedFileUploaded contents


mapCommand : Command -> Cmd Msg
mapCommand command =
    case command of
        NoCommand ->
            Cmd.none

        GetCurrentDateAndTime operation ->
            Task.perform identity (Task.map2 (getDownloadOperation operation) Time.here Time.now)

        DownloadFile mimeType interopFile ->
            Download.string interopFile.fileName mimeType interopFile.fileText

        FocusElement elementToFocus ->
            focus elementToFocus

        SaveEventStartTime startTime ->
            recordEventStartTime startTime

        SelectFileForUpload ->
            File.Select.files [ ".txt", ".csv" ] FilesUploaded

        ReadFiles files ->
            let
                -- Convert each File into a Task that returns the corresponding
                -- InteropFile when performed.
                mapTask : File -> Task Never InteropFile
                mapTask file =
                    Task.map (InteropFile (File.name file)) (File.toString file)
            in
            List.map mapTask files
                |> Task.sequence
                |> Task.perform FilesDropped


handleKey : Int -> Decoder Msg
handleKey keyCode =
    if keyCode == 27 then
        succeed CloseModal

    else if keyCode == 13 then
        succeed ReturnKeyPressed

    else
        fail "unrecognised key"


keyDecoder : Decoder Msg
keyDecoder =
    field "keyCode" int
        |> andThen handleKey


updateAndMapCommand : Msg -> Model -> ( Model, Cmd Msg )
updateAndMapCommand msg model =
    update msg model
        |> Tuple.mapSecond mapCommand


actionsPanelView : Model -> Html Msg
actionsPanelView _ =
    div
        [ id "actionsPanelContainer" ]
        [ normalButton OpenUploadFileDialog [] "Upload files..."
        , normalButton OpenPasteFileDialog [] "Paste..."
        , normalButton ClearAllData [] "Clear everything"
        ]


view : Model -> Html Msg
view model =
    let
        badge : Html Msg
        badge =
            if model.isBeta then
                Badge.badgePrimary [] [ text "BETA" ]

            else
                text ""

        timersItem : Html Msg
        timersItem =
            if model.timers == None && BarcodeScanner.isEmpty model.barcodeScannerData then
                div [] []

            else
                timersView model.timers model.barcodeScannerData model.problems model.highlightedNumberCheckerId

        scannersItem : Html Msg
        scannersItem =
            if BarcodeScanner.isEmpty model.barcodeScannerData then
                div [] []

            else
                barcodeScannersView model

        noFilesUploaded : Html Msg
        noFilesUploaded =
            if model.timers == None && BarcodeScanner.isEmpty model.barcodeScannerData then
                Alert.simpleInfo [ class "no-files-uploaded" ] [ text "No files have been uploaded.  Get started by uploading some timer or scanner files." ]

            else
                text ""
    in
    div
        [ on "keyup" keyDecoder ]
        [ Grid.row []
            [ Grid.col [ Col.xs6 ]
                [ div
                    [ class "clearfix" ]
                    [ h1 [ id "header" ] [ text "Results Tidyup" ]
                    , badge
                    , span [ class "about-link" ]
                        [ a [ href aboutUrl, target "_blank" ] [ text "About" ]
                        ]
                    ]
                , actionsPanelView model
                , errorsView model.lastErrors
                ]
            , Grid.col [ Col.xs6 ]
                [ if BarcodeScanner.isEmpty model.barcodeScannerData then
                    div [] []

                  else
                    eventDateAndTimeView model.eventDateAndTime
                ]
            ]
        , Grid.row []
            ([ timersItem, scannersItem ]
                |> List.map (\element -> Grid.col [ Col.xs6 ] [ element ])
            )
        , noFilesUploaded
        , showModalDialog model
        ]
