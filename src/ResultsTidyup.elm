module ResultsTidyup exposing (main)

import BarcodeScanner exposing (BarcodeScannerData)
import BarcodeScannerEditModal
import BarcodeScannerView exposing (barcodeScannersView)
import Bootstrap.Alert as Alert
import Bootstrap.Badge as Badge
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Tab as Tab
import Browser
import Browser.Dom
import Commands exposing (Command(..), ElementToFocus(..))
import Error exposing (FileError)
import EventDateAndTime exposing (EventDateAndTime)
import EventDateAndTimeView exposing (eventDateAndTimeView)
import File.Download as Download
import Html exposing (Html, a, div, h1, h3, li, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, style)
import Html.Events exposing (on, onClick)
import Json.Decode exposing (Decoder, andThen, fail, field, int, succeed)
import Model exposing (Model, SecondTab(..), initModel)
import Msg exposing (Msg(..))
import NumberCheckerView exposing (numberCheckerView)
import Ports exposing (filesDropped, getInitialHeight, heightUpdated, recordEventStartTime)
import Problems
import ProblemsView exposing (problemsView)
import Stopwatch exposing (Stopwatches(..))
import StopwatchesView exposing (stopwatchesView)
import Task exposing (Task)
import Time
import TimeHandling exposing (formatHoursAndMinutes)
import UpdateLogic exposing (update)


type alias FlagsRecord =
    { startTime : Maybe Int
    , isBeta : Bool
    }


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
            EventDateAndTime "" Nothing startTimeAsString startTime
    in
    ( { initModel
        | isBeta = isBeta
        , eventDateAndTime = initialEventDateAndTime
      }
    , getInitialHeight ()
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ filesDropped FilesDropped
        , heightUpdated ContainerHeightChanged
        ]


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


getHeightAttribute : Maybe Int -> List (Html.Attribute a)
getHeightAttribute lastHeight =
    case lastHeight of
        Just someHeight ->
            [ style "height" (String.fromInt someHeight ++ "px") ]

        Nothing ->
            []


classAttributes : SecondTab -> SecondTab -> List (Html.Attribute Msg)
classAttributes wantedTab actualTab =
    if wantedTab == actualTab then
        [ class "nav-link active" ]

    else
        [ class "nav-link" ]


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
    in
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus elementId)


getDownloadOperation : Commands.DownloadOperation -> (Time.Zone -> Time.Posix -> Msg)
getDownloadOperation downloadOperation =
    case downloadOperation of
        Commands.DownloadSingleStopwatch which ->
            DownloadStopwatch which

        Commands.DownloadMergedStopwatches ->
            DownloadMergedStopwatchData

        Commands.DownloadBarcodeScannerFile filename ->
            DownloadBarcodeScannerFile filename


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


handleEscape : Int -> Decoder Msg
handleEscape keyCode =
    if keyCode == 27 then
        succeed CloseModal

    else
        fail "not Escape key"


escapeKeyDecoder : Decoder Msg
escapeKeyDecoder =
    field "keyCode" int
        |> andThen handleEscape


updateAndMapCommand : Msg -> Model -> ( Model, Cmd Msg )
updateAndMapCommand msg model =
    update msg model
        |> Tuple.mapSecond mapCommand


view : Model -> Html Msg
view model =
    let
        badge : Html Msg
        badge =
            if model.isBeta then
                Badge.badgePrimary [] [ text "BETA" ]

            else
                text ""
    in
    div
        [ on "keyup" escapeKeyDecoder ]
        [ div
            [ class "clearfix" ]
            [ h1 [ id "header" ] [ text "Results Tidyup" ]
            , badge
            ]
        , errorsView model.lastErrors
        , Grid.row []
            [ Grid.col [ Col.xs6 ]
                [ eventDateAndTimeView model.eventDateAndTime
                , stopwatchesView model.stopwatches model.barcodeScannerData model.lastHeight model.highlightedNumberCheckerId
                ]
            , Grid.col [ Col.xs6 ]
                [ problemsView model.problems
                , Tab.config ChangeSecondTab
                    |> Tab.items
                        [ Tab.item
                            { id = "barcodeScannersTab"
                            , link = Tab.link [] [ text "Barcode scanners" ]
                            , pane = Tab.pane [] [ barcodeScannersView model ]
                            }
                        , Tab.item
                            { id = "numberCheckerTab"
                            , link = Tab.link [] [ text "Number checker" ]
                            , pane = Tab.pane [] [ numberCheckerView model.numberCheckerEntries model.numberCheckerManualEntryRow model.lastHeight ]
                            }
                        ]
                    |> Tab.view model.secondTab
                ]
            ]
        ]
