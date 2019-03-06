module ResultsTidyup exposing (main)

import BarcodeScanner exposing (BarcodeScannerData)
import BarcodeScannerView exposing (barcodeScannersView)
import Bootstrap.Alert as Alert
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Tab as Tab
import Browser
import DataStructures exposing (EventDateAndTime, SecondTab(..))
import Error exposing (FileError)
import EventDateAndTimeView exposing (eventDateAndTimeView)
import Html exposing (Html, a, div, h1, h3, li, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, style)
import Html.Events exposing (onClick)
import MergedTable exposing (Stopwatches(..))
import Model exposing (Model, initModel)
import Msg exposing (Msg(..))
import NumberCheckerView exposing (numberCheckerView)
import Ports exposing (filesDropped, getInitialHeight, heightUpdated)
import Problems
import ProblemsView exposing (problemsView)
import StopwatchesView exposing (stopwatchesView)
import TimeHandling exposing (formatHoursAndMinutes)
import UpdateLogic exposing (update)


main : Program (Maybe Int) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Maybe Int -> ( Model, Cmd Msg )
init startTime =
    let
        startTimeAsString : String
        startTimeAsString =
            Maybe.map formatHoursAndMinutes startTime
                |> Maybe.withDefault ""

        initialEventDateAndTime : EventDateAndTime
        initialEventDateAndTime =
            EventDateAndTime "" Nothing startTimeAsString startTime
    in
    ( { initModel | eventDateAndTime = initialEventDateAndTime }, getInitialHeight () )


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


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [ id "header" ] [ text "Results tidy-up" ]
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
                            , pane = Tab.pane [] [ barcodeScannersView model.barcodeScannerData.files ]
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
