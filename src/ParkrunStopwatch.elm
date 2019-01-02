module ParkrunStopwatch exposing (main)

import BarcodeScanner exposing (BarcodeScannerData)
import Browser
import DataStructures exposing (EventDateAndTime)
import Error exposing (Error)
import EventDateAndTimeView exposing (eventDateAndTimeView)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class, id, style)
import MergedTable exposing (Stopwatches(..))
import Model exposing (Model, initModel)
import Msg exposing (Msg(..))
import NumberCheckerView exposing (numberCheckerView)
import Ports exposing (fileDrop, getInitialHeight, heightUpdated)
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
        [ fileDrop FileDropped
        , heightUpdated ContainerHeightChanged
        ]


errorView : Maybe Error -> Html a
errorView maybeError =
    case maybeError of
        Just someError ->
            div [ class "alert alert-danger" ]
                [ text ("Unable to read in the dropped file: " ++ someError.message) ]

        Nothing ->
            text ""


getHeightAttribute : Maybe Int -> List (Html.Attribute a)
getHeightAttribute lastHeight =
    case lastHeight of
        Just someHeight ->
            [ style "height" (String.fromInt someHeight ++ "px") ]

        Nothing ->
            []


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [ id "header" ] [ text "Parkrun stopwatch comparison/merging" ]
        , errorView model.lastError
        , div [ class "row" ]
            [ div (class "col-xs-6" :: getHeightAttribute model.lastHeight)
                [ eventDateAndTimeView model.eventDateAndTime
                , stopwatchesView model.stopwatches model.barcodeScannerData model.lastHeight model.highlightedNumberCheckerId
                ]
            , div (class "col-xs-6" :: getHeightAttribute model.lastHeight)
                [ problemsView model.problems
                , numberCheckerView model.numberCheckerEntries model.numberCheckerManualEntryRow model.lastHeight
                ]
            ]
        ]
