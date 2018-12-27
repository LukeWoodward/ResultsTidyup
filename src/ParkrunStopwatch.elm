module ParkrunStopwatch exposing (main)

import BarcodeScanner exposing (BarcodeScannerData)
import Browser
import Error exposing (Error)
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
import UpdateLogic exposing (update)


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( initModel, getInitialHeight () )


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
        [ class "container" ]
        [ h1 [ id "header" ] [ text "Parkrun stopwatch comparison/merging" ]
        , errorView model.lastError
        , div [ class "row" ]
            [ div (class "col-xs-6" :: getHeightAttribute model.lastHeight)
                [ stopwatchesView model.stopwatches model.barcodeScannerData model.lastHeight model.highlightedNumberCheckerId ]
            , div (class "col-xs-6" :: getHeightAttribute model.lastHeight)
                [ problemsView model.problems
                , numberCheckerView model.numberCheckerEntries model.lastHeight
                ]
            ]
        ]
