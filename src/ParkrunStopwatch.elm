module ParkrunStopwatch exposing (..)

import Html exposing (Html, program, div, text, table, tr, td, h1)
import Html.Attributes exposing (class)
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import TimeHandling exposing (formatTime)
import Ports exposing (fileDrop)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model, Update, View.


type alias Model =
    { stopwatches : List Stopwatch
    , lastError : Maybe String
    }


initModel : Model
initModel =
    { stopwatches = []
    , lastError = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


type Msg
    = FileDropped String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileDropped fileText ->
            case readStopwatchData fileText of
                Ok stopwatch ->
                    ( { model
                        | stopwatches = [ stopwatch ]
                        , lastError = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | lastError = Just error.message }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileDrop FileDropped


errorView : Maybe String -> List (Html a)
errorView maybeString =
    case maybeString of
        Just string ->
            [ div [ class "alert alert-danger" ]
                [ text ("Unable to read in the stopwatch data: " ++ string) ]
            ]

        Nothing ->
            []


stopwatchRow : Int -> Int -> Html a
stopwatchRow index time =
    tr []
        [ td [] [ text (toString (index + 1)) ]
        , td [] [ text (formatTime time) ]
        ]


stopwatchView : List Stopwatch -> List (Html a)
stopwatchView stopwatches =
    case stopwatches of
        [] ->
            []

        (StopwatchData data) :: _ ->
            [ table [] (List.indexedMap stopwatchRow data) ]


view : Model -> Html Msg
view model =
    div
        []
        (h1 [] [ text "Parkrun stopwatch comparison/merging" ]
            :: (errorView model.lastError)
            ++ (stopwatchView model.stopwatches)
        )
