module ParkrunStopwatch exposing (..)

import Html exposing (Html, program, div, text, table, tbody, thead, tr, td, th, h1)
import Html.Attributes exposing (class)
import Stopwatch exposing (Stopwatch(..), readStopwatchData)
import Merger exposing (merge, MergeEntry(..))
import TimeHandling exposing (formatTime)
import Ports exposing (fileDrop)


maxNearMatchDistance : Int
maxNearMatchDistance =
    1
    

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
                        | stopwatches = model.stopwatches ++ [ stopwatch ]
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


noStopwatchesUploadedMessage : List Stopwatch -> List (Html a)
noStopwatchesUploadedMessage stopwatches =
    case stopwatches of
        [] ->
            [ div [ class "alert alert-info" ]
                [ text "No stopwatch files have been uploaded" ]
            ]
        [_] ->
            [ div [ class "alert alert-info" ]
                [ text "Please upload another stopwatch file to enable comparison/merging" ]
            ]
        _ ->
            []
            

cell : String -> Html a
cell contents =
    td [] [ text contents ]


timeCell : String -> Int -> Html a
timeCell className time =
    td [ class className] [ text (formatTime time) ]
    
stopwatchRow : Int -> Int -> Html a
stopwatchRow index time =
    tr []
        [ cell (toString (index + 1))
        , cell (formatTime time)
        ]

        
mergedStopwatchRow : Int -> MergeEntry -> Html a
mergedStopwatchRow index entry =
    let
        indexCell =
            cell (toString (index + 1))
    in
        case entry of
            ExactMatch time ->
                tr
                    []
                    [ indexCell
                    , timeCell "exact-match" time
                    , timeCell "exact-match" time
                    ]
                    
            NearMatch time1 time2 ->
                tr
                    []
                    [ indexCell
                    , timeCell "near-match" time1
                    , timeCell "near-match" time2
                    ]
                    
            Watch1Only time1 ->
                tr
                    []
                    [ indexCell
                    , timeCell "mismatch" time1
                    , cell ""
                    ]
                    
            Watch2Only time2 ->
                tr
                    []
                    [ indexCell
                    , cell ""
                    , timeCell "mismatch" time2
                    ]

                    
tableHeader : String -> Html a
tableHeader headerText =
    th [] [ text headerText ]
                    

tableHeaders : List String -> Html a
tableHeaders headerTexts = 
    thead
        []
        [ tr
            []
            (List.map tableHeader headerTexts)
        ]

                    
stopwatchView : List Stopwatch -> List (Html a)
stopwatchView stopwatches =
    case stopwatches of
        [] ->
            []

        StopwatchData data :: [] ->
            [ table
                [ class "table condensed-table" ]
                [ tableHeaders [ "Position", "Stopwatch 1" ]
                , tbody [] (List.indexedMap stopwatchRow data)
                ]
            ]
            
        StopwatchData data1 :: StopwatchData data2 :: _ ->
            let 
                mergedData =
                    merge maxNearMatchDistance data1 data2
            in
                [ table
                    [ class "table condensed-table" ]
                    [ tableHeaders [ "Position", "Stopwatch 1", "Stopwatch 2" ]
                    , tbody
                        [] 
                        (List.indexedMap mergedStopwatchRow mergedData)
                    ]
                ]


view : Model -> Html Msg
view model =
    div
        []
        (h1 [] [ text "Parkrun stopwatch comparison/merging" ]
            :: (errorView model.lastError)
            ++ (noStopwatchesUploadedMessage model.stopwatches)
            ++ (stopwatchView model.stopwatches)
        )
