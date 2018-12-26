module ViewCommon exposing (intCell, plainCell, tableHeaders)

import Html exposing (Html, td, text, th, thead, tr)


plainCell : String -> Html a
plainCell contents =
    td [] [ text contents ]


intCell : Int -> Html a
intCell contents =
    plainCell (String.fromInt contents)


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
