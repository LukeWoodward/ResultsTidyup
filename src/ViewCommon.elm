module ViewCommon exposing (intCell, plainCell, smallButton, tableHeaders)

import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Html exposing (Html, text)
import Msg exposing (Msg)


plainCell : String -> Table.Cell a
plainCell contents =
    Table.td [] [ text contents ]


intCell : Int -> Table.Cell a
intCell contents =
    plainCell (String.fromInt contents)


tableHeader : String -> Table.Cell a
tableHeader headerText =
    Table.th [] [ text headerText ]


tableHeaders : List String -> Table.THead a
tableHeaders headerTexts =
    Table.simpleThead (List.map tableHeader headerTexts)


smallButton : Msg -> List (Html.Attribute Msg) -> String -> Html Msg
smallButton msg attributes contents =
    Button.button
        [ Button.primary
        , Button.small
        , Button.attrs attributes
        , Button.onClick msg
        ]
        [ text contents ]
