module ViewCommon exposing (athleteLink, intCell, normalButton, outlineButton, plainCell, smallButton, tableHeader, tableHeaderWithClass, tableHeaders)

import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Html exposing (Html, a, text)
import Html.Attributes exposing (class, href, rel, target)
import Msg exposing (Msg)


urlResultsPrefix : String
urlResultsPrefix =
    "http://www.parkrun.org.uk/parkrunner/"


plainCell : String -> Table.Cell a
plainCell contents =
    Table.td [] [ text contents ]


intCell : Int -> Table.Cell a
intCell contents =
    plainCell (String.fromInt contents)


tableHeader : String -> Table.Cell a
tableHeader headerText =
    Table.th [] [ text headerText ]


tableHeaderWithClass : String -> String -> Table.Cell a
tableHeaderWithClass headerText className =
    Table.th [ Table.cellAttr (class className) ] [ text headerText ]


tableHeaders : List String -> Table.THead a
tableHeaders headerTexts =
    Table.simpleThead (List.map tableHeader headerTexts)


normalButton : Msg -> List (Html.Attribute Msg) -> String -> Html Msg
normalButton msg attributes contents =
    Button.button
        [ Button.primary
        , Button.attrs attributes
        , Button.onClick msg
        ]
        [ text contents ]


outlineButton : Msg -> List (Html.Attribute Msg) -> String -> Html Msg
outlineButton msg attributes contents =
    Button.button
        [ Button.outlinePrimary
        , Button.attrs attributes
        , Button.onClick msg
        ]
        [ text contents ]


smallButton : Msg -> List (Html.Attribute Msg) -> String -> Html Msg
smallButton msg attributes contents =
    Button.button
        [ Button.primary
        , Button.attrs (class "btn-xs" :: attributes)
        , Button.onClick msg
        ]
        [ text contents ]


{-| Returns an HTML link that links to the athlete's full result history.
The athlete ID is assumed to contain the leading 'A'.
-}
athleteLink : String -> Html a
athleteLink athleteId =
    a
        [ rel "nofollow"
        , href (urlResultsPrefix ++ String.dropLeft 1 athleteId)
        , target "_blank"
        ]
        [ text athleteId ]
