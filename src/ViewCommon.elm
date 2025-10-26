module ViewCommon exposing
    ( ModalSize(..)
    , athleteLink
    , dangerButton
    , dangerIconButton
    , intCell
    , normalButton
    , normalIconButton
    , outlineButton
    , plainCell
    , role
    , smallButton
    , tableHeader
    , tableHeaderWithClass
    , tableHeaders
    )

import Html exposing (Html, a, button, td, text, th, thead)
import Html.Attributes exposing (attribute, class, href, rel, target, title)
import Html.Events exposing (onClick)
import Msg exposing (Msg)


role : String -> Html.Attribute a
role =
    attribute "role"


urlResultsPrefix : String
urlResultsPrefix =
    "https://www.parkrun.org.uk/parkrunner/"


plainCell : String -> Html a
plainCell contents =
    td [] [ text contents ]


intCell : Int -> Html a
intCell contents =
    plainCell (String.fromInt contents)


tableHeader : String -> Html a
tableHeader headerText =
    th [] [ text headerText ]


tableHeaderWithClass : String -> String -> Html a
tableHeaderWithClass headerText className =
    th [ class className ] [ text headerText ]


tableHeaders : List String -> Html a
tableHeaders headerTexts =
    thead [] (List.map tableHeader headerTexts)


buttonWithClass : String -> Msg -> List (Html.Attribute Msg) -> Html Msg -> Html Msg
buttonWithClass className msg attributes contents =
    button
        (class ("btn " ++ className) :: onClick msg :: attributes)
        [ contents ]


normalButton : Msg -> List (Html.Attribute Msg) -> String -> Html Msg
normalButton msg attributes contents =
    buttonWithClass "btn-primary" msg attributes (text contents)


dangerButton : Msg -> List (Html.Attribute Msg) -> String -> Html Msg
dangerButton msg attributes contents =
    buttonWithClass "btn-danger" msg attributes (text contents)


outlineButton : Msg -> List (Html.Attribute Msg) -> String -> Html Msg
outlineButton msg attributes contents =
    buttonWithClass "btn-outline-primary" msg attributes (text contents)


smallButton : Msg -> List (Html.Attribute Msg) -> String -> Html Msg
smallButton msg attributes contents =
    buttonWithClass "btn-primary btn-xs" msg attributes (text contents)


normalIconButton : Msg -> Html Msg -> String -> Html Msg
normalIconButton msg icon iconTooltip =
    buttonWithClass "btn-primary" msg [ title iconTooltip ] icon


dangerIconButton : Msg -> Html Msg -> String -> Html Msg
dangerIconButton msg icon iconTooltip =
    buttonWithClass "btn-danger" msg [ title iconTooltip ] icon


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


type ModalSize
    = Standard
    | Large
