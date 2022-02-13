module TokenOperationsModal exposing (tokenOperationsButtons, tokenOperationsDialogSizer, tokenOperationsDialogTitle, tokenOperationsModalBody)

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import DataEntry exposing (RangeEntry, rangeToString)
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class, for)
import Html.Events exposing (onClick)
import Msg exposing (Msg(..))
import TokenOperations
    exposing
        ( TokenOperationChangeType(..)
        , TokenOperationEditDetails
        , TokenOperationOption(..)
        , TokenOperationValidationError(..)
        , TokenRangeField(..)
        , isInsertTokenRangeFieldInvalid
        , isRemoveTokenRangeFieldInvalid
        , isReverseTokenRangeFieldInvalid
        , isSwapTokenRange1FieldInvalid
        , isSwapTokenRange2FieldInvalid
        )
import ViewCommon exposing (normalButton, outlineButton)


tokenOperationsDialogTitle : String
tokenOperationsDialogTitle =
    "Token operations"


textRow : String -> Html Msg
textRow textContents =
    div [ class "token-operation-help-text" ] [ text textContents ]


radioButton : String -> TokenOperationOption -> String -> TokenOperationEditDetails -> Grid.Column Msg
radioButton elementId option labelText tokenOperationEditDetails =
    Grid.col [ Col.xs3 ]
        [ Radio.radio
            [ Radio.id elementId
            , Radio.checked (tokenOperationEditDetails.operation == option)
            , Radio.onClick (TokenOperationEdit (ChangeOperation option))
            ]
            labelText
        ]


inputTextField : (TokenOperationEditDetails -> RangeEntry) -> TokenRangeField -> TokenOperationOption -> (TokenOperationEditDetails -> Bool) -> TokenOperationEditDetails -> Grid.Column Msg
inputTextField rangeEntryGetter field option validator tokenOperationEditDetails =
    let
        dangerAttributes : List (Input.Option Msg)
        dangerAttributes =
            if validator tokenOperationEditDetails then
                [ Input.danger ]

            else
                []
    in
    Grid.col [ Col.xs3 ]
        [ Input.text
            ([ Input.value (rangeEntryGetter tokenOperationEditDetails).enteredValue
             , Input.onInput (TokenOperationEdit << RangeEdited field)
             , Input.disabled (tokenOperationEditDetails.operation /= option)
             ]
                ++ dangerAttributes
            )
        ]


validationErrorToString : TokenOperationValidationError -> String
validationErrorToString validationError =
    case validationError of
        NoValidationError ->
            ""

        TokenOperationNotSelected ->
            "Please select a token operation"

        InvalidRange field ->
            "Please enter a valid token or a range of tokens, e.g. 47 or 81-90"

        EmptyRange field ->
            "Please enter token ranges with the lower end of the range first, e.g. 81-90"

        ZeroInRange field ->
            "Token number 0 cannot be used"

        TokenOffEndOfTokens lastToken token field ->
            "Token " ++ String.fromInt token ++ " is beyond the last token used (" ++ String.fromInt lastToken ++ ")"

        InsertRangeOffEndOfTokens lastToken range ->
            "The range " ++ rangeToString range ++ " is entirely beyond the last token used (" ++ String.fromInt lastToken ++ ")"

        RemovingExistingToken token ->
            "Token " ++ String.fromInt token ++ " cannot be removed because it is used"

        RemovingExistingTokens tokens range ->
            let
                reason : String
                reason =
                    case tokens of
                        [ singleToken ] ->
                            "token " ++ String.fromInt singleToken ++ " is used"

                        _ ->
                            "tokens "
                                ++ (List.map String.fromInt tokens
                                        |> String.join ", "
                                   )
                                ++ " are used"
            in
            "Tokens " ++ rangeToString range ++ " cannot be removed because " ++ reason

        RangeOffEndOfTokens lastToken range field ->
            "The range " ++ rangeToString range ++ " goes beyond the last token used (" ++ String.fromInt lastToken ++ ")"

        SwapTokenRangesOfDifferentSizes ->
            "The ranges of tokens to swap are of different sizes.  Please enter two ranges of tokens that are the same size."

        SwapTokenRangesOverlap ->
            "The ranges of tokens to swap overlap.  Please enter two non-overlapping ranges of tokens"

        ReverseTokenRangeSingleToken ->
            "You cannot reverse a single token.  Please specify a range of tokens to reverse"


validationErrorRow : TokenOperationValidationError -> Html Msg
validationErrorRow validationError =
    div [ class "validation-error" ] [ text (validationErrorToString validationError) ]


tokenOperationsModalBody : TokenOperationEditDetails -> Html Msg
tokenOperationsModalBody tokenOperationEditDetails =
    div []
        [ Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "insertTokensRadioButtonId" InsertTokensOption "Insert token(s)" tokenOperationEditDetails
            , inputTextField .insertTokenRange InsertTokenRangeField InsertTokensOption isInsertTokenRangeFieldInvalid tokenOperationEditDetails
            ]
        , textRow
            ("Use this option if a token should have been given out, but wasn't, e.g. due to a funnel ducker or "
                ++ "a person declined to take a finish token and a token wasn't put aside for that finisher."
            )
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "removeTokensRadioButtonId" RemoveTokensOption "Remove token(s)" tokenOperationEditDetails
            , inputTextField .removeTokenRange RemoveTokenRangeField RemoveTokensOption isRemoveTokenRangeFieldInvalid tokenOperationEditDetails
            ]
        , textRow
            ("Use this option if one or more tokens were not given out, for example, they were missing before the event "
                ++ "started, they were dropped during the event by the finish token volunteers and not given out, "
                ++ "or more than one token was given to a finisher."
            )
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "swapTokensRadioButtonId" SwapTokenRangeOption "Swap token(s)" tokenOperationEditDetails
            , inputTextField .swapTokenRange1 SwapTokenRangeField1 SwapTokenRangeOption isSwapTokenRange1FieldInvalid tokenOperationEditDetails
            , Grid.col [ Col.xs1 ] [ text " and " ]
            , inputTextField .swapTokenRange2 SwapTokenRangeField2 SwapTokenRangeOption isSwapTokenRange2FieldInvalid tokenOperationEditDetails
            ]
        , textRow
            ("Use this option if you give out tokens in batches (e.g. 25, 50 or 100 tokens) but one or more of "
                ++ "these batches were given out in the wrong order."
            )
        , Grid.row [ Row.attrs [ class "form-group align-items-center" ] ]
            [ radioButton "reverseTokensRadioButtonId" ReverseTokenRangeOption "Reverse tokens" tokenOperationEditDetails
            , inputTextField .reverseTokenRange ReverseTokenRangeField ReverseTokenRangeOption isReverseTokenRangeFieldInvalid tokenOperationEditDetails
            ]
        , textRow "Use this option if a range of tokens was given out in the reverse order."
        , validationErrorRow tokenOperationEditDetails.validationError
        ]


processTokenOperationsButtonText : TokenOperationEditDetails -> Maybe String
processTokenOperationsButtonText tokenOperationEditDetails =
    case tokenOperationEditDetails.operation of
        NoOptionSelected ->
            Nothing

        InsertTokensOption ->
            Just "Insert tokens"

        RemoveTokensOption ->
            Just "Remove tokens"

        SwapTokenRangeOption ->
            Just "Swap tokens"

        ReverseTokenRangeOption ->
            Just "Reverse tokens"


tokenOperationsButtons : TokenOperationEditDetails -> List (Html Msg)
tokenOperationsButtons tokenOperationEditDetails =
    let
        processButtonText : Maybe String
        processButtonText =
            processTokenOperationsButtonText tokenOperationEditDetails

        processButtons : List (Html Msg)
        processButtons =
            case processButtonText of
                Just buttonText ->
                    [ normalButton (ApplyTokenOperation tokenOperationEditDetails) [] buttonText ]

                Nothing ->
                    []
    in
    processButtons ++ [ outlineButton CloseModal [] "Close" ]


tokenOperationsDialogSizer : Modal.Config Msg -> Modal.Config Msg
tokenOperationsDialogSizer =
    Modal.large
