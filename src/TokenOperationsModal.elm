module TokenOperationsModal exposing (tokenOperationsButtons, tokenOperationsDialogSize, tokenOperationsDialogTitle, tokenOperationsModalBody)

import DataEntry exposing (RangeEntry, rangeToString)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, classList, disabled, for, id, type_, value)
import Html.Events exposing (onClick, onInput)
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
import ViewCommon exposing (ModalSize(..), normalButton, outlineButton)


tokenOperationsDialogTitle : String
tokenOperationsDialogTitle =
    "Token operations"


textRow : TokenOperationOption -> TokenOperationEditDetails -> String -> Html Msg
textRow changeType tokenOperationEditDetails textContents =
    div [ class "token-operation-help-text", classList [ ( "selected", tokenOperationEditDetails.operation == changeType ) ] ]
        [ text textContents ]


radioButton : String -> TokenOperationOption -> String -> TokenOperationEditDetails -> Html Msg
radioButton elementId option labelText tokenOperationEditDetails =
    div [ class "col-6 col-md-3 col-form-label form-check" ]
        [ input [ id elementId, class "form-check-input", type_ "radio", checked (tokenOperationEditDetails.operation == option), onClick (TokenOperationEdit (ChangeOperation option)) ] []
        , label [ for elementId, class "form-check-label" ] [ text labelText ]
        ]


inputTextField : (TokenOperationEditDetails -> RangeEntry) -> TokenRangeField -> TokenOperationOption -> (TokenOperationEditDetails -> Bool) -> TokenOperationEditDetails -> Html Msg
inputTextField rangeEntryGetter field option validator tokenOperationEditDetails =
    let
        validationAttributes : List (Html.Attribute Msg)
        validationAttributes =
            if validator tokenOperationEditDetails then
                [ class "is-invalid" ]

            else
                []
    in
    div [ class "col-6 col-md-3" ]
        [ input
            (type_ "text"
                :: class "form-control"
                :: value (rangeEntryGetter tokenOperationEditDetails).enteredValue
                :: onInput (TokenOperationEdit << RangeEdited field)
                :: disabled (tokenOperationEditDetails.operation /= option)
                :: validationAttributes
            )
            []
        ]


validationErrorToString : TokenOperationValidationError -> String
validationErrorToString validationError =
    case validationError of
        NoValidationError ->
            ""

        TokenOperationNotSelected ->
            "Please select a token operation"

        InvalidRange _ ->
            "Please enter a valid token or a range of tokens, e.g. 47 or 81-90"

        EmptyRange _ ->
            "Please enter token ranges with the lower end of the range first, e.g. 81-90"

        ZeroInRange _ ->
            "Token number 0 cannot be used"

        TokenOffEndOfTokens lastToken token _ ->
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

        RangeOffEndOfTokens lastToken range _ ->
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
    div [ class "container-fluid" ]
        [ div [ class "row mb-3" ]
            [ radioButton "insertTokensRadioButtonId" InsertTokensOption "Insert token(s)" tokenOperationEditDetails
            , inputTextField .insertTokenRange InsertTokenRangeField InsertTokensOption isInsertTokenRangeFieldInvalid tokenOperationEditDetails
            ]
        , textRow InsertTokensOption
            tokenOperationEditDetails
            ("Use this option if a token should have been given out, but wasn't, e.g. due to a funnel ducker or "
                ++ "a person declined to take a finish token and a token wasn't put aside for that finisher."
            )
        , div [ class "row mb-3" ]
            [ radioButton "removeTokensRadioButtonId" RemoveTokensOption "Remove token(s)" tokenOperationEditDetails
            , inputTextField .removeTokenRange RemoveTokenRangeField RemoveTokensOption isRemoveTokenRangeFieldInvalid tokenOperationEditDetails
            ]
        , textRow RemoveTokensOption
            tokenOperationEditDetails
            ("Use this option if one or more tokens were not given out, for example, they were missing before the event "
                ++ "started, they were dropped during the event by the finish token volunteers and not given out, "
                ++ "or more than one token was given to a finisher."
            )
        , div [ class "row mb-3" ]
            [ radioButton "swapTokensRadioButtonId" SwapTokenRangeOption "Swap token(s)" tokenOperationEditDetails
            , inputTextField .swapTokenRange1 SwapTokenRangeField1 SwapTokenRangeOption isSwapTokenRange1FieldInvalid tokenOperationEditDetails
            , div [ class "col-5 offset-1 col-md-1 offset-md-0 col-form-label" ] [ text " and " ]
            , inputTextField .swapTokenRange2 SwapTokenRangeField2 SwapTokenRangeOption isSwapTokenRange2FieldInvalid tokenOperationEditDetails
            ]
        , textRow SwapTokenRangeOption
            tokenOperationEditDetails
            ("Use this option if you give out tokens in batches (e.g. 25, 50 or 100 tokens) but one or more of "
                ++ "these batches were given out in the wrong order."
            )
        , div [ class "row mb-3" ]
            [ radioButton "reverseTokensRadioButtonId" ReverseTokenRangeOption "Reverse tokens" tokenOperationEditDetails
            , inputTextField .reverseTokenRange ReverseTokenRangeField ReverseTokenRangeOption isReverseTokenRangeFieldInvalid tokenOperationEditDetails
            ]
        , textRow ReverseTokenRangeOption tokenOperationEditDetails "Use this option if a range of tokens was given out in the reverse order."
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


tokenOperationsDialogSize : ModalSize
tokenOperationsDialogSize =
    Large
