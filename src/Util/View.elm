module Util.View exposing (HintConfig, HintPosition(..), ValuesFormatted, ValuesRow, addDot, colorToHex, conditionalHide, copyIconPathfinder, copyIconPathfinderAbove, copyIconPathfinderFixed, emptyCell, firstToUpper, fixFillRule, frame, fullWidthCss, hovercard, iconWithHint, ifTrue, indirectTagFillAttr, inputFieldStyles, makeValuesList, noTextSelection, none, onClickWithStop, p, pointer, testId, testKey, timeToCell, toCssColor, truncate, truncateLongIdentifier, truncateLongIdentifierWithLengths)

import Api.Data
import Basics.Extra exposing (flip)
import Color as BColor
import Config.View as View
import Css exposing (Color, Style)
import Css.View as Css
import Dict
import Hex
import Hovercard
import Html as BHtml
import Html.Attributes
import Html.Styled exposing (Attribute, Html, div, span)
import Html.Styled.Attributes exposing (attribute, css)
import Html.Styled.Events exposing (stopPropagationOn)
import Json.Decode
import List.Extra
import Model.Currency as Currency exposing (AssetIdentifier)
import Model.Locale as Locale
import RecordSetter exposing (s_anchor, s_hint, s_iconsCopyS, s_label, s_triangle)
import Theme.Colors as Colors
import Theme.Html.Fields as Fields
import Theme.Html.GraphComponents
import Tuple exposing (pair)
import Util.Css
import Util.Data as Data
import View.Locale as Locale


{-| A stable hook for browser tests, naming what an element _is_.

Names are prefixed `gs-` because plugins render into this same DOM from their
own repositories; the prefix keeps their hooks and ours from colliding. Write
the prefix out at the call site rather than adding it here, so a name found in
`e2e/` can be grepped for in `src/` and vice versa.

A name describes a kind, not an instance: several nodes share `gs-address-node`.
Playwright's strict mode fails an assertion whose locator matches more than one
element, so pair it with [`testKey`](#testKey) when a test needs a particular
one.

The markup is generated from Figma and styled with elm-css, so class names are
content hashes that change with any styling tweak, and visible text is
translated and truncated. Neither survives as a selector. This attribute exists
only so `e2e/` can find an element; it has no effect at runtime.

Add one at the _call site_ of a generated component (its `WithAttributes`
variant takes an attribute list per node) — never inside `generated/`, which the
next codegen run overwrites.

-}
testId : String -> Attribute msg
testId =
    attribute "data-testid"


{-| Identifies _which_ of a kind an element is, for tests that need one in
particular:

    [ testId "gs-address-node", testKey (Id.toString address.id) ]

selects with `[data-testid="gs-address-node"][data-testkey="btc1Archive…"]`.

-}
testKey : String -> Attribute msg
testKey =
    attribute "data-testkey"


none : Html msg
none =
    span [] []


toCssColor : BColor.Color -> Color
toCssColor color =
    BColor.toRgba color
        |> (\{ red, green, blue, alpha } ->
                Css.rgba (red * 255 |> Basics.round) (green * 255 |> Basics.round) (blue * 255 |> Basics.round) alpha
           )


firstToUpper : String -> String
firstToUpper str =
    String.left 1 str
        |> String.toUpper
        |> (\f -> f ++ String.dropLeft 1 str)


truncate : Int -> String -> String
truncate len str =
    if String.length str > len && len > 6 then
        String.left (len - 3) str ++ "…"

    else
        str


truncateLongIdentifier : String -> String
truncateLongIdentifier =
    truncateLongIdentifierWithLengths 8 4


truncateLongIdentifierWithLengths : Int -> Int -> String -> String
truncateLongIdentifierWithLengths start end str =
    let
        zeroInfoPrefixes =
            [ "bc1", "ltc1", "0x" ]

        inputLength =
            String.length str
    in
    if inputLength <= (start + end + 3) then
        str

    else if inputLength > 18 then
        let
            -- sigPart =
            --     if String.startsWith "0x" str then
            --         String.right (String.length str - 2) str
            --     else
            --         str
            sigPart =
                str

            startwOffset =
                start
                    + (zeroInfoPrefixes
                        |> List.Extra.find (\prefix -> String.startsWith prefix str)
                        |> Maybe.map String.length
                        |> Maybe.withDefault 0
                      )
        in
        String.left startwOffset sigPart ++ "…" ++ String.right end sigPart

    else
        str


hovercard : View.Config -> Hovercard.Model -> Int -> List (BHtml.Html msg) -> Html.Styled.Html msg
hovercard vc element zIndex =
    Hovercard.view
        (Hovercard.defaultConfig
            |> Hovercard.withTickLength 16
            |> Hovercard.withZIndex zIndex
            |> Hovercard.withBorderColorString Colors.grey50
            |> Hovercard.withBackgroundColorString Colors.white
            |> Hovercard.withViewport vc.size
        )
        element
        (Css.hovercard vc
            |> List.map (\( k, v ) -> Html.Attributes.style k v)
        )
        >> Html.Styled.fromUnstyled


p : View.Config -> List (Attribute msg) -> List (Html msg) -> Html msg
p vc attrs =
    Html.Styled.p
        ((Css.paragraph vc |> css) :: attrs)


addDot : String -> String
addDot s =
    s ++ "."


type HintPosition
    = Right
    | Above


copyIconPathfinder : View.Config -> String -> Html msg
copyIconPathfinder =
    copyIconWithAttrPathfinder False (([ Css.verticalAlign Css.middle ] |> css) |> List.singleton)


copyIconPathfinderFixed : View.Config -> String -> Html msg
copyIconPathfinderFixed =
    copyIconWithAttrPathfinderInternal True Right False (([ Css.verticalAlign Css.middle ] |> css) |> List.singleton)


copyIconPathfinderAbove : View.Config -> String -> Html msg
copyIconPathfinderAbove =
    copyIconWithAttrPathfinderInternal False Above False (([ Css.verticalAlign Css.middle ] |> css) |> List.singleton)


copyIconWithAttrPathfinder : Bool -> List (Attribute msg) -> View.Config -> String -> Html msg
copyIconWithAttrPathfinder =
    copyIconWithAttrPathfinderInternal False Right


copyIconWithAttrPathfinderInternal : Bool -> HintPosition -> Bool -> List (Attribute msg) -> View.Config -> String -> Html msg
copyIconWithAttrPathfinderInternal fixedHint hp hideHint attr vc value =
    let
        ( component, compAttr, triangleAttr ) =
            case hp of
                Right ->
                    ( Theme.Html.GraphComponents.copyShortcutWithInstances
                    , Theme.Html.GraphComponents.copyShortcutAttributes
                    , css
                        [ Css.px 1 |> Css.left
                        ]
                    )

                Above ->
                    ( Theme.Html.GraphComponents.copyShortcutHintAboveWithInstances
                    , Theme.Html.GraphComponents.copyShortcutHintAboveAttributes
                    , css
                        [ Css.px -5 |> Css.top ]
                    )
    in
    Html.Styled.a
        ((Css.copyIcon vc |> css)
            :: attr
        )
        [ Html.Styled.node "copy-icon"
            [ Html.Styled.Attributes.attribute "data-value" value
            , Locale.string vc.locale "Copied-hint"
                |> Html.Styled.Attributes.attribute "data-copied-label"
            ]
            [ component
                (compAttr
                    |> s_iconsCopyS
                        [ css
                            [ Css.display Css.inlineBlock
                            , Css.color Css.inherit
                            ]
                        ]
                    |> s_hint
                        [ Html.Styled.Attributes.attribute "data-hint" ""
                        , css <|
                            [ Css.display Css.none
                            , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 10))
                            ]
                                ++ (if fixedHint then
                                        [ Css.position Css.fixed |> Css.important ]

                                    else
                                        []
                                   )
                        ]
                    |> s_label
                        [ Html.Styled.Attributes.attribute "data-label" ""
                        ]
                    |> s_anchor
                        [ css
                            [ Css.px 1 |> Css.width |> Css.important
                            ]
                        ]
                    |> s_triangle
                        [ triangleAttr
                        ]
                )
                (Theme.Html.GraphComponents.copyShortcutInstances
                    |> s_hint
                        (if hideHint then
                            Just none

                         else
                            Nothing
                        )
                )
                { root = { hint = Locale.string vc.locale "Copy" }
                }
            ]
        ]


type alias HintConfig msg =
    { position : HintPosition
    , hint : String
    , hide : Bool
    , icon : Html msg
    }


iconWithHint : View.Config -> HintConfig msg -> List (Attribute msg) -> Html msg
iconWithHint vc { position, hint, hide, icon } attr =
    let
        ( ( component, compInst, compAttr ), triangleAttr ) =
            case position of
                Right ->
                    ( ( Theme.Html.GraphComponents.iconWithHintRightWithInstances
                      , Theme.Html.GraphComponents.iconWithHintRightInstances
                      , Theme.Html.GraphComponents.iconWithHintRightAttributes
                      )
                    , css
                        [ Css.px 1 |> Css.left
                        ]
                    )

                Above ->
                    ( ( Theme.Html.GraphComponents.iconWithHintAboveWithInstances
                      , Theme.Html.GraphComponents.iconWithHintAboveInstances
                      , Theme.Html.GraphComponents.iconWithHintAboveAttributes
                      )
                    , css
                        [ Css.px -5 |> Css.top ]
                    )
    in
    Html.Styled.node "with-hint"
        (pointer :: attr)
        [ component
            (compAttr
                |> s_hint
                    [ Html.Styled.Attributes.attribute "data-hint" ""
                    , css
                        [ Css.display Css.none
                        , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 10))
                        , Css.position Css.fixed |> Css.important
                        ]
                    ]
                |> s_label
                    [ Html.Styled.Attributes.attribute "data-label" ""
                    ]
                |> s_anchor
                    [ css
                        [ Css.px 1 |> Css.width |> Css.important
                        ]
                    ]
                |> s_triangle
                    [ triangleAttr
                    ]
            )
            (compInst
                |> s_hint
                    (if hide then
                        Just none

                     else
                        Nothing
                    )
            )
            { root =
                { hint = Locale.string vc.locale hint
                , instance = icon
                }
            }
        ]


colorToHex : BColor.Color -> String
colorToHex cl =
    let
        { red, green, blue } =
            BColor.toRgba cl
    in
    List.map (round >> Hex.toString) [ red * 255, green * 255, blue * 255 ]
        |> List.map (String.padLeft 2 '0')
        |> (::) "#"
        |> String.concat


frame : View.Config -> List (Attribute msg) -> List (Html msg) -> Html msg
frame vc attr =
    div
        ((Css.frame vc |> css) :: attr)


onClickWithStop : msg -> Attribute msg
onClickWithStop msg =
    Json.Decode.succeed ( msg, True )
        |> stopPropagationOn "click"


pointer : Attribute msg
pointer =
    css [ Css.cursor Css.pointer ]


noTextSelection : Attribute msg
noTextSelection =
    css
        [ Css.property "user-select"
            "none"
        , Css.property
            "-ms-user-select"
            "none"
        , Css.property
            "-webkit-user-select"
            "none"
        ]


fullWidthCss : Style
fullWidthCss =
    Css.pct 100 |> Css.width |> Css.important


fixFillRule : Attribute msg
fixFillRule =
    [ Css.property "fill-rule" "evenodd"
    ]
        |> css


indirectTagFillAttr : List (Attribute msg)
indirectTagFillAttr =
    -- `--c-greyBlue500` resolves to a near-white in dark mode, turning the
    -- "indirect tag" icon white. Force the literal light-mode gray so the
    -- icon looks the same in both modes (matches View.Pathfinder.TagDetailsList).
    [ css [ Css.important (Css.property "fill" Colors.greyBlue300_string) ] ]


timeToCell : View.Config -> Int -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
timeToCell vc d =
    let
        t =
            Data.timestampToPosix d
    in
    { firstRowText = Locale.timestampDateUniform vc.locale t
    , secondRowText = Locale.timestampTimeUniform vc.locale vc.showTimeZoneOffset t
    , secondRowVisible = True
    }


emptyCell : { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
emptyCell =
    { firstRowText = ""
    , secondRowText = ""
    , secondRowVisible = False
    }


inputFieldStyles : Bool -> List Css.Style
inputFieldStyles hasError =
    if hasError then
        Fields.textFieldStateError_details.styles
            ++ Fields.textFieldStateErrorText_details.styles

    else
        Fields.textFieldStateDefault_details.styles
            ++ Fields.textFieldStateTypingText_details.styles
            ++ [ Css.pseudoClass "placeholder"
                    Fields.textFieldStateDefaultText_details.styles
               , Css.focus
                    (Fields.textFieldStateTyping_details.styles
                        |> List.map Css.important
                    )
               , Css.hover
                    Fields.textFieldStateHover_details.styles
               ]


ifTrue : Bool -> String -> String
ifTrue bool str =
    if bool then
        str

    else
        ""


type alias ValuesRow =
    { leftValue : ValuesFormatted
    , rightValue : ValuesFormatted
    }


type alias ValuesFormatted =
    { fiat : String
    , fiatFloat : Float
    , coin : String
    , value : Int
    , asset : AssetIdentifier
    }


makeValuesList : View.Config -> String -> Maybe Api.Data.NeighborAddress -> Maybe Api.Data.NeighborAddress -> List ValuesRow
makeValuesList vc network right left =
    let
        leftValues =
            left
                |> relationToValues

        rightValues =
            right
                |> relationToValues

        getValue ( asset, values ) =
            let
                fiatCurr =
                    vc.preferredFiatCurrency

                ass =
                    Currency.asset network asset

                coin =
                    Locale.coin vc.locale ass values.value

                fvalue =
                    Locale.getFiatValue fiatCurr values
                        |> Maybe.withDefault 0
            in
            { fiat =
                fvalue
                    |> Locale.fiat vc.locale fiatCurr
            , fiatFloat = fvalue
            , coin = coin
            , value = values.value
            , asset = ass
            }
                |> pair asset

        emptyValues asset =
            { fiat = Locale.fiat vc.locale vc.preferredFiatCurrency 0
            , fiatFloat = 0
            , coin = Locale.coin vc.locale (Currency.asset network asset) 0
            , value = 0
            , asset = Currency.asset network asset
            }

        relationToValues =
            Maybe.map
                (\{ value, tokenValues } ->
                    getValue ( network, value )
                        |> flip (::)
                            (tokenValues
                                |> Maybe.withDefault Dict.empty
                                |> Dict.toList
                                |> List.map getValue
                            )
                        |> Dict.fromList
                )
                >> Maybe.withDefault Dict.empty

        sort { rightValue, leftValue } =
            rightValue.fiatFloat + leftValue.fiatFloat

        leftStep asset values =
            Dict.insert
                asset
                { leftValue = values
                , rightValue = emptyValues asset
                }

        rightStep asset values =
            Dict.insert
                asset
                { leftValue = emptyValues asset
                , rightValue = values
                }

        bothStep asset lv rv =
            Dict.insert
                asset
                { leftValue = lv
                , rightValue = rv
                }
    in
    Dict.merge
        leftStep
        bothStep
        rightStep
        leftValues
        rightValues
        Dict.empty
        |> Dict.values
        |> List.sortBy sort
        |> List.reverse


conditionalHide : Bool -> List (Html.Styled.Attribute msg)
conditionalHide arg1 =
    if arg1 then
        [ css [ Css.display Css.none ] ]

    else
        []
