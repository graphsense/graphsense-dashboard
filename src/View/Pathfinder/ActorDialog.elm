module View.Pathfinder.ActorDialog exposing (view)

{-| What the API knows about an actor: what the actor tooltip shows (name,
website, a few jurisdictions) and what it has no room for (all jurisdictions,
categories, the number of tags, social media and GitHub). The context's
references and CoinGecko/DefiLlama ids are left out on purpose: they are
sources for the actor record, not something an analyst follows. Opened by a click on an actor, the "learn more" link in the actor
tooltip, or an actor search result.

Built from the generic dialog and the existing chip and row text styles until
the dialog has a design of its own in Figma.

-}

import Api.Data exposing (Actor, LabeledItemRef)
import Config.View as View
import Css
import Html.Styled exposing (Html, a, div, img, text)
import Html.Styled.Attributes exposing (css, href, src, target, title)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.Dialogs as Dialogs
import Theme.Html.GraphComponents as GraphComponents
import Theme.Html.TagsComponents as TagsComponents
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.View exposing (none, pointer, testId)
import View.Locale as Locale


view : msg -> Actor -> View.Config -> Html msg
view closeMsg actor vc =
    let
        legalName =
            actor.context |> Maybe.andThen .legalName

        logo =
            actor.context
                |> Maybe.andThen (.images >> List.head)
                |> Maybe.map
                    (\image ->
                        img
                            [ src (addProtocolPrefx image)
                            , css [ Css.width (Css.px 24), Css.height (Css.px 24), Css.property "object-fit" "contain" ]
                            ]
                            []
                    )
    in
    Dialogs.dialogGenericWithAttributes
        (Dialogs.dialogGenericAttributes
            -- the generated dialog is 418px with a 386px header, too narrow for
            -- two dozen jurisdictions and long reference URLs; overriding
            -- generated properties needs `important` (see CLAUDE.md)
            |> Rs.s_root
                [ testId "gs-actor-dialog"
                , css [ Css.width (Css.px 560) |> Css.important, Css.maxWidth (Css.vw 90) ]
                ]
            |> Rs.s_dialogHeader [ css [ Css.width (Css.pct 100) |> Css.important ] ]
            |> Rs.s_iconsCloseNoPadding [ pointer, onClick closeMsg ]
            |> Rs.s_descriptionFrame
                (if legalName == Nothing then
                    [ css [ Css.display Css.none ] ]

                 else
                    []
                )
            |> Rs.s_buttonsLayout [ css [ Css.display Css.none ] ]
        )
        { inputList =
            [ div
                [ css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.property "gap" "12px"
                    , Css.width (Css.pct 100)
                    , Css.minWidth Css.zero
                    ]
                ]
                (rows vc actor)
            ]
        }
        { cancelButton = { variant = none }
        , confirmButton = { variant = none }
        , root = {}
        , dialogHeader =
            { showIconsFrame = logo /= Nothing
            , header = actor.label
            , description = legalName |> Maybe.withDefault ""
            , icon = logo |> Maybe.withDefault none
            }
        }


rows : View.Config -> Actor -> List (Html msg)
rows vc actor =
    let
        context =
            actor.context

        -- context values arrive blank (GitHub "") or several in one string
        -- (Twitter "BinanceUS,binance,binancezh")
        fromContext field =
            context
                |> Maybe.map field
                |> Maybe.withDefault []
                |> List.concatMap (String.split ",")
                |> List.map String.trim
                |> List.filter (String.isEmpty >> not)

        fromContextMaybe field =
            fromContext (field >> Maybe.map List.singleton >> Maybe.withDefault [])

        websites =
            (actor.uri :: fromContext .uris)
                |> List.filter (String.isEmpty >> not)
                |> List.map (\uri -> ( uri, addProtocolPrefx uri ))
                |> uniqueByUrl

        handleOrUrl base handle =
            ( handle
            , if String.startsWith "http" handle then
                handle

              else
                base ++ handle
            )
    in
    [ row (Locale.string vc.locale "Website") (links websites)
    , row (Locale.string vc.locale "Categories") (chips vc actor.categories)
    , row (Locale.string vc.locale "Jurisdictions") (chips vc actor.jurisdictions)
    , row (Locale.string vc.locale "Tags")
        (actor.nrTags
            |> Maybe.map (Locale.int vc.locale >> text >> List.singleton >> valueText)
            |> Maybe.withDefault []
        )
    , row (Locale.string vc.locale "Social media") (fromContextMaybe .twitterHandle |> List.map (handleOrUrl "https://x.com/") |> links)
    , row "GitHub" (fromContextMaybe .githubOrganisation |> List.map (handleOrUrl "https://github.com/") |> links)
    ]
        |> List.filterMap identity


uniqueByUrl : List ( String, String ) -> List ( String, String )
uniqueByUrl =
    List.foldr
        (\( lbl, url ) acc ->
            if List.any (Tuple.second >> (==) url) acc then
                acc

            else
                ( lbl, url ) :: acc
        )
        []


{-| A row titled `label` (already translated); Nothing (left out) when there
is nothing to show.
-}
row : String -> List (Html msg) -> Maybe (Html msg)
row label content =
    if List.isEmpty content then
        Nothing

    else
        div
            [ css
                [ Css.displayFlex
                , Css.property "gap" "16px"
                , Css.alignItems Css.flexStart
                ]
            ]
            [ div
                [ css
                    (GraphComponents.tooltipRowLabelTheLabel_details.styles
                        ++ [ Css.minWidth (Css.px 110), Css.flexShrink (Css.num 0) ]
                    )
                ]
                [ text label ]
            , div
                [ css
                    [ Css.displayFlex
                    , Css.flexWrap Css.wrap
                    , Css.property "gap" "4px 8px"
                    , Css.minWidth Css.zero
                    , Css.flexGrow (Css.num 1)
                    ]
                ]
                content
            ]
            |> Just


valueText : List (Html msg) -> List (Html msg)
valueText content =
    [ div
        [ css
            (GraphComponents.tooltipRowValueFirstValue_details.styles
                ++ [ Css.whiteSpace Css.normal ]
            )
        ]
        content
    ]


chips : View.Config -> List LabeledItemRef -> List (Html msg)
chips vc =
    List.map
        (\item ->
            TagsComponents.categoryTag
                { root =
                    { tagLabel = Locale.string vc.locale item.label
                    , closeVisible = False
                    }
                }
        )


links : List ( String, String ) -> List (Html msg)
links =
    List.map
        (\( lbl, url ) ->
            a
                [ href url
                , target "_blank"
                , title url
                , css
                    (GraphComponents.tooltipRowValueFirstValue_details.styles
                        ++ [ Css.property "color" Colors.blue600
                           , Css.textDecoration Css.none
                           , Css.hover [ Css.textDecoration Css.underline ]

                           -- one line, cut with an ellipsis; the full URL is the title
                           , Css.display Css.block
                           , Css.maxWidth (Css.pct 100)
                           , Css.overflow Css.hidden
                           , Css.textOverflow Css.ellipsis
                           , Css.flexShrink (Css.num 0)
                           ]
                    )
                ]
                [ text lbl ]
        )
