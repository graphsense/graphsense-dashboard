module View.Pathfinder.ActorDialog exposing (view)

{-| What the API knows about an actor: what the actor tooltip shows (name,
website, a few jurisdictions) and what it has no room for (all jurisdictions,
categories, the number of tags, social media and GitHub). The context's
references and CoinGecko/DefiLlama ids are left out on purpose: they are
sources for the actor record, not something an analyst follows.

Opened by a click on an actor, the "learn more" link in the actor tooltip, or
an actor search result.

The layout is the Figma component `Dialog/Actor`. Each of its rows has one
sample in its value area (`<field> value`); this module replaces the whole
value area with the actor's chips or links, styled like the generated one, and
hides a row the actor has no data for. Keep one sample per value area in
Figma: with several, the generator numbers them (`categoryTag13`, …) and the
numbers change with every edit.

-}

import Api.Data exposing (Actor, LabeledItemRef)
import Config.View as View
import Css
import Html.Styled exposing (Html, a, div, img, text)
import Html.Styled.Attributes exposing (css, href, src, target, title)
import Html.Styled.Events exposing (onClick)
import Maybe.Extra
import RecordSetter as Rs
import Theme.Html.Dialogs as Dialogs
import Theme.Html.TagsComponents as TagsComponents
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.View exposing (none, pointer, testId)
import View.Locale as Locale


view : { closeMsg : msg, showTagsMsg : msg } -> Actor -> View.Config -> Html msg
view { closeMsg, showTagsMsg } actor vc =
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

        handleOrUrl base handle =
            if String.startsWith "http" handle then
                handle

            else
                base ++ handle

        -- "binance", or the last path segment if the API sends a URL
        handleOf handle =
            if String.startsWith "http" handle then
                handle
                    |> String.split "/"
                    |> List.filter (String.isEmpty >> not)
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault handle

            else
                handle

        legalName =
            context |> Maybe.andThen .legalName

        logo =
            context
                |> Maybe.andThen (.images >> List.head)
                |> Maybe.map
                    (\image ->
                        img
                            [ src (addProtocolPrefx image)
                            , css [ Css.width (Css.px 24), Css.height (Css.px 24), Css.property "object-fit" "contain" ]
                            ]
                            []
                    )

        websites =
            (actor.uri :: fromContext .uris)
                |> List.filter (String.isEmpty >> not)
                |> List.map (\uri -> ( uri, addProtocolPrefx uri ))
                |> uniqueByUrl
                |> links

        socialMedia =
            fromContextMaybe .twitterHandle
                -- the platform, then the account: an actor can have several
                -- (Binance: BinanceUS, binance, binancezh)
                |> List.map (\handle -> ( "X.com (" ++ handleOf handle ++ ")", handleOrUrl "https://x.com/" handle ))
                |> links

        github =
            fromContextMaybe .githubOrganisation
                |> List.map (\org -> ( org, handleOrUrl "https://github.com/" org ))
                |> links

        tagsCount =
            actor.nrTags
                |> Maybe.map (Locale.int vc.locale >> text >> List.singleton >> div [ css Dialogs.dialogActorTagsCount_details.styles ])

        label details key =
            div [ css details.styles ] [ text (Locale.string vc.locale key) ]
                |> Just

        value details content =
            div [ css details.styles ] content
                |> Just

        -- Nothing keeps the generated row; Just none drops it
        rowUnless content =
            if List.isEmpty content then
                Just none

            else
                Nothing
    in
    Dialogs.dialogActorWithInstances
        (Dialogs.dialogActorAttributes
            |> Rs.s_root [ testId "gs-actor-dialog" ]
            |> Rs.s_iconsCloseNoPadding [ pointer, onClick closeMsg ]
            -- "Show tags": the actor's tags list
            |> Rs.s_dialogLinkOfTagsValue
                [ onClick showTagsMsg
                , pointer
                , css [ Css.hover [ Css.textDecoration Css.underline ] ]
                , testId "gs-actor-show-tags"
                ]
            |> Rs.s_descriptionFrame
                (if legalName == Nothing then
                    [ css [ Css.display Css.none ] ]

                 else
                    []
                )
        )
        (Dialogs.dialogActorInstances
            |> Rs.s_websiteRow (rowUnless websites)
            |> Rs.s_websiteLabel (label Dialogs.dialogActorWebsiteLabel_details "Website")
            |> Rs.s_websiteValue (value Dialogs.dialogActorWebsiteValue_details websites)
            |> Rs.s_categoriesRow (rowUnless actor.categories)
            |> Rs.s_categoriesLabel (label Dialogs.dialogActorCategoriesLabel_details "Categories")
            |> Rs.s_categoriesValue (value Dialogs.dialogActorCategoriesValue_details (chips vc actor.categories))
            |> Rs.s_jurisdictionsRow (rowUnless actor.jurisdictions)
            |> Rs.s_jurisdictionsLabel (label Dialogs.dialogActorJurisdictionsLabel_details "Jurisdictions")
            |> Rs.s_jurisdictionsValue (value Dialogs.dialogActorJurisdictionsValue_details (chips vc actor.jurisdictions))
            |> Rs.s_tagsRow (rowUnless (Maybe.Extra.toList tagsCount))
            |> Rs.s_tagsLabel (label Dialogs.dialogActorTagsLabel_details "Tags")
            |> Rs.s_tagsCount tagsCount
            |> Rs.s_socialMediaRow (rowUnless socialMedia)
            |> Rs.s_socialMediaLabel (label Dialogs.dialogActorSocialMediaLabel_details "Social media")
            |> Rs.s_socialMediaValue (value Dialogs.dialogActorSocialMediaValue_details socialMedia)
            |> Rs.s_githubRow (rowUnless github)
            |> Rs.s_githubValue (value Dialogs.dialogActorGithubValue_details github)
        )
        { dialogHeader =
            { showIconsFrame = logo /= Nothing
            , header = actor.label
            , description = legalName |> Maybe.withDefault ""
            , icon = logo |> Maybe.withDefault none
            }

        -- the samples' own properties; their value areas are replaced above
        , categoryTagOfCategoriesValue = { tagLabel = "", closeVisible = False }
        , categoryTagOfJurisdictionsValue = { tagLabel = "", closeVisible = False }
        , dialogLinkOfGithubValue = { linkText = "" }
        , dialogLinkOfSocialMediaValue = { linkText = "" }
        , dialogLinkOfWebsiteValue = { linkText = "" }
        , dialogLinkOfTagsValue = { linkText = Locale.string vc.locale "Show tags" }
        }


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


{-| `Dialog/Link` from Figma, made a link: the component draws the text, the
anchor around it carries the URL (also its title, as the text may be a
platform name such as "X.com").
-}
links : List ( String, String ) -> List (Html msg)
links =
    List.map
        (\( lbl, url ) ->
            a
                [ href url
                , target "_blank"
                , title url
                , css
                    [ Css.textDecoration Css.none
                    , Css.hover [ Css.textDecoration Css.underline ]
                    , Css.property "color" "inherit"
                    ]
                ]
                [ Dialogs.dialogLink { root = { linkText = lbl } } ]
        )
