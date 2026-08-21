module View.Graph.Browser exposing (properties, propertyBox)

{-| Slim, reusable property-box renderer.

The old `/graph` UI has been removed; this module retains only the generic
property-box rendering primitives that are reused as shared UI (e.g. by
plugins). It no longer renders the graph browser itself. See
`Model.Graph.Browser` for the data types.

-}

import Config.View as View
import Css as CssStyled
import Css.Browser as Css
import Css.View as CssView
import FontAwesome
import FontAwesome.Layers as FontAwesome
import Html.Attributes
import Html.Styled as Html exposing (Html, a, div, hr, img, input, li, object, option, select, span, text, ul)
import Html.Styled.Attributes exposing (attribute, css, href, id, selected, src, target, title)
import Html.Styled.Events exposing (on, onBlur, onInput)
import Json.Decode as JD
import List.Extra
import Model.Graph.Browser exposing (Row(..), TableLink, Value(..))
import Model.Graph.Coords exposing (Coords)
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Flags exposing (getFlagEmoji)
import Util.Graph
import Util.View exposing (copyableLongIdentifier, none)
import View.Locale as Locale


propertyBox : View.Config -> List (Html msg) -> Html msg
propertyBox vc =
    div
        [ Css.propertyBoxTable vc |> css
        , id "propertyBox"
        ]
        >> List.singleton
        >> div [ Css.propertyBoxRoot vc |> css ]


rule : View.Config -> Html msg
rule vc =
    hr [ Css.propertyBoxRule vc |> css ] []


tableLink : View.Config -> TableLink -> Html msg
tableLink vc link =
    a
        [ Css.propertyBoxTableLink vc link.active |> css
        , href link.link
        , title link.title
        ]
        [ FontAwesome.IconLayer FontAwesome.ellipsisH FontAwesome.Solid [] []
            |> propertyBoxButton link.active
        ]


propertyBoxButton : Bool -> FontAwesome.IconLayer msg -> Html msg
propertyBoxButton active iconlayer =
    FontAwesome.layers
        [ iconlayer
        , FontAwesome.IconLayer FontAwesome.caretRight
            FontAwesome.Solid
            [ FontAwesome.Pull FontAwesome.Right ]
            [ Html.Attributes.style "opacity" <|
                if active then
                    "1"

                else
                    "0"
            ]
        ]
        []
        |> Html.fromUnstyled


properties : View.Config -> List (Row (Value msg) Coords msg) -> List (Html msg)
properties vc =
    List.map (browseRow vc (browseValue vc))


browseRow : View.Config -> (r -> Html msg) -> Row r Coords msg -> Html msg
browseRow vc map row =
    case row of
        Rule ->
            rule vc

        Image muri ->
            div
                [ Css.propertyBoxRow vc False |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    []
                , span
                    []
                    [ case muri of
                        Just uri ->
                            let
                                uriWithPrefix =
                                    addProtocolPrefx uri
                            in
                            object [ attribute "data" uriWithPrefix, Css.propertyBoxImage vc |> css ]
                                [ img [ src vc.theme.userDefautImgUrl, Css.propertyBoxImage vc |> css ] []
                                ]

                        Nothing ->
                            img [ src vc.theme.userDefautImgUrl, Css.propertyBoxImage vc |> css ] []
                    ]
                ]

        Note note ->
            div
                [ Css.propertyBoxRow vc False |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    []
                , span
                    []
                    [ FontAwesome.exclamationTriangle
                        |> FontAwesome.icon
                        |> Html.fromUnstyled
                    , span
                        [ Css.propertyBoxNote vc |> css
                        ]
                        [ text note
                        ]
                    ]
                ]

        Footnote note ->
            div
                [ Css.propertyBoxRow vc False |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    []
                , span
                    [ (Css.propertyBoxNote vc ++ [ CssStyled.fontSize <| CssStyled.em 0.8 ]) |> css ]
                    [ div [ [ CssStyled.textAlign CssStyled.right ] |> css ] [ text note ]
                    ]
                ]

        Row ( key, value, table ) ->
            div
                [ table |> Maybe.map .active |> Maybe.withDefault False |> Css.propertyBoxRow vc |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    [ Locale.text vc.locale key
                    ]
                , span
                    []
                    [ div
                        [ Css.propertyBoxValueInner vc |> css
                        ]
                        [ map value
                        , table
                            |> Maybe.map (tableLink vc)
                            |> Maybe.withDefault none
                        ]
                    ]
                ]

        RowWithMoreActionsButton ( key, value, msg ) ->
            div
                [ Css.propertyBoxRow vc False |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    [ Locale.text vc.locale key
                    ]
                , span
                    []
                    [ div
                        [ Css.propertyBoxValueInner vc |> css
                        ]
                        [ map value
                        , msg
                            |> Maybe.map
                                (\vmsg ->
                                    div
                                        [ Locale.string vc.locale "more actions" |> title
                                        , on "click" (Util.Graph.decodeCoords Coords |> JD.map vmsg)
                                        , Css.propertyBoxTableLink vc False |> css
                                        ]
                                        [ FontAwesome.IconLayer FontAwesome.caretSquareDown FontAwesome.Solid [] []
                                            |> propertyBoxButton False
                                        ]
                                )
                            |> Maybe.withDefault (div [] [])
                        ]
                    ]
                ]

        OptionalRow optionalRow bool ->
            if bool then
                browseRow vc map optionalRow

            else
                span [] []


browseValue : View.Config -> Value msg -> Html msg
browseValue vc value =
    case value of
        Stack values ->
            ul [] (List.map (\val -> li [] [ browseValue vc val ]) values)

        Grid width values ->
            let
                gvalues =
                    List.Extra.greedyGroupsOf width values

                viewRow vrow =
                    li [] [ List.map (browseValue vc) vrow |> span [] ]
            in
            ul [] (List.map viewRow gvalues)

        String str ->
            div [ css [ CssStyled.minHeight <| CssStyled.em 1 ] ]
                [ text str ]

        HashStr str ->
            copyableLongIdentifier vc [ css [ CssStyled.minHeight <| CssStyled.em 1 ] ] str

        AddressStr str ->
            copyableLongIdentifier vc [ css [ CssStyled.minHeight <| CssStyled.em 1 ] ] str

        Country isocode name ->
            span [ css [ CssStyled.minHeight <| CssStyled.em 1, CssStyled.paddingRight <| CssStyled.em 1 ], title name ]
                [ span [ css [ CssStyled.fontSize <| CssStyled.em 1.2, CssStyled.marginRight <| CssStyled.em 0.2 ] ] [ getFlagEmoji isocode |> text ]
                , span [ css [ CssStyled.fontFamily <| CssStyled.monospace ] ] [ text isocode ]
                ]

        Uri lbl uri ->
            a [ href (addProtocolPrefx uri), target "_blank", CssView.link vc |> css ]
                [ text lbl ]

        IconLink icon uri ->
            a [ href (addProtocolPrefx uri), target "_blank", CssView.iconLink vc |> css ]
                [ FontAwesome.icon icon |> Html.fromUnstyled ]

        InternalLink lbl uri ->
            a [ href uri, CssView.link vc |> css ]
                [ text lbl ]

        Html html ->
            html

        Input msg blur current ->
            input
                [ Html.Styled.Attributes.value current
                , onInput msg
                , onBlur blur
                , CssView.input vc |> css
                ]
                []

        Select options msg current ->
            options
                |> List.map
                    (\( key, ttl ) ->
                        option
                            [ Html.Styled.Attributes.value key
                            , current == key |> selected
                            ]
                            [ Locale.string vc.locale ttl
                                |> text
                            ]
                    )
                |> select
                    [ CssView.input vc |> css
                    , onInput msg
                    ]
