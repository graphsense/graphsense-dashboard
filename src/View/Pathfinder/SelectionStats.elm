module View.Pathfinder.SelectionStats exposing (view)

{-| The side panel for a multi-selection (backlog B006): what the selected
addresses hold and moved, and how money flows between them on the graph.

Drawn here (frame, headings, rows) until the panel has a Figma component of
its own: the generated side-panel rows assume the address panel's layout.

-}

import Config.View as View
import Css
import Html.Styled exposing (Html, div, span, text)
import Html.Styled.Attributes exposing (css, title)
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.Selection exposing (MultiSelectOptions)
import Model.Pathfinder.SelectionStats as SelectionStats
import Msg.Pathfinder exposing (Msg(..))
import Theme.Colors as Colors
import Theme.Html.Icons as Icons
import Time
import Util.View exposing (onClickWithStop, testId)
import View.Locale as Locale


view : View.Config -> Pathfinder.Model -> List MultiSelectOptions -> Html Msg
view vc model selection =
    let
        stats =
            SelectionStats.compute vc.preferredFiatCurrency selection model.network

        money =
            Locale.fiat vc.locale vc.preferredFiatCurrency

        date =
            Maybe.map ((*) 1000 >> Time.millisToPosix >> Locale.timestampDateUniform vc.locale)
                >> Maybe.withDefault "–"

        row label value =
            div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.spaceBetween
                    , Css.property "gap" "12px"
                    , Css.padding2 (Css.px 3) Css.zero
                    , Css.fontSize (Css.px 13)
                    ]
                ]
                [ span [ css [ Css.property "color" Colors.greyBlue700 ] ] [ text label ]
                , span [ css [ Css.fontWeight (Css.int 500), Css.textAlign Css.right ] ] [ text value ]
                ]

        heading label =
            div
                [ css
                    [ Css.fontSize (Css.px 14)
                    , Css.fontWeight (Css.int 600)
                    , Css.marginTop (Css.px 12)
                    , Css.marginBottom (Css.px 4)
                    ]
                ]
                [ text label ]
    in
    div
        [ testId "gs-selection-stats"
        , css
            [ Css.width (Css.px 360)
            , Css.padding (Css.px 16)
            , Css.boxSizing Css.borderBox
            , Css.borderRadius (Css.px 8)
            , Css.property "background-color" Colors.white
            , Css.property "box-shadow" ("0 2px 12px " ++ Colors.greyShadow)
            , Css.property "color" Colors.greyBlue900
            , Css.marginRight (Css.px 16)
            ]
        ]
        ([ div [ css [ Css.displayFlex, Css.alignItems Css.flexStart ] ]
            [ div [ css [ Css.flexGrow (Css.num 1), Css.fontSize (Css.px 20), Css.fontWeight (Css.int 600) ] ]
                [ text (Locale.interpolated vc.locale "{0} selected" [ String.fromInt (stats.addresses + stats.txs) ]) ]
            , span
                [ testId "gs-selection-stats-close"
                , title (Locale.string vc.locale "Close")
                , css [ Css.cursor Css.pointer, Css.displayFlex ]
                , onClickWithStop UserClosedDetailsView
                ]
                [ Icons.iconsCloseBlack {} ]
            ]
         , div [ css [ Css.fontSize (Css.px 12), Css.property "color" Colors.greyBlue500, Css.marginTop (Css.px 2) ] ]
            [ text
                (Locale.interpolated vc.locale
                    "{0} addresses · {1} transactions · {2} clusters"
                    [ String.fromInt stats.addresses, String.fromInt stats.txs, String.fromInt stats.clusters ]
                )
            ]
         , heading (Locale.string vc.locale "On this graph")
         , row (Locale.string vc.locale "Inflow from outside") (money stats.inflow)
         , row (Locale.string vc.locale "Outflow to outside") (money stats.outflow)
         , row (Locale.string vc.locale "Between selected") (money stats.between)
         ]
            ++ (case stats.txsBetween of
                    Just n ->
                        [ row (Locale.string vc.locale "Transactions between the two") (String.fromInt n) ]

                    Nothing ->
                        []
               )
            ++ [ heading (Locale.string vc.locale "Selected addresses, all time")
               , row (Locale.string vc.locale "Total received") (money stats.received)
               , row (Locale.string vc.locale "Total sent") (money stats.sent)
               , row (Locale.string vc.locale "Balance") (money stats.balance)
               , row (Locale.string vc.locale "First usage") (date stats.firstActivity)
               , row (Locale.string vc.locale "Last usage") (date stats.lastActivity)
               , div [ css [ Css.fontSize (Css.px 11), Css.property "color" Colors.greyBlue500, Css.marginTop (Css.px 10) ] ]
                    [ Locale.text vc.locale "Flows count only the transactions on the graph." ]
               ]
        )
