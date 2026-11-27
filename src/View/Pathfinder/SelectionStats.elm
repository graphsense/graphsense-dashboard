module View.Pathfinder.SelectionStats exposing (view)

{-| The side panel for a multi-selection (backlog B006): what the selected
addresses hold and moved, and how money flows between them on the graph.

Drawn here (frame, headings, rows) until the panel has a Figma component of
its own: the generated side-panel rows assume the address panel's layout.

-}

import Config.View as View
import Css
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css, title)
import Model.Pathfinder as Pathfinder
import Model.Pathfinder.Selection exposing (MultiSelectOptions)
import Model.Pathfinder.SelectionStats as SelectionStats
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter as Rs
import Theme.Html.GraphFilters as GraphFilters
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
            { label = label, value = value }
    in
    GraphFilters.multiSelectionPanelWithAttributes
        (GraphFilters.multiSelectionPanelAttributes
            |> Rs.s_root [ testId "gs-selection-stats", css [ Css.marginRight (Css.px 16) ] ]
            |> Rs.s_close
                [ testId "gs-selection-stats-close"
                , title (Locale.string vc.locale "Close")
                , css [ Css.cursor Css.pointer ]
                , onClickWithStop UserClosedDetailsView
                ]
        )
        { root =
            { title = Locale.interpolated vc.locale "{0} selected" [ String.fromInt (stats.addresses + stats.txs) ]
            , summary =
                Locale.interpolated vc.locale
                    "{0} addresses · {1} transactions · {2} clusters"
                    [ String.fromInt stats.addresses, String.fromInt stats.txs, String.fromInt stats.clusters ]
            , graphHeading = Locale.string vc.locale "On this graph"
            , allTimeHeading = Locale.string vc.locale "Selected addresses, all time"
            , footnote = Locale.string vc.locale "Flows count only the transactions on the graph."
            , txsBetweenVisible = stats.txsBetween /= Nothing
            }
        , inflow = row (Locale.string vc.locale "Inflow from outside") (money stats.inflow)
        , outflow = row (Locale.string vc.locale "Outflow to outside") (money stats.outflow)
        , between = row (Locale.string vc.locale "Between selected") (money stats.between)
        , txsBetween =
            row (Locale.string vc.locale "Transactions between the two")
                (stats.txsBetween |> Maybe.map String.fromInt |> Maybe.withDefault "")
        , received = row (Locale.string vc.locale "Total received") (money stats.received)
        , sent = row (Locale.string vc.locale "Total sent") (money stats.sent)
        , balance = row (Locale.string vc.locale "Balance") (money stats.balance)
        , firstUsage = row (Locale.string vc.locale "First usage") (date stats.firstActivity)
        , lastUsage = row (Locale.string vc.locale "Last usage") (date stats.lastActivity)
        }
