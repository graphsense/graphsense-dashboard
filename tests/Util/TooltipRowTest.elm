module Util.TooltipRowTest exposing (suite)

{-| A tooltip row keeps label and value apart. The generated row only spreads
them, so a label longer than its minimum width (German "Schlagwörter") ran
into the value.
-}

import Dict
import Html.Styled
import Support.Env as Env
import Support.MainApp as App
import Test exposing (Test, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Util.Tooltip
import Util.TooltipType exposing (TooltipType(..))


suite : Test
suite =
    test "tooltip rows have a gap between label and value" <|
        \_ ->
            let
                actor =
                    { categories = []
                    , context = Nothing
                    , id = "binance"
                    , jurisdictions = []
                    , label = "Binance"
                    , nrTags = Nothing
                    , uri = "binance.com"
                    }

                pathfinder =
                    App.initAt "/pathfinder" |> App.model |> .pathfinder
            in
            Util.Tooltip.view Env.viewConfig
                { pathfinder | actors = Dict.insert actor.id actor pathfinder.actors }
                (ActorDetails actor.id)
                |> Html.Styled.div []
                |> Html.Styled.toUnstyled
                |> Query.fromHtml
                |> Query.has [ Selector.containing [ Selector.text "gap:16px" ] ]
