module Config.Update exposing (Config, networkConfirmedServed, networkServed)

import Api.Data
import Model.Graph.Coords exposing (BBox)
import Model.Locale as Locale


type alias Config =
    { locale : Locale.Model
    , size : Maybe BBox -- position and size of the main pane
    , allConcepts : List Api.Data.Concept
    , abuseConcepts : List Api.Data.Concept
    , networks : List String -- what the backend serves right now (statistics); empty until they arrive
    , preferredFiatCurrency : String -- for the on-graph value filter
    }


{-| Same rule as `Config.View.networkServed`: a network missing from the
statistics is switched off (lite-networks setting) or gated for this account.
Empty means the statistics have not arrived yet, so nothing is rejected.
-}
networkServed : Config -> String -> Bool
networkServed uc network =
    List.isEmpty uc.networks
        || List.any (\n -> String.toLower n == String.toLower network) uc.networks


{-| Stricter than `networkServed`: only a network the statistics list. Before
they arrive nothing counts as served. For a choice that is not revisited when
the statistics come in, such as drawing a bridge row as an edge to a network
the graph may not be able to load.
-}
networkConfirmedServed : Config -> String -> Bool
networkConfirmedServed uc network =
    not (List.isEmpty uc.networks) && networkServed uc network
