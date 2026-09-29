module Config.Update exposing (Config, networkServed)

import Api.Data
import Model.Graph.Coords exposing (BBox)
import Model.Locale as Locale


type alias Config =
    { locale : Locale.Model
    , size : Maybe BBox -- position and size of the main pane
    , allConcepts : List Api.Data.Concept
    , abuseConcepts : List Api.Data.Concept
    , networks : List String -- what the backend serves right now (statistics); empty until they arrive
    }


{-| Same rule as `Config.View.networkServed`: a network missing from the
statistics is switched off (lite-networks setting) or gated for this account.
Empty means the statistics have not arrived yet, so nothing is rejected.
-}
networkServed : Config -> String -> Bool
networkServed uc network =
    List.isEmpty uc.networks
        || List.any (\n -> String.toLower n == String.toLower network) uc.networks
