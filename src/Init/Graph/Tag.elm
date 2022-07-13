module Init.Graph.Tag exposing (..)

import Browser.Dom as Dom
import Init.Search as Search
import Model.Graph.Id as Id exposing (..)
import Model.Graph.Tag exposing (..)
import Model.Node exposing (Node(..))
import RecordSetter exposing (..)


initAddressTag : AddressId -> Dom.Element -> Maybe UserTag -> Model
initAddressTag id element existing =
    { input = initInput (Address id) existing
    , hovercardElement = element
    }


initEntityTag : EntityId -> Dom.Element -> Maybe UserTag -> Model
initEntityTag id element existing =
    { input = initInput (Entity id) existing
    , hovercardElement = element
    }


initInput : Node AddressId EntityId -> Maybe UserTag -> Input
initInput id existing =
    { label =
        Search.init
            |> s_input (Maybe.map .label existing |> Maybe.withDefault "")
    , source = Maybe.map .source existing |> Maybe.withDefault ""
    , category = Maybe.andThen .category existing |> Maybe.withDefault ""
    , abuse = Maybe.andThen .abuse existing |> Maybe.withDefault ""
    , id = id
    }
