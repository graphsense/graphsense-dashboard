module Init.Graph.Tag exposing (..)

import Browser.Dom as Dom
import Init.Search as Search
import Model.Graph.Id exposing (..)
import Model.Graph.Tag exposing (..)
import RecordSetter exposing (..)


init : AddressId -> Dom.Element -> Maybe UserTag -> Model
init id element existing =
    { input = initInput id existing
    , hovercardElement = element
    }


initInput : AddressId -> Maybe UserTag -> Input
initInput id existing =
    { label =
        Search.init
            |> s_input (Maybe.map .label existing |> Maybe.withDefault "")
    , source = Maybe.map .source existing |> Maybe.withDefault ""
    , category = Maybe.andThen .category existing |> Maybe.withDefault ""
    , abuse = Maybe.andThen .abuse existing |> Maybe.withDefault ""
    , id = id
    }
