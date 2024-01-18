module Init.Graph.Tag exposing (..)

import Browser.Dom as Dom
import Hovercard
import Init.Search as Search
import Model.Graph.Id exposing (..)
import Model.Graph.Tag exposing (..)
import Model.Node exposing (Node(..))
import Model.Search as Search
import Msg.Graph exposing (Msg(..))
import Tuple exposing (mapSecond)


initAddressTag : AddressId -> Maybe UserTag -> ( Model, Cmd Msg )
initAddressTag id existing =
    let
        ( hovercard, cmd ) =
            addressIdToString id
                |> Hovercard.init
                |> mapSecond (Cmd.map TagHovercardMsg)
    in
    ( { input = initInput (Address id) existing
      , existing = existing
      , hovercard = hovercard
      }
    , cmd
    )


initEntityTag : EntityId -> Maybe UserTag -> ( Model, Cmd Msg )
initEntityTag id existing =
    let
        ( hovercard, cmd ) =
            entityIdToString id
                |> Hovercard.init
                |> mapSecond (Cmd.map TagHovercardMsg)
    in
    ( { input = initInput (Entity id) existing
      , existing = existing
      , hovercard = hovercard
      }
    , cmd
    )


initInput : Node AddressId EntityId -> Maybe UserTag -> Input
initInput id existing =
    { label =
        Search.init Search.SearchTagsOnly
            |> Search.setQuery (Maybe.map .label existing |> Maybe.withDefault "")
    , source = Maybe.map .source existing |> Maybe.withDefault ""
    , category = Maybe.andThen .category existing |> Maybe.withDefault ""
    , abuse = Maybe.andThen .abuse existing |> Maybe.withDefault ""
    , id = id
    }
