module Init.Pathfinder.Table.TagsTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import RecordSetter as Rs


init : Api.Data.AddressTags -> Table Api.Data.AddressTag
init data =
    let
        tags =
            data.addressTags

        sdata =
            tags
                |> List.sortBy (.confidenceLevel >> Maybe.withDefault 0)
                |> List.reverse
    in
    Init.Graph.Table.initSorted False "confidenceLevel"
        |> Rs.s_data sdata
        |> Rs.s_filtered sdata
        |> Rs.s_loading False
        |> Rs.s_nextpage data.nextPage
