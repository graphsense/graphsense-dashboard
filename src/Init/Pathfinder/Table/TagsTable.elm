module Init.Pathfinder.Table.TagsTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import RecordSetter as Rs


init : List Api.Data.AddressTag -> Table Api.Data.AddressTag
init data =
    Init.Graph.Table.initUnsorted
        |> Rs.s_data data
        |> Rs.s_filtered data
        |> Rs.s_loading False
