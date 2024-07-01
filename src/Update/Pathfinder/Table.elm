module Update.Pathfinder.Table exposing (appendData)

import Model.Graph.Table as GT
import Model.Pathfinder.Table as PT
import RecordSetter exposing (s_loading, s_nextpage)
import Update.Graph.Table as GT


appendData : PT.PagedTable p -> GT.Filter p -> Maybe String -> List p -> PT.PagedTable p
appendData pt f nextPage data =
    { pt
        | table =
            GT.appendData f data pt.table
                |> s_nextpage nextPage
                |> s_loading False
    }
