module Update.Graph.Table exposing (..)

import Model.Graph.Table exposing (..)


appendData : Maybe String -> List a -> Table a -> Table a
appendData nextpage data table =
    { table
        | data = table.data ++ data
        , nextpage = nextpage
        , loading = False
    }
