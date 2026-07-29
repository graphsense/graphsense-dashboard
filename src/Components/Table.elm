module Components.Table exposing (Filter, Table, UpdateSearchTerm(..), appendData, filterTable, initUnsorted, setData, sortBy)

import Table


type alias Table a =
    { data : List a
    , filtered : List a
    , loading : Bool
    , state : Table.State
    , nextpage : Maybe String
    , searchTerm : Maybe String
    }


type alias Filter a =
    { search : String -> a -> Bool
    , filter : a -> Bool
    }


initUnsorted : Table a
initUnsorted =
    { data = []
    , filtered = []
    , loading = False
    , state = Table.initialSort ""
    , nextpage = Nothing
    , searchTerm = Nothing
    }


appendData : Filter a -> List a -> Table a -> Table a
appendData config data table =
    { table
        | data = table.data ++ data
        , filtered =
            table.filtered
                ++ filterTheData config table data
        , loading = False
    }


filterTheData : Filter a -> Table a -> List a -> List a
filterTheData { search, filter } table data =
    let
        d =
            List.filter filter data
    in
    table.searchTerm
        |> Maybe.map (\term -> List.filter (search term) d)
        |> Maybe.withDefault d


setData : Filter a -> List a -> Table a -> Table a
setData config data table =
    { table
        | data = data
        , filtered = filterTheData config table data
        , loading = False
    }


type UpdateSearchTerm
    = Update (Maybe String)
    | Keep


filterTable : (a -> Bool) -> Table a -> Table a
filterTable filter table =
    { table
        | data = List.filter filter table.data
        , filtered = List.filter filter table.filtered
    }


sortBy : String -> Bool -> Table d -> Table d
sortBy col asc table =
    { table | state = Table.sortBy col asc }
