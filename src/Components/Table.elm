module Components.Table exposing (Filter, Table, UpdateSearchTerm(..), appendData, asCsv, filterData, filterTable, filterTheData, init, initSorted, initUnsorted, searchData, setData, sortBy)

import Csv.Encode
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


init : String -> Table a
init =
    initSorted True


initSorted : Bool -> String -> Table a
initSorted desc col =
    { data = []
    , filtered = []
    , loading = False
    , state = Table.sortBy col desc
    , nextpage = Nothing
    , searchTerm = Nothing
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


searchData : Filter a -> UpdateSearchTerm -> Table a -> Table a
searchData config searchTerm table =
    let
        t =
            { table
                | searchTerm =
                    case searchTerm of
                        Update st ->
                            st

                        Keep ->
                            table.searchTerm
            }
    in
    filterData config t


filterData : Filter a -> Table a -> Table a
filterData config table =
    { table
        | filtered = filterTheData config table table.data
    }


asCsv : (a -> List ( String, String )) -> Table a -> String
asCsv prepare { filtered } =
    filtered
        |> Csv.Encode.encode
            { encoder = Csv.Encode.withFieldNames prepare
            , fieldSeparator = ','
            }


filterTable : (a -> Bool) -> Table a -> Table a
filterTable filter table =
    { table
        | data = List.filter filter table.data
        , filtered = List.filter filter table.filtered
    }


sortBy : String -> Bool -> Table d -> Table d
sortBy col desc table =
    { table | state = Table.sortBy col desc }
