module Update.Graph.Table exposing (UpdateSearchTerm(..), appendData, asCsv, filterData, searchData, setData)

import Config.Graph.Table as Table
import Csv.Encode
import Model.Graph.Table exposing (..)


appendData : Maybe String -> Table.Config a -> List a -> Table a -> Table a
appendData nextPage config data table =
    { table
        | data = table.data ++ data
        , filtered =
            table.filtered
                ++ filterTheData config table data
        , loading = False

        --, nextpage = Debug.todo "remove nextpage"
    }


filterTheData : Table.Config a -> Table a -> List a -> List a
filterTheData { search, filter } table data =
    let
        d =
            List.filter filter data
    in
    table.searchTerm
        |> Maybe.map (\term -> List.filter (search term) d)
        |> Maybe.withDefault d


setData : Table.Config a -> List a -> Table a -> Table a
setData config data table =
    { table
        | data = data
        , filtered = filterTheData config table data
        , loading = False
    }


type UpdateSearchTerm
    = Update (Maybe String)
    | Keep


searchData : Table.Config a -> UpdateSearchTerm -> Table a -> Table a
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


filterData : Table.Config a -> Table a -> Table a
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
        , filtered = List.filter filter table.data
    }
