module Update.Graph.Table exposing (..)

import Csv.Encode
import Model.Graph.Table exposing (..)


appendData : Maybe String -> List a -> Table a -> Table a
appendData nextpage data table =
    { table
        | data = table.data ++ data
        , filtered = table.filtered ++ filterData table.filter data table.filterFunction
        , nextpage = nextpage
        , loading = False
    }


setData : List a -> Table a -> Table a
setData data table =
    { table
        | data = data
        , filtered = filterData table.filter data table.filterFunction
        , nextpage = table.nextpage
        , loading = False
    }


applyFilter : Maybe String -> Table a -> Table a
applyFilter filter table =
    { table
        | filter = filter
        , filtered = filterData filter table.data table.filterFunction
    }


filterData : Maybe String -> List a -> (String -> a -> Bool) -> List a
filterData fl l fun =
    fl
        |> Maybe.map (\f -> List.filter (fun f) l)
        |> Maybe.withDefault l


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
