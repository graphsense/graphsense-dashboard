module Model.Pathfinder.ConversionDetails exposing (ConversionDetailsModel, conversionLegTableFilter)

import Api.Data
import Components.Table exposing (Filter, Table)
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)


conversionLegTableFilter : Filter Api.Data.Tx
conversionLegTableFilter =
    { search =
        \_ _ -> True
    , filter = always True
    }


type alias ConversionDetailsModel =
    { isConversionLegTableOpen : Bool
    , raw : ConversionEdge
    , table : Table Api.Data.Tx
    }
