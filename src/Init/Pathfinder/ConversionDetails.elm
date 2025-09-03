module Init.Pathfinder.ConversionDetails exposing (init)

import Components.Table as Table
import Model.Pathfinder.ConversionDetails exposing (ConversionDetailsModel, conversionLegTableFilter)
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)


init : ConversionEdge -> ConversionDetailsModel
init conversionEdge =
    { isConversionLegTableOpen = False
    , raw = conversionEdge
    , table =
        Table.initUnsorted
            |> Table.setData conversionLegTableFilter [ conversionEdge.rawInputTransaction, conversionEdge.rawOutputTransaction ]
    }
