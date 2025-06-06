module Update.Pathfinder.AggEdge exposing (setFromAddress, setToAddress)

import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Tuple exposing (first, second)


setFromAddress : Address -> AggEdge -> AggEdge
setFromAddress address aggEdge =
    if first aggEdge.id == address.id then
        { aggEdge
            | fromAddress = Just address
        }

    else
        aggEdge


setToAddress : Address -> AggEdge -> AggEdge
setToAddress address aggEdge =
    if second aggEdge.id == address.id then
        { aggEdge
            | toAddress = Just address
        }

    else
        aggEdge
