module Data.Pathfinder.Address exposing (address1, address2, address3, address4, address5, address8)

import Config.Pathfinder exposing (nodeXOffset, nodeYOffset)
import Data.Pathfinder.Id as Id
import Init.Pathfinder.Address as Address
import Model.Pathfinder.Address exposing (Address)
import RecordSetter exposing (s_isStartingPoint)


address1 : Address
address1 =
    Address.init Id.address1 { x = 0, y = 0 }
        |> s_isStartingPoint True


address2 : Address
address2 =
    Address.init Id.address2 { x = 0, y = nodeYOffset }


address3 : Address
address3 =
    Address.init Id.address3 { x = 2 * nodeXOffset, y = 0 }


address4 : Address
address4 =
    Address.init Id.address4 { x = 2 * nodeXOffset, y = nodeYOffset }


address5 : Address
address5 =
    Address.init Id.address5 { x = 2 * nodeXOffset, y = nodeYOffset * 2 }


address8 : Address
address8 =
    Address.init Id.address8 { x = 4 * nodeXOffset, y = -nodeYOffset }
