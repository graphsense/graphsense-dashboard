module Model.Pathfinder.ContextMenu exposing (ContextMenu, ContextMenuType(..))

import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)


type alias ContextMenu =
    ( Coords, ContextMenuType )


type ContextMenuType
    = AddressContextMenu Id
    | TransactionContextMenu Id
