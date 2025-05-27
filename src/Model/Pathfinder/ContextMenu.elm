module Model.Pathfinder.ContextMenu exposing (ContextMenu, ContextMenuType(..), isContextMenuTypeEqual)

import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)


type alias ContextMenu =
    ( Coords, ContextMenuType )


type ContextMenuType
    = AddressContextMenu Id
    | TransactionContextMenu Id
    | AddressIdChevronActions Id
    | TransactionIdChevronActions Id


isContextMenuTypeEqual : ContextMenuType -> ContextMenuType -> Bool
isContextMenuTypeEqual a b =
    case ( a, b ) of
        ( AddressContextMenu idA, AddressContextMenu idB ) ->
            idA == idB

        ( TransactionContextMenu idA, TransactionContextMenu idB ) ->
            idA == idB

        ( AddressIdChevronActions idA, AddressIdChevronActions idB ) ->
            idA == idB

        ( TransactionIdChevronActions idA, TransactionIdChevronActions idB ) ->
            idA == idB

        _ ->
            False
