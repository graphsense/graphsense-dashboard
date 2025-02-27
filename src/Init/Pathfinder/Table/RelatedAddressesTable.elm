module Init.Pathfinder.Table.RelatedAddressesTable exposing (init)

import Api.Data
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Graph.Table
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))


itemsPerPage : Int
itemsPerPage =
    5


init : Id -> Api.Data.Entity -> ( RelatedAddressesTable.Model, List Effect )
init addressId entity =
    ( { table =
            { table = Init.Graph.Table.initUnsorted
            , nrItems = Just entity.noAddresses
            , currentPage = 1
            , itemsPerPage = itemsPerPage
            }
      }
    , BrowserGotEntityAddressesForRelatedAddressesTable addressId
        |> Api.GetEntityAddressesEffect
            { currency = entity.currency
            , entity = entity.entity
            , pagesize = itemsPerPage
            , nextpage = Nothing
            }
        |> ApiEffect
        |> List.singleton
    )
