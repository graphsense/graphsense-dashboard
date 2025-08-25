module Update.Pathfinder.Table.RelatedAddressesPubkeyTable exposing (appendAddresses, itemsPerPage, loadData, loadFirstPage, tableConfig, updateTable)

import Api.Data
import Api.Request.Addresses
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Maybe.Extra
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesPubkeyTable exposing (Model, filter, getTable, setTable)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import PagedTable
import Tuple exposing (mapFirst, mapSecond)


tableConfig : Model -> PagedTable.Config Effect
tableConfig rm =
    { fetch = Just (loadData rm.addressId)
    }


itemsPerPage : Int
itemsPerPage =
    5


loadFirstPage : Id -> Effect
loadFirstPage id =
    loadData id itemsPerPage Nothing


loadData : Id -> Int -> Maybe String -> Effect
loadData id pagesize nextpage =
    (BrowserGotPubkeyRelations
        >> AddressDetailsMsg id
    )
        |> Api.ListRelatedAddressesEffect
            { currency = Id.network id
            , address = Id.id id
            , reltype = Api.Request.Addresses.AddressRelationTypePubkey
            , pagesize = pagesize
            , nextpage = nextpage
            }
        |> ApiEffect


appendAddresses : Maybe String -> List Api.Data.RelatedAddress -> Model -> ( Model, List Effect )
appendAddresses nextpage addresses ra =
    PagedTable.appendData
        (tableConfig ra)
        (filter ra)
        nextpage
        addresses
        ra.table
        |> mapFirst (setTable ra)
        |> mapSecond Maybe.Extra.toList


updateTable : (PagedTable.Model Api.Data.RelatedAddress -> ( PagedTable.Model Api.Data.RelatedAddress, Maybe Effect )) -> Model -> ( Model, List Effect )
updateTable updTable model =
    getTable model
        |> updTable
        |> mapFirst (setTable model)
        |> mapSecond Maybe.Extra.toList
