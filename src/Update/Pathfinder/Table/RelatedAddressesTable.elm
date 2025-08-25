module Update.Pathfinder.Table.RelatedAddressesTable exposing (appendAddresses, appendTaggedAddresses, init, loadFirstPage, tableConfig, updateTable)

import Api.Data
import Basics.Extra exposing (flip)
import Components.PagedTable as PagedTable
import Components.Table as Table
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Maybe.Extra
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesTable exposing (Model, filter, getTable, setTable)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter as Rs
import Set
import Tuple exposing (mapFirst, mapSecond)


tableConfig : Model -> PagedTable.Config Effect
tableConfig rm =
    { fetch = Just (loadData rm)
    }


itemsPerPage : Int
itemsPerPage =
    5


init : Id -> Api.Data.Entity -> Model
init addressId entity =
    { table =
        PagedTable.init Table.initUnsorted
            |> PagedTable.setNrItems entity.noAddresses
            |> PagedTable.setItemsPerPage itemsPerPage
    , addressId = addressId
    , entity = { currency = entity.currency, entity = entity.entity }
    , existingTaggedAddresses = Set.empty
    , allTaggedAddressesFetched = False
    }


loadFirstPage : Model -> Effect
loadFirstPage model =
    loadData model itemsPerPage Nothing


loadData : Model -> Int -> Maybe String -> Effect
loadData model pagesize nextpage =
    let
        params =
            { currency = model.entity.currency
            , entity = model.entity.entity
            , pagesize = pagesize
            , nextpage = nextpage
            }

        fetchClusterAddresses =
            model.allTaggedAddressesFetched
    in
    (if fetchClusterAddresses then
        BrowserGotEntityAddressesForRelatedAddressesTable
            >> AddressDetailsMsg model.addressId
            |> Api.GetEntityAddressesEffect params

     else
        BrowserGotEntityAddressTagsForRelatedAddressesTable (Id.network model.addressId)
            >> AddressDetailsMsg model.addressId
            |> Api.GetEntityAddressTagsEffect params
    )
        |> ApiEffect


appendTaggedAddresses : Maybe String -> List Api.Data.Address -> Model -> ( Model, List Effect )
appendTaggedAddresses nextpage addresses ra =
    let
        existingTaggedAddresses =
            addresses
                |> List.map .address
                |> Set.fromList
                |> Set.union ra.existingTaggedAddresses
    in
    appendAddresses nextpage
        addresses
        { ra
            | allTaggedAddressesFetched =
                nextpage == Nothing
        }
        |> mapFirst (Rs.s_existingTaggedAddresses existingTaggedAddresses)
        |> (\( raNew, eff ) ->
                ( raNew
                , eff
                    ++ (if not ra.allTaggedAddressesFetched && raNew.allTaggedAddressesFetched then
                            [ loadData raNew itemsPerPage Nothing
                            ]

                        else
                            []
                       )
                )
           )


appendAddresses : Maybe String -> List Api.Data.Address -> Model -> ( Model, List Effect )
appendAddresses nextpage addresses ra =
    let
        dedupAddresses =
            addresses
                |> List.filter (.address >> flip Set.member ra.existingTaggedAddresses >> not)
    in
    PagedTable.appendData
        (tableConfig ra)
        (filter ra)
        nextpage
        dedupAddresses
        ra.table
        |> mapFirst (setTable ra)
        |> mapSecond Maybe.Extra.toList


updateTable : (PagedTable.Model Api.Data.Address -> ( PagedTable.Model Api.Data.Address, Maybe Effect )) -> Model -> ( Model, List Effect )
updateTable updTable model =
    getTable model
        |> updTable
        |> mapFirst (setTable model)
        |> mapSecond Maybe.Extra.toList
