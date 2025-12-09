module Update.Pathfinder.Table.RelatedAddressesTable exposing (abort, appendEntityAddresses, appendTaggedAddresses, gotoFirstPage, init, tableConfig, updateTable)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..), effectToTracker)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesTable exposing (Model, filter, getTable, setTable)
import Msg.Pathfinder as Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter as Rs exposing (s_force, s_table)
import Set
import Tuple exposing (mapFirst, mapSecond)


tableConfig : Model -> InfiniteTable.Config Effect
tableConfig rm =
    { fetch = loadData rm
    , force = False
    , triggerOffset = 100
    , effectToTracker = effectToTracker
    , abort = Api.CancelEffect >> ApiEffect
    }


pagesize : Int
pagesize =
    25


init : Id -> Api.Data.Entity -> Model
init addressId entity =
    { table =
        InfiniteTable.init "relatedAddressesTable" pagesize Table.initUnsorted
    , addressId = addressId
    , entity = { currency = entity.currency, entity = entity.entity }
    , existingTaggedAddresses = Set.empty
    , allTaggedAddressesFetched = False
    }


gotoFirstPage : InfiniteTable.Config Effect -> Model -> ( Model, List Effect )
gotoFirstPage config model =
    let
        force =
            model.allTaggedAddressesFetched
    in
    InfiniteTable.gotoFirstPage { config | force = force } model.table
        |> mapFirst (flip s_table model)


abort : InfiniteTable.Config Effect -> Model -> ( Model, List Effect )
abort config model =
    InfiniteTable.abort config model.table
        |> mapFirst (flip s_table model)


loadData : Model -> Maybe ( String, Bool ) -> Int -> Maybe String -> Effect
loadData model _ pagesize_ nextpage =
    let
        params =
            { currency = model.entity.currency
            , entity = model.entity.entity
            , pagesize = pagesize_
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


appendTaggedAddresses : (InfiniteTable.Msg -> Pathfinder.Msg) -> Maybe String -> List Api.Data.Address -> Model -> ( Model, List Effect )
appendTaggedAddresses mapCmd nextpage addresses ra =
    let
        existingTaggedAddresses =
            addresses
                |> List.map .address
                |> Set.fromList
                |> Set.union ra.existingTaggedAddresses

        force =
            not ra.allTaggedAddressesFetched && raNew.allTaggedAddressesFetched

        raNew =
            { ra
                | allTaggedAddressesFetched =
                    nextpage == Nothing
            }
    in
    appendAddresses mapCmd
        nextpage
        force
        addresses
        raNew
        |> mapFirst (Rs.s_existingTaggedAddresses existingTaggedAddresses)


appendEntityAddresses : (InfiniteTable.Msg -> Pathfinder.Msg) -> Maybe String -> List Api.Data.Address -> Model -> ( Model, List Effect )
appendEntityAddresses mapCmd nextpage addresses ra =
    let
        dedupAddresses =
            addresses
                |> List.filter (.address >> flip Set.member ra.existingTaggedAddresses >> not)
    in
    appendAddresses mapCmd nextpage False dedupAddresses ra


appendAddresses : (InfiniteTable.Msg -> Pathfinder.Msg) -> Maybe String -> Bool -> List Api.Data.Address -> Model -> ( Model, List Effect )
appendAddresses mapCmd nextpage force addresses ra =
    let
        ( table, cmd, eff ) =
            InfiniteTable.appendData
                (tableConfig ra |> s_force force)
                (filter ra)
                nextpage
                addresses
                ra.table
    in
    ( table, eff )
        |> mapFirst (setTable ra)
        |> mapSecond
            ((::)
                (cmd
                    |> Cmd.map mapCmd
                    |> CmdEffect
                )
            )


updateTable : (InfiniteTable.Msg -> Pathfinder.Msg) -> (InfiniteTable.Model Api.Data.Address -> ( InfiniteTable.Model Api.Data.Address, Cmd InfiniteTable.Msg, List Effect )) -> Model -> ( Model, List Effect )
updateTable mapCmd updTable model =
    let
        ( m, cmd, eff ) =
            getTable model
                |> updTable
    in
    ( m, eff )
        |> mapFirst (setTable model)
        |> mapSecond
            ((::)
                (cmd
                    |> Cmd.map mapCmd
                    |> CmdEffect
                )
            )
