module Update.Pathfinder.Table.RelatedAddressesTable exposing (appendAddresses, appendTaggedAddresses, init, loadFirstPage, tableConfig, updateTable)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Maybe.Extra
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesTable exposing (Model, filter, getTable, setTable)
import Msg.Pathfinder as Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter as Rs exposing (s_table)
import Set
import Tuple exposing (mapFirst, mapSecond)


tableConfig : Model -> InfiniteTable.Config Effect
tableConfig rm =
    { fetch = loadData rm
    , triggerOffset = 100
    }


pagesize : Int
pagesize =
    25


init : Id -> Api.Data.Entity -> Model
init addressId entity =
    { table =
        InfiniteTable.init
            { pagesize = pagesize
            , rowHeight = 36
            , containerHeight = 300
            }
            Table.initUnsorted
    , addressId = addressId
    , entity = { currency = entity.currency, entity = entity.entity }
    , existingTaggedAddresses = Set.empty
    , allTaggedAddressesFetched = False
    }


loadFirstPage : InfiniteTable.Config Effect -> Model -> ( Model, List Effect )
loadFirstPage config model =
    InfiniteTable.loadFirstPage config model.table
        |> mapFirst (flip s_table model)
        |> mapSecond Maybe.Extra.toList


loadData : Model -> Int -> Maybe String -> Effect
loadData model pagesize_ nextpage =
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
                            [ (tableConfig raNew).fetch pagesize Nothing
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
    InfiniteTable.appendData
        (tableConfig ra)
        (filter ra)
        nextpage
        dedupAddresses
        ra.table
        |> mapFirst (setTable ra)
        |> mapSecond Maybe.Extra.toList


updateTable : (InfiniteTable.Msg -> Pathfinder.Msg) -> (InfiniteTable.Model Api.Data.Address -> ( InfiniteTable.Model Api.Data.Address, Cmd InfiniteTable.Msg, Maybe Effect )) -> Model -> ( Model, List Effect )
updateTable mapCmd updTable model =
    let
        ( m, cmd, eff ) =
            getTable model
                |> updTable
    in
    ( m, eff )
        |> mapFirst (setTable model)
        |> mapSecond Maybe.Extra.toList
        |> mapSecond
            ((::)
                (cmd
                    |> Cmd.map mapCmd
                    |> CmdEffect
                )
            )
