module Update.Pathfinder.Table.RelatedAddressesTable exposing (appendClusterAddresses, appendTaggedAddresses, init, selectBoxMsg, tableConfig, updateTable)

import Api.Data
import Basics.Extra exposing (flip)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Graph.Table
import Maybe.Extra
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Table.RelatedAddressesTable exposing (ListType(..), Model, filter, getCurrentTable, setCurrentTable)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import PagedTable
import RecordSetter as Rs
import Tuple exposing (mapFirst, mapSecond)
import Util exposing (n)
import Util.ThemedSelectBox as ThemedSelectBox exposing (OutMsg(..))


tableConfig : Model -> PagedTable.Config Effect
tableConfig =
    tableConfigWithListType Nothing


tableConfigWithListType : Maybe ListType -> Model -> PagedTable.Config Effect
tableConfigWithListType lt rm =
    { fetch = Just (loadData rm lt)
    }


itemsPerPage : Int
itemsPerPage =
    5


init : Id -> Api.Data.Entity -> ( Model, List Effect )
init addressId entity =
    let
        model =
            { clusterAddresses =
                PagedTable.init Init.Graph.Table.initUnsorted
                    |> PagedTable.setNrItems entity.noAddresses
                    |> PagedTable.setItemsPerPage itemsPerPage
            , taggedAddresses =
                PagedTable.init Init.Graph.Table.initUnsorted
                    |> PagedTable.setNrItems entity.noAddressTags
                    |> PagedTable.setItemsPerPage itemsPerPage
            , addressId = addressId
            , entity = { currency = entity.currency, entity = entity.entity }
            , selectBox =
                ThemedSelectBox.init
                    [ TaggedAddresses
                    , ClusterAddresses
                    ]
            , selected = TaggedAddresses
            }
    in
    ( model
    , [ loadData model (Just TaggedAddresses) itemsPerPage Nothing
      , loadData model (Just ClusterAddresses) itemsPerPage Nothing
      ]
    )


loadData : Model -> Maybe ListType -> Int -> Maybe String -> Effect
loadData model lt pagesize nextpage =
    let
        params =
            { currency = model.entity.currency
            , entity = model.entity.entity
            , pagesize = pagesize
            , nextpage = nextpage
            }

        selected =
            lt
                |> Maybe.withDefault model.selected
    in
    (case selected of
        ClusterAddresses ->
            BrowserGotEntityAddressesForRelatedAddressesTable
                >> AddressDetailsMsg model.addressId
                |> Api.GetEntityAddressesEffect params

        TaggedAddresses ->
            BrowserGotEntityAddressTagsForRelatedAddressesTable (Id.network model.addressId)
                >> AddressDetailsMsg model.addressId
                |> Api.GetEntityAddressTagsEffect params
    )
        |> ApiEffect


selectBoxMsg : ThemedSelectBox.Msg ListType -> Model -> ( Model, List Effect )
selectBoxMsg sm model =
    let
        ( newModel, outMsg ) =
            ThemedSelectBox.update sm model.selectBox
                |> mapFirst (flip Rs.s_selectBox model)
    in
    n <|
        case outMsg of
            Selected x ->
                { newModel
                    | selected = x
                }

            NoSelection ->
                newModel


appendClusterAddresses : Maybe String -> List Api.Data.Address -> Model -> ( Model, List Effect )
appendClusterAddresses nextpage addresses ra =
    PagedTable.appendData
        (tableConfigWithListType (Just ClusterAddresses) ra)
        (filter ra)
        nextpage
        addresses
        ra.clusterAddresses
        |> mapFirst (flip Rs.s_clusterAddresses ra)
        |> mapSecond Maybe.Extra.toList


appendTaggedAddresses : Maybe String -> List Api.Data.Address -> Model -> ( Model, List Effect )
appendTaggedAddresses nextpage addresses ra =
    PagedTable.appendData
        (tableConfigWithListType (Just TaggedAddresses) ra)
        (filter ra)
        nextpage
        addresses
        ra.taggedAddresses
        |> mapFirst (flip Rs.s_taggedAddresses ra)
        |> mapSecond Maybe.Extra.toList


updateTable : (PagedTable.Model Api.Data.Address -> ( PagedTable.Model Api.Data.Address, Maybe Effect )) -> Model -> ( Model, List Effect )
updateTable updTable model =
    getCurrentTable model
        |> updTable
        |> mapFirst (setCurrentTable model)
        |> mapSecond Maybe.Extra.toList
