module Update.Pathfinder.Table.RelatedAddressesTable exposing (appendClusterAddresses, appendTaggedAddresses, goToFirstPage, init, loadNextPage, previousPage, selectBoxMsg)

import Api.Data
import Basics.Extra exposing (flip)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Graph.Table
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.PagedTable as PagedTable
import Model.Pathfinder.Table.RelatedAddressesTable exposing (ListType(..), Model, filter, getCurrentTable, setCurrentTable)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter as Rs
import Tuple exposing (mapFirst)
import Update.Pathfinder.PagedTable as PagedTable
import Util exposing (n)
import Util.ThemedSelectBox as ThemedSelectBox exposing (OutMsg(..))


itemsPerPage : Int
itemsPerPage =
    5


init : Id -> Api.Data.Entity -> ( Model, List Effect )
init addressId entity =
    let
        model =
            { clusterAddresses =
                { table = Init.Graph.Table.initUnsorted
                , nrItems = Just entity.noAddresses
                , currentPage = 1
                , itemsPerPage = itemsPerPage
                }
            , taggedAddresses =
                { table = Init.Graph.Table.initUnsorted
                , nrItems = Just entity.noAddressTags
                , currentPage = 1
                , itemsPerPage = itemsPerPage
                }
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
    , loadData model Nothing
        ++ loadData { model | selected = ClusterAddresses } Nothing
    )


loadNextPage : Model -> ( Model, List Effect )
loadNextPage model =
    getCurrentTable model
        |> PagedTable.nextPage (loadData model)
        |> mapFirst (setCurrentTable model)


loadData : Model -> Maybe String -> List Effect
loadData model nextpage =
    let
        params =
            { currency = model.entity.currency
            , entity = model.entity.entity
            , pagesize = itemsPerPage + 1
            , nextpage = nextpage
            }
    in
    (case model.selected of
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
        |> List.singleton


previousPage : Model -> ( Model, List Effect )
previousPage ra =
    getCurrentTable ra
        |> PagedTable.decPage
        |> setCurrentTable ra
        |> n


goToFirstPage : Model -> ( Model, List Effect )
goToFirstPage ra =
    getCurrentTable ra
        |> PagedTable.goToFirstPage
        |> setCurrentTable ra
        |> n


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
    addresses
        |> filterCurrentAddress ra
        |> PagedTable.appendData ra.clusterAddresses filter nextpage
        |> flip Rs.s_clusterAddresses ra
        |> n


appendTaggedAddresses : Maybe String -> List Api.Data.Address -> Model -> ( Model, List Effect )
appendTaggedAddresses nextpage addresses ra =
    addresses
        |> filterCurrentAddress ra
        |> PagedTable.appendData ra.taggedAddresses filter nextpage
        |> flip Rs.s_taggedAddresses ra
        |> n


filterCurrentAddress : Model -> List Api.Data.Address -> List Api.Data.Address
filterCurrentAddress ra =
    List.filter (.address >> (/=) (Id.id ra.addressId))
