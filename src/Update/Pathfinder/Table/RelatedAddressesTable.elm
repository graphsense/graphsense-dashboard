module Update.Pathfinder.Table.RelatedAddressesTable exposing (init, loadNextPage)

import Api.Data
import Basics.Extra exposing (flip)
import Effect.Api as Api
import Effect.Pathfinder exposing (Effect(..))
import Init.Graph.Table
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.PagedTable as PagedTable
import Model.Pathfinder.Table.RelatedAddressesTable exposing (ListType(..), Model)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails exposing (Msg(..))
import RecordSetter as Rs
import Tuple exposing (mapFirst)
import Util.ThemedSelectBox as ThemedSelectBox


itemsPerPage : Int
itemsPerPage =
    5


init : Id -> Api.Data.Entity -> ( Model, List Effect )
init addressId entity =
    let
        model =
            { table =
                { table = Init.Graph.Table.initUnsorted
                , nrItems = Just entity.noAddresses
                , currentPage = 1
                , itemsPerPage = itemsPerPage
                }
            , addressId = addressId
            , entity = { currency = entity.currency, entity = entity.entity }
            , selectBox =
                ThemedSelectBox.init
                    [ TaggedAddresses
                    , AllAddresses
                    ]
            , selected = TaggedAddresses
            }
    in
    ( model
    , loadData model Nothing
    )


loadNextPage : Model -> ( Model, List Effect )
loadNextPage model =
    model.table
        |> PagedTable.nextPage (loadData model)
        |> mapFirst (flip Rs.s_table model)


loadData : Model -> Maybe String -> List Effect
loadData model nextpage =
    BrowserGotEntityAddressesForRelatedAddressesTable
        >> AddressDetailsMsg model.addressId
        |> Api.GetEntityAddressesEffect
            { currency = model.entity.currency
            , entity = model.entity.entity
            , pagesize = itemsPerPage
            , nextpage = nextpage
            }
        |> ApiEffect
        |> List.singleton
