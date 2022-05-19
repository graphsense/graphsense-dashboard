module Update.Graph.Browser exposing (..)

import Api.Data
import Effect.Graph exposing (Effect(..))
import Init.Graph.Browser exposing (..)
import Model.Graph.Address as Address
import Model.Graph.Browser exposing (..)
import Model.Graph.Entity as Entity
import Msg.Graph exposing (Msg(..))
import RecordSetter exposing (..)
import Route.Graph as Route


loadingAddress : { currency : String, address : String } -> Maybe Route.AddressTable -> Model -> ( Model, List Effect )
loadingAddress id maybeTable model =
    let
        ( table, effects ) =
            case maybeTable of
                Just Route.AddressTagsTable ->
                    ( AddressTagsTable initTable
                        |> AddressTable
                    , []
                    )

                Just Route.AddressTxsTable ->
                    ( AddressTxsTable initTable
                        |> AddressTable
                    , [ GetAddressTxsEffect
                            { currency = id.currency
                            , address = id.address
                            , nextpage = Nothing
                            , pagesize = 100
                            , toMsg = BrowserGotAddressTxs id
                            }
                      ]
                    )

                Just Route.AddressIncomingNeighborsTable ->
                    ( AddressIncomingNeighborsTable initTable
                        |> AddressTable
                    , []
                    )

                Just Route.AddressOutgoingNeighborsTable ->
                    ( AddressOutgoingNeighborsTable initTable
                        |> AddressTable
                    , []
                    )

                Nothing ->
                    ( NoTable
                    , []
                    )
    in
    ( { model
        | type_ = Address (Loading id.currency id.address)
        , table = table
        , visible = True
      }
    , effects
    )


show : Model -> Model
show model =
    { model
        | visible = True
    }


showEntity : Entity.Entity -> Model -> Model
showEntity entity model =
    show model
        |> s_type_ (Entity (Loaded entity))


showAddress : Address.Address -> Model -> Model
showAddress address model =
    show model
        |> s_type_ (Address (Loaded address))


showAddressTxs : { currency : String, address : String } -> Api.Data.AddressTxs -> Model -> Model
showAddressTxs id data model =
    if matchAddressId id model then
        { model
            | table =
                case model.table of
                    AddressTable (AddressTxsTable table) ->
                        appendData data.nextPage data.addressTxs table
                            |> AddressTxsTable
                            |> AddressTable

                    _ ->
                        model.table
        }

    else
        model


matchAddressId : { currency : String, address : String } -> Model -> Bool
matchAddressId { currency, address } model =
    case model.type_ of
        Address (Loading c id) ->
            c == currency && id == address

        Address (Loaded a) ->
            a.address.currency == currency && a.address.address == address

        _ ->
            False


appendData : Maybe String -> List a -> Table a -> Table a
appendData nextpage data table =
    { table
        | data = table.data ++ data
        , nextpage = nextpage
    }
