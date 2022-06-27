module Update.Store exposing (..)

import Api.Data
import Dict
import Effect exposing (n)
import Effect.Store exposing (..)
import Model.Store exposing (..)
import Msg.Store exposing (..)
import RemoteData exposing (WebData)
import Tuple exposing (..)


type Retrieved a
    = Found a
    | NotFound (List Effect)


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        BrowserGotAddress address ->
            { model
                | addresses = Dict.insert ( address.currency, address.address ) (RemoteData.Success address) model.addresses
            }
                |> n

        BrowserGotEntity address entity ->
            { model
                | entities = Dict.insert ( entity.currency, entity.entity ) (RemoteData.Success entity) model.entities
            }
                |> n

        BrowserGotEntityForAddress address entity ->
            { model
                | entities = Dict.insert ( entity.currency, entity.entity ) (RemoteData.Success entity) model.entities
            }
                |> n

        StoreMsg (Store.BrowserGotAddress address) ->
            let
                ( store, storeEffects ) =
                    Store.update (Store.BrowserGotAddress address) model.store

                ( newStore, retrieved ) =
                    Store.getEntity
                        { currency = address.currency
                        , entity = address.entity
                        , forAddress = address.address
                        }
                        store

                ( graph, effects ) =
                    case retrieved of
                        Store.Found entity ->
                            Graph.addAddressAndEntity uc address entity model.graph
                                |> mapSecond (List.map GraphEffect)

                        Store.NotFound eff ->
                            Graph.addAddress uc address model.graph
                                |> mapSecond (List.map GraphEffect)
                                |> mapSecond ((++) (List.map StoreEffect eff))
            in
            ( { model
                | store = newStore
                , graph = graph
              }
            , List.map StoreEffect storeEffects
                ++ effects
            )

        StoreMsg (Store.BrowserGotEntity a entity) ->
            let
                ( store, storeEffects ) =
                    Store.update (Store.BrowserGotEntity a entity) model.store

                ( newStore, retrieved ) =
                    Store.getAddress { currency = entity.currency, address = a } store

                ( graph, effects ) =
                    case retrieved of
                        Store.Found address ->
                            Graph.addAddressAndEntity uc address entity model.graph
                                |> mapSecond (List.map GraphEffect)

                        Store.NotFound eff ->
                            Graph.addEntity uc entity model.graph
                                |> mapSecond (List.map GraphEffect)
                                |> mapSecond ((++) (List.map StoreEffect eff))
            in
            ( { model
                | store = newStore
                , graph = graph
              }
            , List.map StoreEffect storeEffects
                ++ effects
            )

        StoreMsg (Store.BrowserGotEntityForAddress a entity) ->
            update uc (Store.BrowserGotEntity a entity |> StoreMsg) model


remoteDataToRetreived : WebData a -> Maybe (Retrieved a)
remoteDataToRetreived a =
    case a of
        RemoteData.Success s ->
            Found s
                |> Just

        RemoteData.Loading ->
            NotFound []
                |> Just

        _ ->
            Nothing


getAddress : { currency : String, address : String } -> Model -> ( Model, Retrieved Api.Data.Address )
getAddress { currency, address } model =
    Dict.get ( currency, address ) model.addresses
        |> Maybe.andThen remoteDataToRetreived
        |> Maybe.map (pair model)
        |> Maybe.withDefault
            ({ currency = currency
             , address = address
             , toMsg = BrowserGotAddress
             }
                |> GetAddressEffect
                |> List.singleton
                |> NotFound
                |> pair
                    { model
                        | addresses = Dict.insert ( currency, address ) RemoteData.Loading model.addresses
                    }
            )


getEntity : { currency : String, entity : Int, forAddress : String } -> Model -> ( Model, Retrieved Api.Data.Entity )
getEntity { currency, entity, forAddress } model =
    Dict.get ( currency, entity ) model.entities
        |> Maybe.andThen remoteDataToRetreived
        |> Maybe.map (pair model)
        |> Maybe.withDefault
            ({ currency = currency
             , entity = entity
             , toMsg = BrowserGotEntity forAddress
             }
                |> GetEntityEffect
                |> List.singleton
                |> NotFound
                |> pair
                    { model
                        | entities = Dict.insert ( currency, entity ) RemoteData.Loading model.entities
                    }
            )
