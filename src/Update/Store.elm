module Update.Store exposing (..)

import Api.Data
import Dict
import Effect exposing (n)
import Effect.Store exposing (..)
import Model.Store exposing (..)
import Msg.Store exposing (..)
import RemoteData exposing (WebData)


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


getAddress : { currency : String, address : String } -> Model -> Retrieved Api.Data.Address
getAddress { currency, address } { addresses } =
    Dict.get ( currency, address ) addresses
        |> Maybe.andThen remoteDataToRetreived
        |> Maybe.withDefault
            ({ currency = currency
             , address = address
             , toMsg = BrowserGotAddress
             }
                |> GetAddressEffect
                |> List.singleton
                |> NotFound
            )


getEntity : { currency : String, entity : Int, forAddress : String } -> Model -> Retrieved Api.Data.Entity
getEntity { currency, entity, forAddress } { entities } =
    Dict.get ( currency, entity ) entities
        |> Maybe.andThen remoteDataToRetreived
        |> Maybe.withDefault
            ({ currency = currency
             , entity = entity
             , toMsg = BrowserGotEntity forAddress
             }
                |> GetEntityEffect
                |> List.singleton
                |> NotFound
            )
