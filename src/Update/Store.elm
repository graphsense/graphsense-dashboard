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
