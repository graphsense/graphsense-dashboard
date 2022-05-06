module Update.Store exposing (..)

import Api.Data
import Dict
import Effect.Store exposing (..)
import Model.Store exposing (..)
import Msg.Store exposing (..)


type Retrieved a
    = Found a
    | NotFound Effect


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        BrowserGotAddress address ->
            { model
                | addresses = Dict.insert ( address.currency, address.address ) address model.addresses
            }
                |> n


getAddress : { currency : String, address : String } -> Model -> Retrieved Api.Data.Address
getAddress { currency, address } { addresses } =
    Dict.get ( currency, address ) addresses
        |> Maybe.map Found
        |> Maybe.withDefault
            ({ currency = currency
             , address = address
             , toMsg = BrowserGotAddress
             }
                |> GetAddressEffect
                |> NotFound
            )
