module Model.Graph.Browser exposing (..)

import Api.Data
import Model.Graph.Address exposing (Address)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Table exposing (..)
import Time


type alias Model =
    { type_ : Type
    , visible : Bool
    , now : Time.Posix
    }


type Type
    = None
    | Address (Loadable String Address) (Maybe AddressTable)
    | Entity (Loadable Int Entity) (Maybe EntityTable)


type Loadable id thing
    = Loading String id
    | Loaded thing


loadableAddressCurrency : Loadable id Address -> String
loadableAddressCurrency l =
    case l of
        Loading curr _ ->
            curr

        Loaded a ->
            a.address.currency


loadableEntityCurrency : Loadable id Entity -> String
loadableEntityCurrency l =
    case l of
        Loading curr _ ->
            curr

        Loaded a ->
            a.entity.currency


loadableAddressId : Loadable String Address -> String
loadableAddressId l =
    case l of
        Loading _ id ->
            id

        Loaded a ->
            a.address.address


loadableEntityId : Loadable Int Entity -> Int
loadableEntityId l =
    case l of
        Loading _ id ->
            id

        Loaded a ->
            a.entity.entity
