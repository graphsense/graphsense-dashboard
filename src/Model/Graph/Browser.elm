module Model.Graph.Browser exposing (..)

import Api.Data
import Config.Graph as Graph
import Html.Styled exposing (Html)
import Json.Encode exposing (Value)
import Model.Graph.Address exposing (Address)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Table exposing (..)
import Time


type alias Model =
    { type_ : Type
    , visible : Bool
    , now : Time.Posix
    , height : Maybe Float
    }


type Type
    = None
    | Address (Loadable String Address) (Maybe AddressTable)
    | Entity (Loadable Int Entity) (Maybe EntityTable)
    | Plugin String


type Loadable id thing
    = Loading String id
    | Loaded thing


type Value msg
    = String String
    | EntityId Graph.Config Entity
    | Transactions { noIncomingTxs : Int, noOutgoingTxs : Int }
    | Usage Time.Posix Int
    | Duration Int
    | Value String Api.Data.Values
    | Input (String -> msg) msg String
    | Html (Html msg)
    | LoadingValue


type alias TableLink =
    { title : String
    , link : String
    , active : Bool
    }


type Row r
    = Row ( String, r, Maybe TableLink )
    | Rule


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
