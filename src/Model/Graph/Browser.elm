module Model.Graph.Browser exposing (..)

import Api.Data
import Config.Graph as Graph
import Html.Styled exposing (Html)
import Json.Encode exposing (Value)
import Model.Address as A
import Model.Entity as E
import Model.Graph.Address exposing (Address)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Table exposing (..)
import Model.Graph.Tag as Tag
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
      --| Label String (Maybe (Table Api.Data.AddressTag))
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


loadableAddress : Loadable String Address -> A.Address
loadableAddress l =
    case l of
        Loading curr id ->
            { currency = curr
            , address = id
            }

        Loaded a ->
            { currency = a.address.currency
            , address = a.address.address
            }


loadableEntity : Loadable Int Entity -> E.Entity
loadableEntity l =
    case l of
        Loading curr id ->
            { currency = curr
            , entity = id
            }

        Loaded a ->
            { currency = a.entity.currency
            , entity = a.entity.entity
            }


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
