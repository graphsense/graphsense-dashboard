module Model.Graph.Browser exposing (Model, Row(..), ScrollPos, TableLink, Type(..), Value(..), loadableActor, loadableActorId, loadableAddress, loadableAddressCurrency, loadableAddressId, loadableBlock, loadableBlockId, loadableCurrency, loadableEntity, loadableEntityCurrency, loadableEntityId, loadableTx, loadableTxAccount, loadableTxId)

import Api.Data
import Components.Table exposing (Table)
import Config.Graph as Graph
import FontAwesome
import Html.Styled exposing (Html)
import IntDict exposing (IntDict)
import Json.Encode exposing (Value)
import Model.Actor as Act
import Model.Address as A
import Model.Block as B
import Model.Currency exposing (AssetIdentifier)
import Model.Entity as E
import Model.Graph.Actor exposing (Actor)
import Model.Graph.Address exposing (Address)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Layer as Layer
import Model.Graph.Link exposing (Link)
import Model.Graph.Table exposing (..)
import Model.Graph.Tag as Tag
import Model.Loadable as Loadable exposing (Loadable(..))
import Model.Tx as T
import Time


type alias Model =
    { type_ : Type
    , visible : Bool
    , now : Time.Posix
    , height : Maybe Float
    , layers : IntDict Layer.Layer
    , width : Float
    }


type Type
    = None
    | Address (Loadable String Address) (Maybe AddressTable)
    | Entity (Loadable Int Entity) (Maybe EntityTable)
    | Actor (Loadable String Actor) (Maybe ActorTable)
    | TxUtxo (Loadable String Api.Data.TxUtxo) (Maybe TxUtxoTable)
    | TxAccount (Loadable ( String, Maybe Int ) Api.Data.TxAccount) String (Maybe TxAccountTable)
    | Label String (Table Api.Data.AddressTag)
    | Block (Loadable Int Api.Data.Block) (Maybe BlockTable)
    | Addresslink Address (Link Address) (Maybe AddresslinkTable)
    | Entitylink Entity (Link Entity) (Maybe AddresslinkTable)
    | UserTags (Table Tag.UserTag)
    | Plugin


type Value msg
    = String String
    | Stack (List (Value msg))
    | Grid Int (List (Value msg))
    | AddressStr String
    | HashStr String
    | Country String String
    | Uri String String
    | IconLink FontAwesome.Icon String
    | InternalLink String String
    | EntityId Graph.Config Entity
    | Transactions { noIncomingTxs : Int, noOutgoingTxs : Int }
    | Usage Time.Posix Int
    | Duration Int
    | Value (List ( AssetIdentifier, Api.Data.Values ))
    | MultiValue Graph.Config String Int (List ( AssetIdentifier, Api.Data.Values ))
    | Input (String -> msg) msg String
    | Select (List ( String, String )) (String -> msg) String
    | Html (Html msg)
    | LoadingValue


type alias TableLink =
    { title : String
    , link : String
    , active : Bool
    }


type Row r i msg
    = Row ( String, r, Maybe TableLink )
    | RowWithMoreActionsButton ( String, r, Maybe (i -> msg) )
    | Note String
    | Footnote String
    | Image (Maybe String)
    | Rule
    | OptionalRow (Row r i msg) Bool


type alias ScrollPos =
    { scrollTop : Float
    , contentHeight : Int
    , containerHeight : Int
    }


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


loadableActor : Loadable String Actor -> Act.Actor
loadableActor l =
    case l of
        Loading _ actorId ->
            { actorId = actorId }

        Loaded a ->
            { actorId = a.id }


loadableBlock : Loadable Int Api.Data.Block -> B.Block
loadableBlock l =
    case l of
        Loading curr id ->
            { currency = curr
            , block = id
            }

        Loaded a ->
            { currency = a.currency
            , block = a.height
            }


loadableTx : Loadable String Api.Data.TxUtxo -> T.Tx
loadableTx l =
    case l of
        Loading curr id ->
            { currency = curr
            , txHash = id
            }

        Loaded a ->
            { currency = a.currency
            , txHash = a.txHash
            }


loadableTxAccount : Loadable ( String, Maybe Int ) Api.Data.TxAccount -> T.TxAccount
loadableTxAccount l =
    case l of
        Loading curr ( id, tokenTxId ) ->
            { currency = curr
            , txHash = id
            , tokenTxId = tokenTxId
            }

        Loaded a ->
            { currency = a.currency
            , txHash = a.txHash
            , tokenTxId = a.tokenTxId
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


loadableCurrency : Loadable id { a | currency : String } -> String
loadableCurrency l =
    case l of
        Loading curr _ ->
            curr

        Loaded a ->
            a.currency


loadableAddressId : Loadable String Address -> String
loadableAddressId =
    Loadable.id (.address >> .address)


loadableActorId : Loadable String Actor -> String
loadableActorId =
    Loadable.id .id


loadableEntityId : Loadable Int Entity -> Int
loadableEntityId =
    Loadable.id (.entity >> .entity)


loadableTxId : Loadable String { a | txHash : String } -> String
loadableTxId =
    Loadable.id .txHash


loadableBlockId : Loadable Int { a | height : Int } -> Int
loadableBlockId =
    Loadable.id .height
