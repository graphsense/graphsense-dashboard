module Support.GraphFilterFixture exposing (accountTx, addr, baseTx, chain, day, networkOf, txId, usd)

{-| Small Pathfinder networks for the on-graph filter tests.
-}

import Animation
import Api.Data
import Dict
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Tx as Tx exposing (Tx)


addr : String -> Id
addr =
    Id.init "eth"


txId : String -> Id
txId =
    Id.init "eth"


usd : Float -> Api.Data.Values
usd v =
    { value = 1, fiatValues = [ { code = "usd", value = v } ] }


{-| An account tx between two addresses on the graph.
-}
accountTx : String -> String -> String -> String -> Api.Data.Values -> Int -> Tx
accountTx id from to asset values timestamp =
    let
        raw =
            { contractCreation = Nothing
            , currency = asset
            , fee = Nothing
            , fromAddress = from
            , height = 1
            , identifier = id
            , isExternal = Nothing
            , network = "eth"
            , timestamp = timestamp
            , toAddress = to
            , tokenTxId = Nothing
            , txHash = id
            , txType = "account"
            , value = values
            }
    in
    baseTx (txId id)
        (Tx.Account
            { from = addr from
            , to = addr to
            , fromAddress = Just (Address.init (addr from) { x = 0, y = 0 })
            , toAddress = Just (Address.init (addr to) { x = 0, y = 0 })
            , value = values
            , raw = raw
            }
        )


networkOf : List String -> List Tx -> Network
networkOf addresses txs =
    let
        base =
            Network.init
    in
    { base
        | addresses =
            addresses
                |> List.map (\a -> ( addr a, Address.init (addr a) { x = 0, y = 0 } ))
                |> Dict.fromList
        , txs = txs |> List.map (\t -> ( t.id, t )) |> Dict.fromList
    }


{-| a --(1000 $, usdt, day 10)--> b --(5 $, eth, day 20)--> c, d stands alone.
-}
chain : Network
chain =
    networkOf [ "a", "b", "c", "d" ]
        [ accountTx "t1" "a" "b" "usdt" (usd 1000) (day 10)
        , accountTx "t2" "b" "c" "eth" (usd 5) (day 20)
        ]


day : Int -> Int
day n =
    n * 86400


baseTx : Id -> Tx.TxType -> Tx
baseTx id type_ =
    { id = id
    , hovered = False
    , selected = False
    , type_ = type_
    , x = 0
    , y = Animation.static 0
    , dx = 0
    , dy = 0
    , clock = 0
    , opacity = Animation.static 1
    , isStartingPoint = False
    , conversionType = Nothing
    , unsupportedConversions = []
    , index = 0
    }
