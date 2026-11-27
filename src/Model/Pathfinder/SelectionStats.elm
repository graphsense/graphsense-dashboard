module Model.Pathfinder.SelectionStats exposing (Stats, compute)

{-| Statistics for a multi-selection on the Pathfinder graph (backlog B006).

Everything comes from what the graph has already loaded: the addresses' own
data (totals, balance, activity), and the transactions on the graph for the
flows. Fiat only, in the user's currency; an amount without a fiat value
counts as 0.

  - `received`, `sent`, `balance`: the selected addresses' totals, all assets.
  - `inflow`, `outflow`, `between`: money moved on the graph into the
    selection from outside, out of it, and among the selected addresses.
  - `txsBetween`: for exactly two selected addresses, the transactions on the
    graph that involve both.

-}

import Api.Data
import Dict
import Model.Locale as Locale
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..))
import Model.Pathfinder.Tx exposing (Tx, TxType(..))
import RemoteData
import Set exposing (Set)


type alias Stats =
    { addresses : Int
    , txs : Int
    , clusters : Int
    , networks : List String
    , firstActivity : Maybe Int
    , lastActivity : Maybe Int
    , received : Float
    , sent : Float
    , balance : Float
    , inflow : Float
    , outflow : Float
    , between : Float
    , txsBetween : Maybe Int
    }


compute : String -> List MultiSelectOptions -> Network -> Stats
compute fiatCode selection network =
    let
        addressIds =
            List.filterMap
                (\o ->
                    case o of
                        MSelectedAddress id ->
                            Just id

                        MSelectedTx _ ->
                            Nothing
                )
                selection
                |> Set.fromList

        txCount =
            List.length selection - Set.size addressIds

        addresses =
            Set.toList addressIds |> List.filterMap (\id -> Dict.get id network.addresses)

        datas =
            List.filterMap (.data >> RemoteData.toMaybe) addresses

        fiat =
            Locale.getFiatValue fiatCode >> Maybe.withDefault 0

        allAssets native tokens =
            fiat native
                + (tokens
                    |> Maybe.map (Dict.values >> List.map fiat >> List.sum)
                    |> Maybe.withDefault 0
                  )

        timestamps f =
            List.filterMap (f >> Maybe.map .timestamp) datas

        flows =
            network.txs
                |> Dict.values
                |> List.foldl (addFlows fiat addressIds) { inflow = 0, outflow = 0, between = 0 }
    in
    { addresses = Set.size addressIds
    , txs = txCount
    , clusters = addresses |> List.filterMap Address.getClusterId |> List.map Id.toString |> Set.fromList |> Set.size
    , networks = Set.toList addressIds |> List.map Id.network |> Set.fromList |> Set.toList
    , firstActivity = timestamps .firstTx |> List.minimum
    , lastActivity = timestamps .lastTx |> List.maximum
    , received = datas |> List.map (\d -> allAssets d.totalReceived d.totalTokensReceived) |> List.sum
    , sent = datas |> List.map (\d -> allAssets d.totalSpent d.totalTokensSpent) |> List.sum
    , balance = datas |> List.map (\d -> allAssets d.balance d.tokenBalances) |> List.sum
    , inflow = flows.inflow
    , outflow = flows.outflow
    , between = flows.between
    , txsBetween =
        case Set.toList addressIds of
            [ a, b ] ->
                network.txs
                    |> Dict.values
                    |> List.filter (\tx -> involves a tx && involves b tx)
                    |> List.length
                    |> Just

            _ ->
                Nothing
    }


type alias Flows =
    { inflow : Float, outflow : Float, between : Float }


{-| An account tx moves its value from one address to another. A UTXO tx moves
each output's value; it comes from the selection when one of the tx's inputs
is selected.
-}
addFlows : (Api.Data.Values -> Float) -> Set Id -> Tx -> Flows -> Flows
addFlows fiat selected tx acc =
    let
        add fromIn toIn v a =
            case ( fromIn, toIn ) of
                ( True, True ) ->
                    { a | between = a.between + v }

                ( False, True ) ->
                    { a | inflow = a.inflow + v }

                ( True, False ) ->
                    { a | outflow = a.outflow + v }

                ( False, False ) ->
                    a
    in
    case tx.type_ of
        Account t ->
            add (Set.member t.from selected) (Set.member t.to selected) (fiat t.value) acc

        Utxo t ->
            let
                fromSelection =
                    Dict.keys t.inputs |> List.any (\id -> Set.member id selected)
            in
            Dict.toList t.outputs
                |> List.foldl (\( id, io ) a -> add fromSelection (Set.member id selected) (fiat io.values) a) acc


involves : Id -> Tx -> Bool
involves id tx =
    case tx.type_ of
        Account t ->
            t.from == id || t.to == id

        Utxo t ->
            Dict.member id t.inputs || Dict.member id t.outputs
