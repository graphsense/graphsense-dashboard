module Data.Pathfinder.Network exposing (empty, one2ThreeAddresses, one2TwoAddresses, one2TwoTxs2ThreeAddresses, one2TwoTxs2ThreeAddressesWithOverlapping, oneAddress, oneAddressWithIncomingTx, oneAddressWithOutgoingTx, oneAddressWithTwoTxs, twoConnectedAddresses, twoIndependentAddresses)

import Data.Pathfinder.Address as Address
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Tx as Tx
import Dict
import Init.Pathfinder.Network as Init
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address as Address
import Model.Pathfinder.Network exposing (Network)
import RecordSetter exposing (s_address, s_incomingTxs)
import Set
import Update.Pathfinder.Tx as Tx



{-
                        TEST NETWORK

                                   ------------------> (A8)
                                  /                    /
   (A6) --> (T2) --> (A1) --> (T1) --> (A3) --> (T4) --
                         \      |\
                     (A2) \     | ---> (A4)
                           \    \
                            \    ----> (A5)
                             \
                              (T3) --> (A7)
-}


empty : Network
empty =
    Init.init


oneAddress : Network
oneAddress =
    { addresses =
        Dict.fromList
            [ ( Id.address1, Address.address1 ) ]
    , txs = Dict.empty
    , aggEdges = Dict.empty
    , addressAggEdgeMap = Dict.empty
    , animatedAddresses = Set.empty
    , animatedTxs = Set.empty
    , fetchedEdges = Set.empty
    }


oneAddressWithOutgoingTx : Network
oneAddressWithOutgoingTx =
    { oneAddress
        | txs =
            Dict.fromList
                [ ( Id.tx1
                  , Tx.updateUtxo
                        (Tx.updateUtxoIo Incoming Id.address1 (s_address (Just Address.address1)))
                        Tx.tx1
                  )
                ]
        , addresses =
            Dict.update Id.address1
                (Maybe.map
                    (\address ->
                        { address
                            | outgoingTxs = Address.Txs (Set.singleton Id.tx1)
                        }
                    )
                )
                oneAddress.addresses
    }


oneAddressWithIncomingTx : Network
oneAddressWithIncomingTx =
    { oneAddress
        | txs =
            Dict.fromList
                [ ( Id.tx2
                  , Tx.updateUtxo
                        (Tx.updateUtxoIo Outgoing Id.address1 (s_address (Just Address.address1)))
                        Tx.tx2
                  )
                ]
        , addresses =
            Dict.update Id.address1
                (Maybe.map
                    (\address ->
                        { address
                            | incomingTxs = Address.Txs (Set.singleton Id.tx2)
                        }
                    )
                )
                oneAddress.addresses
    }


oneAddressWithTwoTxs : Network
oneAddressWithTwoTxs =
    { oneAddress
        | txs =
            Dict.union
                oneAddressWithOutgoingTx.txs
                oneAddressWithIncomingTx.txs
        , addresses =
            Dict.update Id.address1
                (Maybe.map
                    (\address ->
                        { address
                            | outgoingTxs = Address.Txs (Set.singleton Id.tx1)
                            , incomingTxs = Address.Txs (Set.singleton Id.tx2)
                        }
                    )
                )
                oneAddress.addresses
    }


twoIndependentAddresses : Network
twoIndependentAddresses =
    { oneAddress
        | addresses = Dict.insert Id.address2 Address.address2 oneAddress.addresses
    }


twoConnectedAddresses : Network
twoConnectedAddresses =
    let
        address3 =
            Address.address3
                |> s_incomingTxs (Address.Txs (Set.insert Id.tx1 (Address.txsToSet Address.address3.incomingTxs)))
    in
    { oneAddressWithOutgoingTx
        | addresses = Dict.insert Id.address3 address3 oneAddressWithOutgoingTx.addresses
        , txs =
            Dict.update Id.tx1
                (Maybe.map
                    (Tx.updateUtxo
                        (Tx.updateUtxoIo Outgoing Id.address3 (s_address (Just Address.address5)))
                    )
                )
                oneAddressWithOutgoingTx.txs
    }


one2TwoAddresses : Network
one2TwoAddresses =
    let
        address4 =
            Address.address4
                |> s_incomingTxs (Address.Txs (Set.insert Id.tx1 (Address.txsToSet Address.address4.incomingTxs)))
    in
    { twoConnectedAddresses
        | addresses = Dict.insert Id.address4 address4 twoConnectedAddresses.addresses
        , txs =
            Dict.update Id.tx1
                (Maybe.map
                    (Tx.updateUtxo
                        (Tx.updateUtxoIo Outgoing Id.address4 (s_address (Just Address.address5)))
                    )
                )
                twoConnectedAddresses.txs
    }


one2ThreeAddresses : Network
one2ThreeAddresses =
    let
        address5 =
            Address.address5
                |> s_incomingTxs (Address.Txs (Set.insert Id.tx1 (Address.txsToSet Address.address5.incomingTxs)))
    in
    { one2TwoAddresses
        | addresses = Dict.insert Id.address5 address5 one2TwoAddresses.addresses
        , txs =
            Dict.update Id.tx1
                (Maybe.map
                    (Tx.updateUtxo
                        (Tx.updateUtxoIo Outgoing Id.address5 (s_address (Just address5)))
                    )
                )
                one2TwoAddresses.txs
    }


one2TwoTxs2ThreeAddresses : Network
one2TwoTxs2ThreeAddresses =
    { one2ThreeAddresses
        | addresses =
            Dict.update Id.address1
                (Maybe.map
                    (\address ->
                        { address
                            | outgoingTxs = Address.Txs (Set.insert Id.tx3 (Address.txsToSet address.outgoingTxs))
                        }
                    )
                )
                one2ThreeAddresses.addresses
        , txs =
            Dict.insert Id.tx3 Tx.tx3 one2ThreeAddresses.txs
                |> Dict.update Id.tx3
                    (Maybe.map
                        (Tx.updateUtxo
                            (Tx.updateUtxoIo Incoming Id.address1 (s_address (Just Address.address1)))
                        )
                    )
    }


one2TwoTxs2ThreeAddressesWithOverlapping : Network
one2TwoTxs2ThreeAddressesWithOverlapping =
    let
        address8 =
            Address.address8
                |> (\a ->
                        { a
                            | incomingTxs =
                                [ Id.tx1, Id.tx4 ]
                                    |> Set.fromList
                                    |> Address.Txs
                        }
                   )
    in
    { one2TwoTxs2ThreeAddresses
        | addresses =
            Dict.update Id.address3
                (Maybe.map
                    (\address ->
                        { address
                            | outgoingTxs = Address.Txs (Set.insert Id.tx4 (Address.txsToSet address.outgoingTxs))
                        }
                    )
                )
                one2TwoTxs2ThreeAddresses.addresses
                |> Dict.insert Id.address8 address8
        , txs =
            Dict.insert Id.tx4 Tx.tx4 one2TwoTxs2ThreeAddresses.txs
                |> Dict.update Id.tx4
                    (Maybe.map
                        (Tx.updateUtxo
                            (Tx.updateUtxoIo Incoming Id.address3 (s_address (Just Address.address3))
                                >> Tx.updateUtxoIo Outgoing Id.address8 (s_address (Just Address.address8))
                            )
                        )
                    )
    }
