module Data.Pathfinder.Network exposing (..)

import Data.Pathfinder.Address as Address
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Tx as Tx
import Dict
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Network as Init
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Network exposing (Network)
import RecordSetter exposing (s_incomingTxs, s_outgoingTxs, s_visible)
import Set
import Update.Pathfinder.Tx as Tx


empty : Network
empty =
    Init.init


oneAddress : Network
oneAddress =
    { addresses =
        Dict.fromList
            [ ( Id.address1, Address.address1 ) ]
    , txs = Dict.empty
    }


oneAddressWithOutgoingTx : Network
oneAddressWithOutgoingTx =
    { oneAddress
        | txs =
            Dict.fromList
                [ ( Id.tx1
                  , Tx.updateUtxo
                        (Tx.updateUtxoIo Incoming Id.address1 (s_visible True))
                        Tx.tx1
                  )
                ]
        , addresses =
            Dict.update Id.address1
                (Maybe.map
                    (\address ->
                        { address
                            | outgoingTxs = Set.fromList [ Id.tx1 ]
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
                        (Tx.updateUtxoIo Outgoing Id.address1 (s_visible True))
                        Tx.tx2
                  )
                ]
        , addresses =
            Dict.update Id.address1
                (Maybe.map
                    (\address ->
                        { address
                            | incomingTxs = Set.fromList [ Id.tx2 ]
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
                            | outgoingTxs = Set.fromList [ Id.tx1 ]
                            , incomingTxs = Set.fromList [ Id.tx2 ]
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
                |> s_incomingTxs (Set.insert Id.tx1 Address.address3.incomingTxs)
    in
    { oneAddressWithOutgoingTx
        | addresses = Dict.insert Id.address3 address3 oneAddressWithOutgoingTx.addresses
        , txs =
            Dict.update Id.tx1
                (Maybe.map
                    (Tx.updateUtxo
                        (Tx.updateUtxoIo Outgoing Id.address3 (s_visible True))
                    )
                )
                oneAddressWithOutgoingTx.txs
    }


one2TwoAddresses : Network
one2TwoAddresses =
    let
        address4 =
            Address.address4
                |> s_incomingTxs (Set.insert Id.tx1 Address.address4.incomingTxs)
    in
    { twoConnectedAddresses
        | addresses = Dict.insert Id.address4 address4 twoConnectedAddresses.addresses
        , txs =
            Dict.update Id.tx1
                (Maybe.map
                    (Tx.updateUtxo
                        (Tx.updateUtxoIo Outgoing Id.address4 (s_visible True))
                    )
                )
                twoConnectedAddresses.txs
    }


one2ThreeAddresses : Network
one2ThreeAddresses =
    let
        address5 =
            Address.address5
                |> s_incomingTxs (Set.insert Id.tx1 Address.address5.incomingTxs)
    in
    { one2TwoAddresses
        | addresses = Dict.insert Id.address5 address5 one2TwoAddresses.addresses
        , txs =
            Dict.update Id.tx1
                (Maybe.map
                    (Tx.updateUtxo
                        (Tx.updateUtxoIo Outgoing Id.address5 (s_visible True))
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
                            | outgoingTxs = Set.insert Id.tx3 address.outgoingTxs
                        }
                    )
                )
                one2ThreeAddresses.addresses
        , txs =
            Dict.insert Id.tx3 Tx.tx3 one2ThreeAddresses.txs
                |> Dict.update Id.tx3
                    (Maybe.map
                        (Tx.updateUtxo
                            (Tx.updateUtxoIo Incoming Id.address1 (s_visible True))
                        )
                    )
    }