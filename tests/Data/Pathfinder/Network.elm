module Data.Pathfinder.Network exposing (..)

import Data.Pathfinder.Address as Address
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Tx as Tx
import Dict
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Network as Init
import Model.Pathfinder.Network exposing (Network)
import RecordSetter exposing (s_incomingTxs)
import Set


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
        | txs = Dict.fromList [ ( Id.tx1, Tx.tx1 ) ]
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
        | txs = Dict.fromList [ ( Id.tx2, Tx.tx2 ) ]
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
        | txs = Dict.fromList [ ( Id.tx1, Tx.tx1 ), ( Id.tx2, Tx.tx2 ) ]
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
    }
