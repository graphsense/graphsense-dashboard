module Graph.Update.TestLayer exposing (..)

import Api.Data
import Expect
import Init.Graph.Id as Id
import List.Extra
import Model.Graph.Entity as Graph
import Model.Graph.Id as Id
import Model.Graph.Layer exposing (Layer)
import Test exposing (..)
import Tuple exposing (..)
import Update.Graph.Layer as Layer


type TestRow
    = Address ( String, Api.Data.Address, List Layer -> Layer.Added Id.AddressId )
    | Entity ( String, Api.Data.Entity, List Layer -> Layer.Added Id.EntityId )


addr : { currency : String, address : String, entity : Int } -> Api.Data.Address
addr { currency, address, entity } =
    { address = address
    , balance = { fiatValues = [], value = 0 }
    , currency = currency
    , entity = entity
    , firstTx =
        { height = 1
        , timestamp = 1
        , txHash = ""
        }
    , inDegree = 0
    , lastTx =
        { height = 1
        , timestamp = 1
        , txHash = ""
        }
    , noIncomingTxs = 0
    , noOutgoingTxs = 0
    , outDegree = 0
    , tags = Nothing
    , totalReceived = { fiatValues = [], value = 0 }
    , totalSpent = { fiatValues = [], value = 0 }
    }


ent : { currency : String, entity : Int } -> Api.Data.Entity
ent { currency, entity } =
    { rootAddress = ""
    , balance = { fiatValues = [], value = 0 }
    , currency = currency
    , entity = entity
    , firstTx =
        { height = 1
        , timestamp = 1
        , txHash = ""
        }
    , inDegree = 0
    , lastTx =
        { height = 1
        , timestamp = 1
        , txHash = ""
        }
    , noIncomingTxs = 0
    , noOutgoingTxs = 0
    , outDegree = 0
    , noAddresses = 1
    , tags = Nothing
    , totalReceived = { fiatValues = [], value = 0 }
    , totalSpent = { fiatValues = [], value = 0 }
    }


updateEntity : Int -> Int -> (Graph.Entity -> Graph.Entity) -> List Layer -> List Layer
updateEntity l e upd =
    List.Extra.updateAt l
        (\layer ->
            { layer
                | entities =
                    List.Extra.updateAt e upd layer.entities
            }
        )


data : List TestRow
data =
    let
        addrA =
            addr { currency = "btc", address = "a", entity = 1 }

        entityId =
            Id.initEntityId { currency = "btc", layer = 0, id = 1 }

        entity1 =
            ent { currency = "btc", entity = 1 }
    in
    [ ( "add single address to empty graph"
      , addrA
      , \_ ->
            { layers = []
            , new = []
            }
      )
        |> Address
    , ( "add single entity to empty graph"
      , entity1
      , \_ ->
            { layers =
                [ { entities =
                        [ { id = entityId
                          , entity = entity1
                          , addresses = []
                          , x = 0
                          , y = 0
                          , dx = 0
                          , dy = 0
                          }
                        ]
                  , id = 0
                  , x = 0
                  }
                ]
            , new = [ entityId ]
            }
      )
        |> Entity
    , let
        id =
            Id.initAddressId { currency = addrA.currency, layer = 0, id = addrA.address }
      in
      ( "add address to entity"
      , addrA
      , \previous ->
            { layers =
                updateEntity 0
                    0
                    (\entity ->
                        { entity
                            | addresses =
                                [ { id = id
                                  , address = addrA
                                  , x = 25
                                  , y = 40
                                  }
                                ]
                        }
                    )
                    previous
            , new = [ id ]
            }
      )
        |> Address
    , let
        addrB =
            { addrA
                | address = "B"
            }

        id =
            Id.initAddressId { currency = addrB.currency, layer = 0, id = addrB.address }
      in
      ( "add another address to entity"
      , addrB
      , \previous ->
            { layers =
                updateEntity 0
                    0
                    (\entity ->
                        { entity
                            | addresses =
                                entity.addresses
                                    ++ [ { id = id
                                         , address = addrB
                                         , x = 25
                                         , y = 90
                                         }
                                       ]
                        }
                    )
                    previous
            , new = [ id ]
            }
      )
        |> Address
    , let
        entity2 =
            { entity1 | currency = "ltc" }
      in
      ( "add another entity"
      , entity2
      , \previous ->
            let
                entityId2 =
                    Id.initEntityId { currency = "ltc", layer = 0, id = 1 }
            in
            { layers =
                List.Extra.updateAt 0
                    (\layer ->
                        { layer
                            | entities =
                                layer.entities
                                    ++ [ { id = entityId2
                                         , entity = entity2
                                         , addresses = []
                                         , x = 0
                                         , y = 176
                                         , dx = 0
                                         , dy = 0
                                         }
                                       ]
                        }
                    )
                    previous
            , new = [ entityId2 ]
            }
      )
        |> Entity
    , let
        addrB =
            { addrA
                | currency = "ltc"
            }

        id =
            Id.initAddressId { currency = addrB.currency, layer = 0, id = addrB.address }
      in
      ( "add another ltc address to entity"
      , addrB
      , \previous ->
            { layers =
                updateEntity 0
                    1
                    (\entity ->
                        { entity
                            | addresses =
                                entity.addresses
                                    ++ [ { id = id
                                         , address = addrB
                                         , x = 25
                                         , y = 216
                                         }
                                       ]
                        }
                    )
                    previous
            , new = [ id ]
            }
      )
        |> Address
    ]


suite : Test
suite =
    describe "The Graph.Update.Layer module"
        (data
            |> List.foldl
                (\row ( layers, tests ) ->
                    case row of
                        Address ( title, input, output ) ->
                            let
                                o =
                                    output layers
                            in
                            (test title <|
                                \_ ->
                                    Expect.equal o (Layer.addAddress input layers)
                            )
                                :: tests
                                |> pair o.layers

                        Entity ( title, input, output ) ->
                            let
                                o =
                                    output layers
                            in
                            (test title <|
                                \_ ->
                                    Expect.equal o (Layer.addEntity input layers)
                            )
                                :: tests
                                |> pair o.layers
                )
                ( [], [] )
            |> second
        )
