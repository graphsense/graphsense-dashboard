module Graph.Update.TestLayer exposing (..)

import Api.Data
import Color
import Config.Update as Update
import Dict
import Expect
import Init.Graph.Id as Id
import Init.Graph.Layer as Layer
import IntDict exposing (IntDict)
import List.Extra
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id exposing (EntityId)
import Model.Graph.Layer exposing (Layer)
import RecordSetter exposing (..)
import Test exposing (..)
import Tuple exposing (..)
import Update.Graph.Layer as Layer


type TestRow
    = Address ( String, Api.Data.Address, IntDict Layer -> Layer.Acc Id.AddressId )
    | Entity ( String, Api.Data.Entity, IntDict Layer -> Layer.Acc Id.EntityId )
    | EntityNeighbors
        { title : String
        , anchor : EntityId
        , isOutgoing : Bool
        , neighbors : List Api.Data.Entity
        , output : IntDict Layer -> Layer.Acc Id.EntityId
        }


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


updateEntity : Int -> Id.EntityId -> (Entity -> Entity) -> IntDict Layer -> IntDict Layer
updateEntity l e upd =
    IntDict.update l
        (Maybe.map
            (\layer ->
                { layer
                    | entities =
                        Dict.update e (Maybe.map upd) layer.entities
                }
            )
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

        entityNode =
            { id = entityId
            , entity = entity1
            , addresses = Dict.empty
            , category = Nothing
            , x = 0
            , y = 0
            , dx = 0
            , dy = 0
            , links = Entity.Links Dict.empty
            }

        entity2 =
            { entity1 | currency = "ltc" }

        entityId2 =
            Id.initEntityId { currency = "ltc", layer = 0, id = 1 }

        entityNode2 =
            { id = entityId2
            , entity = entity2
            , addresses = Dict.empty
            , category = Nothing
            , x = 0
            , y = 186
            , dx = 0
            , dy = 0
            , links = Entity.Links Dict.empty
            }
    in
    [ ( "add single address to empty graph"
      , addrA
      , \_ ->
            { layers = IntDict.empty
            , new = []
            , repositioned = []
            , colors = Dict.empty
            }
      )
        |> Address
    , ( "add single entity to empty graph"
      , entity1
      , \_ ->
            { layers =
                IntDict.singleton 0
                    { entities =
                        Dict.singleton entityId entityNode
                    , id = 0
                    , x = 0
                    }
            , new = [ entityNode ]
            , repositioned = []
            , colors = Dict.empty
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
                    entityId
                    (\entity ->
                        { entity
                            | addresses =
                                Dict.singleton id
                                    { id = id
                                    , entityId = entityId
                                    , address = addrA
                                    , category = Nothing
                                    , x = 25
                                    , y = 40
                                    , dx = 0
                                    , dy = 0
                                    }
                        }
                    )
                    previous
            , new = [ id ]
            , repositioned = []
            , colors = Dict.empty
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
                    entityId
                    (\entity ->
                        { entity
                            | addresses =
                                Dict.insert id
                                    { id = id
                                    , entityId = entityId
                                    , address = addrB
                                    , category = Nothing
                                    , x = 25
                                    , y = 90
                                    , dx = 0
                                    , dy = 0
                                    }
                                    entity.addresses
                        }
                    )
                    previous
            , new = [ id ]
            , repositioned = []
            , colors = Dict.empty
            }
      )
        |> Address
    , ( "add another entity"
      , entity2
      , \previous ->
            { layers =
                IntDict.update 0
                    (Maybe.map
                        (\layer ->
                            { layer
                                | entities = Dict.insert entityId2 entityNode2 layer.entities
                            }
                        )
                    )
                    previous
            , new = [ entityNode2 ]
            , repositioned = []
            , colors = Dict.empty
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
                    entityId2
                    (\entity ->
                        { entity
                            | addresses =
                                Dict.insert id
                                    { id = id
                                    , entityId = entityId2
                                    , address = addrB
                                    , category = Nothing
                                    , x = 25
                                    , y = 226
                                    , dx = 0
                                    , dy = 0
                                    }
                                    entity.addresses
                        }
                    )
                    previous
            , new = [ id ]
            , repositioned = []
            , colors = Dict.empty
            }
      )
        |> Address
    , let
        entity3 =
            { entity1
                | entity = 3
            }

        entity3Id =
            Id.initEntityId
                { currency = entity3.currency
                , layer = 1
                , id = entity3.entity
                }

        entityNode3 =
            { id = entity3Id
            , addresses = Dict.empty
            , x = 490
            , y = 362
            , dx = 0
            , dy = 0
            , category = Nothing
            , entity = entity3
            , links = Entity.Links Dict.empty
            }

        entity4 =
            { entity1
                | entity = 4
            }

        entity4Id =
            Id.initEntityId
                { currency = entity4.currency
                , layer = 1
                , id = entity4.entity
                }

        entityNode4 =
            { id = entity4Id
            , addresses = Dict.empty
            , x = 490
            , y = 262
            , dx = 0
            , dy = 0
            , category = Nothing
            , entity = entity4
            , links = Entity.Links Dict.empty
            }

        entity5 =
            { entity1
                | entity = 5
            }

        entity5Id =
            Id.initEntityId
                { currency = entity5.currency
                , layer = 1
                , id = entity5.entity
                }

        entityNode5 =
            { id = entity5Id
            , addresses = Dict.empty
            , x = 490
            , y = entityNode2.y
            , dx = 0
            , dy = 0
            , category = Nothing
            , entity = entity4
            , links = Entity.Links Dict.empty
            }
      in
      { title = "add 2 neighbors to second entity"
      , anchor = entityId2
      , isOutgoing = True
      , neighbors =
            [ entity3, entity4, entity5 ]
      , output =
            \previous ->
                { layers =
                    IntDict.insert 1
                        (Layer.init 1
                            |> s_entities
                                (Dict.fromList
                                    [ ( entity3Id
                                      , entityNode3
                                      )
                                    , ( entity4Id
                                      , entityNode4
                                      )
                                    , ( entity5Id
                                      , entityNode5
                                      )
                                    ]
                                )
                        )
                        previous
                , new = [ entityNode5, entityNode4, entityNode3 ]
                , repositioned = [ entityNode3, entityNode4 ]
                , colors = Dict.empty
                }
      }
        |> EntityNeighbors
    ]


config : Update.Config
config =
    { defaultColor = Color.grey
    , colorScheme = [ Color.red, Color.blue, Color.green ]
    }


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
                                    Expect.equal o (Layer.addAddress config Dict.empty input layers)
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
                                    Expect.equal o (Layer.addEntity config Dict.empty input layers)
                            )
                                :: tests
                                |> pair o.layers

                        EntityNeighbors { title, anchor, isOutgoing, neighbors, output } ->
                            let
                                o =
                                    output layers
                            in
                            (test title <|
                                \_ ->
                                    Layer.addEntityNeighbors
                                        config
                                        anchor
                                        isOutgoing
                                        Dict.empty
                                        neighbors
                                        layers
                                        |> Expect.equal o
                            )
                                :: tests
                                |> pair o.layers
                )
                ( IntDict.empty, [] )
            |> second
        )
