module Model.Node exposing (Node(..), NodeType(..))


type Node address entity
    = Address address
    | Entity entity


type NodeType
    = AddressType
    | EntityType
