module Model.Node exposing (Node(..))


type Node address entity
    = Address address
    | Entity entity
