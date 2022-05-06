module Model.Graph.Id exposing (..)

{-| A tuple of layer index, currency, address/entity id
-}


type alias AddressId =
    ( Int, String, String )


type alias EntityId =
    ( Int, String, Int )


layer : ( Int, currency, id ) -> Int
layer ( i, _, _ ) =
    i


currency : ( layer, String, id ) -> String
currency ( _, i, _ ) =
    i


id : AddressId -> String
id ( _, _, i ) =
    i


entityId : EntityId -> Int
entityId ( _, _, i ) =
    i
