module Update.Graph.Address exposing (..)

import Api.Data
import Color exposing (Color)
import Model.Graph.Address exposing (..)
import Model.Graph.Coords exposing (Coords)
import RecordSetter exposing (..)


move : Coords -> Address -> Address
move { x, y } address =
    { address
        | dx = x
        , dy = y
    }


release : Address -> Address
release address =
    { address
        | dx = 0
        , dy = 0
        , x = address.x + address.dx
        , y = address.y + address.dy
    }


translate : Coords -> Address -> Address
translate { x, y } address =
    { address
        | x = address.x + x
        , y = address.y + y
    }


updateTags : List Api.Data.AddressTag -> Address -> Address
updateTags tags address =
    { address
        | address =
            address.address
                |> s_tags (Just tags)
        , category =
            tagsToCategory (Just tags)
    }


updateColor : Color -> Address -> Address
updateColor color address =
    { address
        | color =
            if Just (Color.toCssString color) == Maybe.map Color.toCssString address.color then
                Nothing

            else
                Just color
    }
