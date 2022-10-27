module Update.Graph.Address exposing (..)

import Api.Data
import Color exposing (Color)
import Dict
import Model.Graph.Address exposing (..)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Link as Link
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
        | tags = Just tags
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


insertAddressShadowLink : Address -> Address -> Address
insertAddressShadowLink target source =
    case source.shadowLinks of
        Links links ->
            { source
                | shadowLinks =
                    Dict.insert target.id
                        { node = target
                        , forceShow = False
                        , link = Link.PlaceholderLinkData
                        , selected = False
                        }
                        links
                        |> Links
            }
