module Generate.Common.TextNode exposing (..)

import Api.Raw exposing (..)


getName : TextNode -> String
getName node =
    node.defaultShapeTraits.isLayerTrait.name
