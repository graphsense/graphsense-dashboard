module Generate.Html.RectangleNode exposing (..)

import Api.Raw exposing (..)
import Elm 
import Elm.Op
import Gen.Html.Styled as Html
import Gen.Html.Styled.Attributes as Attributes
import Generate.Common.RectangleNode exposing (getName)
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Util exposing (getElementAttributes, withVisibility)
import RecordSetter exposing (..)
import Types exposing (Config)


toExpressions : Config -> RectangleNode -> List Elm.Expression
toExpressions config node =
    Html.call_.div
        (getName node
            |> getElementAttributes config
            |> Elm.Op.append
                ((toCss node
                    |> Attributes.css
                 )
                    :: toAttributes node
                    |> Elm.list
                )
        )
        (Elm.list [])
        |> withVisibility config.propertyExpressions node.rectangularShapeTraits.defaultShapeTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


toCss : RectangleNode -> List Elm.Expression
toCss node =
    DefaultShapeTraits.toCss node.rectangularShapeTraits.defaultShapeTraits


toAttributes : RectangleNode -> List Elm.Expression
toAttributes node =
    []
