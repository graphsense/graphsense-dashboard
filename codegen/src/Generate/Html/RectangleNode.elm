module Generate.Html.RectangleNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Elm.Op
import Gen.Html.Styled as Html
import Gen.Html.Styled.Attributes as Attributes
import Generate.Common.DefaultShapeTraits as Common
import Generate.Common.RectangleNode exposing (getName)
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Util exposing (getElementAttributes, withVisibility)
import RecordSetter exposing (..)
import Types exposing (Config, Details)


toExpressions : Config -> ( String, String ) -> RectangleNode -> List Elm.Expression
toExpressions config nameId node =
    Html.call_.div
        (getName node
            |> getElementAttributes config
            |> Elm.Op.append
                ((toStyles node
                    |> Attributes.css
                 )
                    :: toAttributes node
                    |> Elm.list
                )
        )
        (Elm.list [])
        |> withVisibility nameId config.propertyExpressions node.rectangularShapeTraits.defaultShapeTraits.isLayerTrait.componentPropertyReferences
        |> List.singleton


toStyles : RectangleNode -> List Elm.Expression
toStyles node =
    DefaultShapeTraits.toStyles node.rectangularShapeTraits.defaultShapeTraits


toDetails : RectangleNode -> Details
toDetails node =
    Common.toDetails (toStyles node) node.rectangularShapeTraits


toAttributes : RectangleNode -> List Elm.Expression
toAttributes node =
    []
