module Generate.Html.RectangleNode exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Common.DefaultShapeTraits as Common
import Generate.Html.DefaultShapeTraits as DefaultShapeTraits
import Generate.Util exposing (m)
import RecordSetter exposing (..)
import Types exposing (ColorMap, Config, Details)



{-
   toExpressions : Config -> String -> RectangleNode -> List Elm.Expression
   toExpressions config componentName node =
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
           |> withVisibility componentName config.propertyExpressions node.rectangularShapeTraits.defaultShapeTraits.isLayerTrait.componentPropertyReferences
           |> List.singleton
-}


toStyles : ColorMap -> RectangleNode -> List Elm.Expression
toStyles colorMap node =
    DefaultShapeTraits.toStyles colorMap node.rectangularShapeTraits.defaultShapeTraits
        |> m (Css.px >> Css.width) (Maybe.map .x node.rectangularShapeTraits.defaultShapeTraits.size)
        |> m (Css.px >> Css.height) (Maybe.map .y node.rectangularShapeTraits.defaultShapeTraits.size)


toDetails : ColorMap -> RectangleNode -> Details
toDetails colorMap node =
    Common.toDetails (toStyles colorMap node) node.rectangularShapeTraits


toAttributes : RectangleNode -> List Elm.Expression
toAttributes _ =
    []
