module Generate.Html exposing (..)

{-| -}

import Api.Raw exposing (..)
import Elm
import Elm.Annotation as Type
import Elm.Op
import Gen.Html.Styled
import Gen.Html.Styled.Attributes
import Generate.Html.ComponentNode as ComponentNode
import Generate.Html.TextNode as TextNode
import Tuple exposing (mapFirst, pair, second)


subcanvasNodeToExpressions : SubcanvasNode -> List ( String, Elm.Expression )
subcanvasNodeToExpressions node =
    case node of
        SubcanvasNodeComponentNode n ->
            componentNodeToExpressions n

        SubcanvasNodeComponentSetNode n ->
            componentSetNodeToExpressions n

        SubcanvasNodeTextNode n ->
            TextNode.toExpressions n

        _ ->
            []


componentNodeToExpressions : ComponentNode -> List ( String, Elm.Expression )
componentNodeToExpressions node =
    -- aim:
    -- myButton : { variant : Variant, iconVisible : Bool, text : String } -> List (Attribute msg) -> List (Html msg) -> Html msg
    Elm.fn2
        ( "attributes"
        , Nothing
        )
        ( "children"
        , Gen.Html.Styled.annotation_.html
            (Type.var "msg")
            |> Type.list
            |> Just
        )
        (\attributes children ->
            Gen.Html.Styled.call_.div
                (attributes
                    |> Elm.Op.cons
                        (ComponentNode.toCss node
                            |> Gen.Html.Styled.Attributes.css
                        )
                )
                (children
                    |> Elm.Op.append
                        (frameTraitsToExpressions node.frameTraits
                            |> List.map second
                            |> Elm.list
                        )
                )
        )
        |> pair node.frameTraits.isLayerTrait.name
        |> List.singleton


frameTraitsToExpressions : FrameTraits -> List ( String, Elm.Expression )
frameTraitsToExpressions node =
    node.children
        |> List.map subcanvasNodeToExpressions
        |> List.concat


componentSetNodeToExpressions : ComponentSetNode -> List ( String, Elm.Expression )
componentSetNodeToExpressions node =
    -- name --> Elm.declaration
    -- columnPropertyDefinitions -> Parameters
    -- variants -> union type
    -- children --> cases
    -- each child component -> componentNodeCode Body
    frameTraitsToExpressions node.frameTraits
        |> List.map (mapFirst ((++) (node.frameTraits.isLayerTrait.name ++ " ")))
