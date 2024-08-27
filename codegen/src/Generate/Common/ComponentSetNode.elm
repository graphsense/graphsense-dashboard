module Generate.Common.ComponentSetNode exposing (toTypes)

import Api.Raw exposing (..)
import Dict as Dict
import Elm
import Elm.Annotation as Annotation
import Generate.Util exposing (sanitize)
import List.Nonempty as NList
import Tuple exposing (pair, second)


toTypes : ComponentSetNode -> List Elm.Declaration
toTypes node =
    node.componentPropertyDefinitions
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []
        |> List.filter (second >> .type_ >> (==) ComponentPropertyTypeVARIANT)
        |> List.filterMap
            (\( name, { variantOptions } ) ->
                variantOptions
                    |> Maybe.andThen NList.fromList
                    |> Maybe.map (pair name)
            )
        |> List.map
            (\( name, options ) ->
                let
                    typeName =
                        node.frameTraits.isLayerTrait.name ++ " " ++ name
                in
                options
                    |> NList.toList
                    |> List.map ((++) (typeName ++ " ") >> sanitize >> Elm.variant)
                    |> Elm.customType (sanitize typeName)
            )
