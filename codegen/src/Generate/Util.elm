module Generate.Util exposing (..)

import Api.Raw exposing (ComponentPropertyReferences, Rectangle)
import Dict as Dict
import Elm exposing (Expression)
import Elm.Annotation as Type
import Gen.Svg.Styled
import Gen.Svg.Styled.Attributes as Attributes
import String.Case exposing (toCamelCaseLower)
import String.Format as Format
import Types exposing (ComponentPropertyExpressions, Config)


m : (a -> Elm.Expression) -> Maybe a -> List Elm.Expression -> List Elm.Expression
m fun =
    Maybe.map (fun >> List.singleton) >> Maybe.withDefault [] >> (++)


a : (a -> Maybe Elm.Expression) -> Maybe a -> List Elm.Expression -> List Elm.Expression
a fun =
    Maybe.andThen fun >> Maybe.map List.singleton >> Maybe.withDefault [] >> (++)


mm : (a -> List Elm.Expression) -> Maybe a -> List Elm.Expression -> List Elm.Expression
mm fun =
    Maybe.map fun >> Maybe.withDefault [] >> (++)


aa : (a -> Maybe (List Elm.Expression)) -> Maybe a -> List Elm.Expression -> List Elm.Expression
aa fun =
    Maybe.andThen fun >> Maybe.withDefault [] >> (++)


lengthOrAutoType : Elm.Expression -> Elm.Expression
lengthOrAutoType =
    Elm.withType
        (Type.namedWith
            [ "Css" ]
            "LengthOrAuto"
            [ Type.var "compatible" ]
        )


lengthType : Elm.Expression -> Elm.Expression
lengthType =
    Elm.withType
        (Type.namedWith
            [ "Css" ]
            "Length"
            [ Type.var "compatible"
            , Type.var "units"
            ]
        )


numberType : Elm.Expression -> Elm.Expression
numberType =
    Elm.withType
        (Type.namedWith
            [ "Css" ]
            "Number"
            [ Type.var "compatible"
            ]
        )


intOrAutoType : Elm.Expression -> Elm.Expression
intOrAutoType =
    Elm.withType
        (Type.namedWith
            [ "Css" ]
            "IntOrAuto"
            [ Type.var "compatible" ]
        )


getElementAttributes : Config -> String -> Elm.Expression
getElementAttributes { attributes } name =
    Elm.get (toCamelCaseLower name) attributes


uniqueElementName : String -> String -> String
uniqueElementName arg1 arg2 =
    arg1
        ++ " "
        ++ arg2
        |> toCamelCaseLower


withVisibility : ComponentPropertyExpressions -> Maybe ComponentPropertyReferences -> Expression -> Expression
withVisibility def references element =
    references
        |> Maybe.andThen (Dict.get "visible")
        |> Maybe.andThen (\ref -> Dict.get ref def)
        |> Maybe.map
            (\bool ->
                Gen.Svg.Styled.text ""
                    |> Elm.ifThen bool element
            )
        |> Maybe.withDefault element


withInstanceSwap : ComponentPropertyExpressions -> Maybe ComponentPropertyReferences -> Expression -> Expression
withInstanceSwap def references element =
    references
        |> Maybe.andThen (Dict.get "mainComponent")
        |> Maybe.andThen (\ref -> Dict.get ref def)
        |> Maybe.withDefault element


toTranslate : Rectangle -> Elm.Expression
toTranslate b =
    "translate({{ x }}, {{ y }})"
        |> Format.namedValue "x" (b.x |> String.fromFloat)
        |> Format.namedValue "y" (b.y |> String.fromFloat)
        |> Attributes.transform