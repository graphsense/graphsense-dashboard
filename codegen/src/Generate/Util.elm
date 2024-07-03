module Generate.Util exposing (..)

import Api.Raw exposing (ComponentPropertyReferences, Rectangle, Transform)
import Dict
import Elm exposing (Expression)
import Elm.Annotation as Annotation
import Gen.Svg.Styled
import String.Case exposing (toCamelCaseLower)
import String.Format as Format
import Types exposing (ComponentPropertyExpressions, Config, Metadata)


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
        (Annotation.namedWith
            [ "Css" ]
            "LengthOrAuto"
            [ Annotation.var "compatible" ]
        )


lengthType : Elm.Expression -> Elm.Expression
lengthType =
    Elm.withType
        (Annotation.namedWith
            [ "Css" ]
            "Length"
            [ Annotation.var "compatible"
            , Annotation.var "units"
            ]
        )


numberType : Elm.Expression -> Elm.Expression
numberType =
    Elm.withType
        (Annotation.namedWith
            [ "Css" ]
            "Number"
            [ Annotation.var "compatible"
            ]
        )


intOrAutoType : Elm.Expression -> Elm.Expression
intOrAutoType =
    Elm.withType
        (Annotation.namedWith
            [ "Css" ]
            "IntOrAuto"
            [ Annotation.var "compatible" ]
        )


getElementAttributes : Config -> String -> Elm.Expression
getElementAttributes { attributes } name =
    Elm.get (sanitize name) attributes


uniqueElementName : String -> String -> String
uniqueElementName arg1 arg2 =
    arg1
        ++ " "
        ++ arg2
        |> sanitize


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


toTranslate : Rectangle -> String
toTranslate b =
    "translate({{ x }}, {{ y }})"
        |> Format.namedValue "x" (b.x |> String.fromFloat)
        |> Format.namedValue "y" (b.y |> String.fromFloat)


toRotate : Float -> String
toRotate r =
    "rotate({{ }})"
        |> Format.value (r * (180 / pi) |> String.fromFloat)


toMatrix : Transform -> String
toMatrix ( ( a_, c, e ), ( b, d, f ) ) =
    "matrix({{ }})"
        |> Format.value ([ a_, b, c, d, 0, 0 ] |> List.map String.fromFloat |> String.join ",")


metadataToDeclaration : String -> Metadata -> Elm.Declaration
metadataToDeclaration componentName metadata =
    let
        prefix =
            if componentName == metadata.name then
                componentName

            else
                componentName ++ " " ++ metadata.name
    in
    [ ( "x", Elm.float metadata.bbox.x )
    , ( "y", Elm.float metadata.bbox.y )
    , ( "width", Elm.float metadata.bbox.width )
    , ( "height", Elm.float metadata.bbox.height )
    , ( "strokeWidth", Elm.float metadata.strokeWidth )
    ]
        |> Elm.record
        |> Elm.declaration (prefix ++ " dimensions" |> sanitize)


sanitize : String -> String
sanitize s =
    let
        c =
            toCamelCaseLower s
    in
    if String.left 1 c |> String.toList |> List.all Char.isDigit then
        "n"
            ++ c

    else
        c
