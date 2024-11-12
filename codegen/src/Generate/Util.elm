module Generate.Util exposing (..)

import Api.Raw exposing (ComponentPropertyReferences, Rectangle, Transform)
import Dict exposing (Dict)
import Elm exposing (Expression)
import Elm.Annotation as Annotation
import Gen.Svg.Styled
import Maybe.Extra
import String.Case exposing (toCamelCaseLower)
import String.Format as Format
import Types exposing (ComponentPropertyExpressions, Config, Details)


m : (a -> Elm.Expression) -> Maybe a -> List Elm.Expression -> List Elm.Expression
m fun =
    Maybe.map (fun >> List.singleton) >> Maybe.withDefault [] >> (++)


a : (a -> Maybe Elm.Expression) -> Maybe a -> List Elm.Expression -> List Elm.Expression
a fun =
    Maybe.andThen fun >> Maybe.map List.singleton >> Maybe.withDefault [] >> (++)


mm : (a -> List Elm.Expression) -> Maybe a -> List Elm.Expression -> List Elm.Expression
mm fun =
    Maybe.map fun >> Maybe.withDefault [] >> (++)


mm2 : (a -> b -> List Elm.Expression) -> Maybe a -> Maybe b -> List Elm.Expression -> List Elm.Expression
mm2 fun a_ b =
    Maybe.map2 fun a_ b
        |> Maybe.withDefault []
        |> (++)


aa : (a -> Maybe (List Elm.Expression)) -> Maybe a -> List Elm.Expression -> List Elm.Expression
aa fun =
    Maybe.andThen fun >> Maybe.withDefault [] >> (++)


a2 : (a -> b -> Maybe Elm.Expression) -> Maybe a -> Maybe b -> List Elm.Expression -> List Elm.Expression
a2 fun a_ b =
    Maybe.Extra.andThen2 fun a_ b
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
        |> (++)


a3 : (a -> b -> c -> Maybe Elm.Expression) -> Maybe a -> Maybe b -> Maybe c -> List Elm.Expression -> List Elm.Expression
a3 fun a_ b c =
    Maybe.Extra.andThen3 fun a_ b c
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
        |> (++)


i : Elm.Expression -> Bool -> List Elm.Expression -> List Elm.Expression
i expression condition =
    (++)
        (if condition then
            [ expression
            ]

         else
            []
        )


n : Elm.Expression -> Bool -> List Elm.Expression -> List Elm.Expression
n expression condition =
    (++)
        (if not condition then
            [ expression
            ]

         else
            []
        )


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


getByNameId : ( String, String ) -> Dict ( String, String ) a -> Maybe a
getByNameId ( name, id ) d =
    Dict.get ( name, id ) d
        |> Maybe.Extra.orElse (Dict.get ( name, "" ) d)


withVisibility : String -> Dict String ComponentPropertyExpressions -> Maybe ComponentPropertyReferences -> Expression -> Expression
withVisibility componentName def references element =
    references
        |> Maybe.andThen (Dict.get "visible")
        |> Maybe.andThen
            (\ref ->
                Dict.get componentName def
                    |> Maybe.andThen (Dict.get ref)
            )
        |> Maybe.map
            (\bool ->
                Gen.Svg.Styled.text ""
                    |> Elm.ifThen bool element
            )
        |> Maybe.withDefault element


getTextProperty : String -> Dict String ComponentPropertyExpressions -> Maybe ComponentPropertyReferences -> Maybe Expression
getTextProperty componentName def references =
    references
        |> Maybe.andThen (Dict.get "characters")
        |> Maybe.andThen
            (\ref ->
                Dict.get componentName def
                    |> Maybe.andThen (Dict.get ref)
            )


withInstanceSwap : ComponentPropertyExpressions -> Maybe ComponentPropertyReferences -> Expression -> Expression
withInstanceSwap def references element =
    getMainComponentProperty references
        |> Maybe.andThen (\ref -> Dict.get ref def)
        |> Maybe.withDefault element


getMainComponentProperty : Maybe ComponentPropertyReferences -> Maybe String
getMainComponentProperty =
    Maybe.andThen (Dict.get "mainComponent")


toTranslate : { a | x : Float, y : Float } -> String
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


detailsToDeclaration : String -> String -> Details -> Elm.Declaration
detailsToDeclaration parentName componentName details =
    let
        prefix =
            parentName
                ++ " "
                ++ (if componentName == details.name then
                        componentName

                    else
                        componentName ++ " " ++ details.instanceName ++ " " ++ details.name
                   )
    in
    [ ( "x", Elm.float details.bbox.x )
    , ( "y", Elm.float details.bbox.y )
    , ( "width", Elm.float details.bbox.width )
    , ( "height", Elm.float details.bbox.height )
    , ( "renderedWidth", Elm.float details.renderedSize.width )
    , ( "renderedHeight", Elm.float details.renderedSize.height )
    , ( "strokeWidth", Elm.float details.strokeWidth )
    , ( "styles", Elm.list details.styles )
    ]
        |> Elm.record
        |> Elm.declaration (sanitize prefix ++ "_details")


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
