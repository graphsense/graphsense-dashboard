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


getByNameId : ( String, String ) -> Dict ( String, String ) a -> Maybe a
getByNameId ( name, id ) d =
    Dict.get ( name, id ) d
        |> Maybe.Extra.orElse (Dict.get ( name, "" ) d)


withVisibility : String -> Dict String ComponentPropertyExpressions -> Maybe ComponentPropertyReferences -> Expression -> Expression
withVisibility componentName def references element =
    references
        |> Debug.log ("123 references " ++ componentName)
        |> Maybe.andThen (Dict.get "visible")
        |> Debug.log "123 visibl "
        |> Maybe.andThen
            (\ref ->
                Dict.get componentName def
                    |> Debug.log "123 get def"
                    |> Maybe.andThen (Dict.get ref)
                    |> Debug.log "123 found"
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
        |> Debug.log ("xyz getTextProperty " ++ componentName)
        |> Maybe.andThen (Dict.get "characters")
        |> Debug.log "xyz characters"
        |> Maybe.andThen
            (\ref ->
                Dict.get componentName def
                    |> Debug.log "xyz getFromExpressions"
                    |> Maybe.andThen (Dict.get ref)
            )


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


detailsToDeclaration : String -> String -> Details -> Elm.Declaration
detailsToDeclaration parentName componentName details =
    let
        prefix =
            parentName
                ++ " "
                ++ (if componentName == details.name then
                        componentName

                    else
                        componentName ++ " " ++ details.name
                   )
    in
    [ ( "x", Elm.float details.bbox.x )
    , ( "y", Elm.float details.bbox.y )
    , ( "width", Elm.float details.bbox.width )
    , ( "height", Elm.float details.bbox.height )
    , ( "strokeWidth", Elm.float details.strokeWidth )
    , ( "styles", Elm.list details.styles )
    ]
        |> Elm.record
        |> Elm.declaration (prefix ++ " details" |> sanitize)


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
