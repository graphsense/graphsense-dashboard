module Util.Url.Parser.Query exposing
    ( Parser, string, int
    , map
    )

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
says a URL looks like this:

      https://example.com:8042/over/there?name=ferret#nose
      \___/   \______________/\_________/ \_________/ \__/
        |            |            |            |        |
      scheme     authority       path        query   fragment

This module is for parsing the `query` part.

In this library, a valid query looks like `?search=hats&page=2` where each
query parameter has the format `key=value` and is separated from the next
parameter by the `&` character.


# Parse Query Parameters

@docs Parser, string, int


# Mapping

@docs map

-}

import Dict
import Util.Url.Parser.Internal as Q



-- PARSERS


{-| Parse a query like `?search=hat&page=2` into nice Elm data.
-}
type alias Parser a =
    Q.QueryParser a



-- PRIMITIVES


{-| Handle `String` parameters.


    search : Parser (Maybe String)
    search =
        string "search"

    -- ?search=cats             == Just "cats"
    -- ?search=42               == Just "42"
    -- ?branch=left             == Nothing
    -- ?search=cats&search=dogs == Nothing

Check out [`custom`](#custom) if you need to handle multiple `search`
parameters for some reason.

-}
string : String -> Parser (Maybe String)
string key =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    Just str

                _ ->
                    Nothing


{-| Handle `Int` parameters. Maybe you want to show paginated search results:


    page : Parser (Maybe Int)
    page =
        int "page"

    -- ?page=2        == Just 2
    -- ?page=17       == Just 17
    -- ?page=two      == Nothing
    -- ?sort=date     == Nothing
    -- ?page=2&page=3 == Nothing

Check out [`custom`](#custom) if you need to handle multiple `page` parameters
or something like that.

-}
int : String -> Parser (Maybe Int)
int key =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    String.toInt str

                _ ->
                    Nothing



-- CUSTOM PARSERS


{-| Create a custom query parser. The [`string`](#string), [`int`](#int), and
[`enum`](#enum) parsers are defined using this function. It can help you handle
anything though!

Say you are unlucky enough to need to handle `?post=2&post=7` to show a couple
posts on screen at once. You could say:


    posts : Parser (Maybe (List Int))
    posts =
        custom "post" (List.maybeMap String.toInt)

    -- ?post=2        == [2]
    -- ?post=2&post=7 == [2, 7]
    -- ?post=2&post=x == [2]
    -- ?hats=2        == []

-}
custom : String -> (List String -> a) -> Parser a
custom key func =
    Q.Parser <|
        \dict ->
            func (Maybe.withDefault [] (Dict.get key dict))



-- MAPPING


{-| Transform a parser in some way. Maybe you want your `page` query parser to
default to `1` if there is any problem?

    page : Parser Int
    page =
        map (Result.withDefault 1) (int "page")

-}
map : (a -> b) -> Parser a -> Parser b
map func (Q.Parser a) =
    Q.Parser <| \dict -> func (a dict)
