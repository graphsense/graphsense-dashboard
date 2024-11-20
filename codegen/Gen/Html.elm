module Gen.Html exposing (a, abbr, address, annotation_, article, aside, audio, b, bdi, bdo, blockquote, br, button, call_, canvas, caption, cite, code, col, colgroup, datalist, dd, del, details, dfn, div, dl, dt, em, embed, fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, header, hr, i, iframe, img, input, ins, kbd, label, legend, li, main_, map, mark, math, menu, menuitem, meter, moduleName_, nav, node, object, ol, optgroup, option, output, p, param, pre, progress, q, rp, rt, ruby, s, samp, section, select, small, source, span, strong, sub, summary, sup, table, tbody, td, text, textarea, tfoot, th, thead, time, tr, track, u, ul, values_, var, video, wbr)

{-| 
@docs moduleName_, text, node, map, h1, h2, h3, h4, h5, h6, div, p, hr, pre, blockquote, span, a, code, em, strong, i, b, u, sub, sup, br, ol, ul, li, dl, dt, dd, img, iframe, canvas, math, form, input, textarea, button, select, option, section, nav, article, aside, header, footer, address, main_, figure, figcaption, table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th, fieldset, legend, label, datalist, optgroup, output, progress, meter, audio, video, source, track, embed, object, param, ins, del, small, cite, dfn, abbr, time, var, samp, kbd, s, q, mark, ruby, rt, rp, bdi, bdo, wbr, details, summary, menuitem, menu, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Html" ]


{-| Just put plain text in the DOM. It will escape the string so that it appears
exactly as you specify.

    text "Hello World!"

text: String -> Html.Html msg
-}
text : String -> Elm.Expression
text textArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "text"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.string textArg ]


{-| General way to create HTML nodes. It is used to define all of the helper
functions in this library.

    div : List (Attribute msg) -> List (Html msg) -> Html msg
    div attributes children =
        node "div" attributes children

You can use this to create custom nodes if you need to create something that
is not covered by the helper functions in this library.

node: String -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
node : String -> List Elm.Expression -> List Elm.Expression -> Elm.Expression
node nodeArg nodeArg0 nodeArg1 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "node"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.string nodeArg, Elm.list nodeArg0, Elm.list nodeArg1 ]


{-| Transform the messages produced by some `Html`. In the following example,
we have `viewButton` that produces `()` messages, and we transform those values
into `Msg` values in `view`.

    type Msg = Left | Right

    view : model -> Html Msg
    view model =
      div []
        [ map (\_ -> Left) (viewButton "Left")
        , map (\_ -> Right) (viewButton "Right")
        ]

    viewButton : String -> Html ()
    viewButton name =
      button [ onClick () ] [ text name ]

This should not come in handy too often. Definitely read [this][reuse] before
deciding if this is what you want.

[reuse]: https://guide.elm-lang.org/reuse/

map: (a -> msg) -> Html.Html a -> Html.Html msg
-}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "map"
             , annotation =
                 Just
                     (Type.function
                          [ Type.function [ Type.var "a" ] (Type.var "msg")
                          , Type.namedWith [ "Html" ] "Html" [ Type.var "a" ]
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| h1: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg -}
h1 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h1 h1Arg h1Arg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "h1"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list h1Arg, Elm.list h1Arg0 ]


{-| h2: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg -}
h2 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h2 h2Arg h2Arg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "h2"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list h2Arg, Elm.list h2Arg0 ]


{-| h3: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg -}
h3 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h3 h3Arg h3Arg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "h3"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list h3Arg, Elm.list h3Arg0 ]


{-| h4: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg -}
h4 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h4 h4Arg h4Arg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "h4"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list h4Arg, Elm.list h4Arg0 ]


{-| h5: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg -}
h5 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h5 h5Arg h5Arg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "h5"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list h5Arg, Elm.list h5Arg0 ]


{-| h6: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg -}
h6 : List Elm.Expression -> List Elm.Expression -> Elm.Expression
h6 h6Arg h6Arg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "h6"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list h6Arg, Elm.list h6Arg0 ]


{-| Represents a generic container with no special meaning.

div: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
div : List Elm.Expression -> List Elm.Expression -> Elm.Expression
div divArg divArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "div"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list divArg, Elm.list divArg0 ]


{-| Defines a portion that should be displayed as a paragraph.

p: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
p : List Elm.Expression -> List Elm.Expression -> Elm.Expression
p pArg pArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "p"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list pArg, Elm.list pArg0 ]


{-| Represents a thematic break between paragraphs of a section or article or
any longer content.

hr: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
hr : List Elm.Expression -> List Elm.Expression -> Elm.Expression
hr hrArg hrArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "hr"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list hrArg, Elm.list hrArg0 ]


{-| Indicates that its content is preformatted and that this format must be
preserved.

pre: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
pre : List Elm.Expression -> List Elm.Expression -> Elm.Expression
pre preArg preArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "pre"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list preArg, Elm.list preArg0 ]


{-| Represents a content that is quoted from another source.

blockquote: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
blockquote : List Elm.Expression -> List Elm.Expression -> Elm.Expression
blockquote blockquoteArg blockquoteArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "blockquote"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list blockquoteArg, Elm.list blockquoteArg0 ]


{-| Represents text with no specific meaning. This has to be used when no other
text-semantic element conveys an adequate meaning, which, in this case, is
often brought by global attributes like `class`, `lang`, or `dir`.

span: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
span : List Elm.Expression -> List Elm.Expression -> Elm.Expression
span spanArg spanArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "span"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list spanArg, Elm.list spanArg0 ]


{-| Represents a hyperlink, linking to another resource.

a: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
a : List Elm.Expression -> List Elm.Expression -> Elm.Expression
a aArg aArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "a"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list aArg, Elm.list aArg0 ]


{-| Represents computer code.

code: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
code : List Elm.Expression -> List Elm.Expression -> Elm.Expression
code codeArg codeArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "code"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list codeArg, Elm.list codeArg0 ]


{-| Represents emphasized text, like a stress accent.

em: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
em : List Elm.Expression -> List Elm.Expression -> Elm.Expression
em emArg emArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "em"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list emArg, Elm.list emArg0 ]


{-| Represents especially important text.

strong: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
strong : List Elm.Expression -> List Elm.Expression -> Elm.Expression
strong strongArg strongArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "strong"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list strongArg, Elm.list strongArg0 ]


{-| Represents some text in an alternate voice or mood, or at least of
different quality, such as a taxonomic designation, a technical term, an
idiomatic phrase, a thought, or a ship name.

i: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
i : List Elm.Expression -> List Elm.Expression -> Elm.Expression
i iArg iArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "i"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list iArg, Elm.list iArg0 ]


{-| Represents a text which to which attention is drawn for utilitarian
purposes. It doesn't convey extra importance and doesn't imply an alternate
voice.

b: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
b : List Elm.Expression -> List Elm.Expression -> Elm.Expression
b bArg bArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "b"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list bArg, Elm.list bArg0 ]


{-| Represents a non-textual annotation for which the conventional
presentation is underlining, such labeling the text as being misspelt or
labeling a proper name in Chinese text.

u: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
u : List Elm.Expression -> List Elm.Expression -> Elm.Expression
u uArg uArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "u"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list uArg, Elm.list uArg0 ]


{-| Represent a subscript.

sub: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
sub : List Elm.Expression -> List Elm.Expression -> Elm.Expression
sub subArg subArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "sub"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list subArg, Elm.list subArg0 ]


{-| Represent a superscript.

sup: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
sup : List Elm.Expression -> List Elm.Expression -> Elm.Expression
sup supArg supArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "sup"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list supArg, Elm.list supArg0 ]


{-| Represents a line break.

br: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
br : List Elm.Expression -> List Elm.Expression -> Elm.Expression
br brArg brArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "br"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list brArg, Elm.list brArg0 ]


{-| Defines an ordered list of items.

ol: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
ol : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ol olArg olArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "ol"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list olArg, Elm.list olArg0 ]


{-| Defines an unordered list of items.

ul: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
ul : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ul ulArg ulArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "ul"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list ulArg, Elm.list ulArg0 ]


{-| Defines a item of an enumeration list.

li: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
li : List Elm.Expression -> List Elm.Expression -> Elm.Expression
li liArg liArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "li"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list liArg, Elm.list liArg0 ]


{-| Defines a definition list, that is, a list of terms and their associated
definitions.

dl: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
dl : List Elm.Expression -> List Elm.Expression -> Elm.Expression
dl dlArg dlArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "dl"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list dlArg, Elm.list dlArg0 ]


{-| Represents a term defined by the next `dd`.

dt: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
dt : List Elm.Expression -> List Elm.Expression -> Elm.Expression
dt dtArg dtArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "dt"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list dtArg, Elm.list dtArg0 ]


{-| Represents the definition of the terms immediately listed before it.

dd: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
dd : List Elm.Expression -> List Elm.Expression -> Elm.Expression
dd ddArg ddArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "dd"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list ddArg, Elm.list ddArg0 ]


{-| Represents an image.

img: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
img : List Elm.Expression -> List Elm.Expression -> Elm.Expression
img imgArg imgArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "img"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list imgArg, Elm.list imgArg0 ]


{-| Embedded an HTML document.

iframe: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
iframe : List Elm.Expression -> List Elm.Expression -> Elm.Expression
iframe iframeArg iframeArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "iframe"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list iframeArg, Elm.list iframeArg0 ]


{-| Represents a bitmap area for graphics rendering.

canvas: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
canvas : List Elm.Expression -> List Elm.Expression -> Elm.Expression
canvas canvasArg canvasArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "canvas"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list canvasArg, Elm.list canvasArg0 ]


{-| Defines a mathematical formula.

math: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
math : List Elm.Expression -> List Elm.Expression -> Elm.Expression
math mathArg mathArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "math"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list mathArg, Elm.list mathArg0 ]


{-| Represents a form, consisting of controls, that can be submitted to a
server for processing.

form: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
form : List Elm.Expression -> List Elm.Expression -> Elm.Expression
form formArg formArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "form"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list formArg, Elm.list formArg0 ]


{-| Represents a typed data field allowing the user to edit the data.

input: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
input : List Elm.Expression -> List Elm.Expression -> Elm.Expression
input inputArg inputArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "input"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list inputArg, Elm.list inputArg0 ]


{-| Represents a multiline text edit control.

textarea: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
textarea : List Elm.Expression -> List Elm.Expression -> Elm.Expression
textarea textareaArg textareaArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "textarea"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list textareaArg, Elm.list textareaArg0 ]


{-| Represents a button.

button: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
button : List Elm.Expression -> List Elm.Expression -> Elm.Expression
button buttonArg buttonArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "button"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list buttonArg, Elm.list buttonArg0 ]


{-| Represents a control allowing selection among a set of options.

select: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
select : List Elm.Expression -> List Elm.Expression -> Elm.Expression
select selectArg selectArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "select"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list selectArg, Elm.list selectArg0 ]


{-| Represents an option in a `select` element or a suggestion of a `datalist`
element.

option: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
option : List Elm.Expression -> List Elm.Expression -> Elm.Expression
option optionArg optionArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "option"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list optionArg, Elm.list optionArg0 ]


{-| Defines a section in a document.

section: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
section : List Elm.Expression -> List Elm.Expression -> Elm.Expression
section sectionArg sectionArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "section"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list sectionArg, Elm.list sectionArg0 ]


{-| Defines a section that contains only navigation links.

nav: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
nav : List Elm.Expression -> List Elm.Expression -> Elm.Expression
nav navArg navArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "nav"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list navArg, Elm.list navArg0 ]


{-| Defines self-contained content that could exist independently of the rest
of the content.

article: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
article : List Elm.Expression -> List Elm.Expression -> Elm.Expression
article articleArg articleArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "article"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list articleArg, Elm.list articleArg0 ]


{-| Defines some content loosely related to the page content. If it is removed,
the remaining content still makes sense.

aside: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
aside : List Elm.Expression -> List Elm.Expression -> Elm.Expression
aside asideArg asideArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "aside"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list asideArg, Elm.list asideArg0 ]


{-| Defines the header of a page or section. It often contains a logo, the
title of the web site, and a navigational table of content.

header: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
header : List Elm.Expression -> List Elm.Expression -> Elm.Expression
header headerArg headerArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "header"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list headerArg, Elm.list headerArg0 ]


{-| Defines the footer for a page or section. It often contains a copyright
notice, some links to legal information, or addresses to give feedback.

footer: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
footer : List Elm.Expression -> List Elm.Expression -> Elm.Expression
footer footerArg footerArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "footer"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list footerArg, Elm.list footerArg0 ]


{-| Defines a section containing contact information.

address: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
address : List Elm.Expression -> List Elm.Expression -> Elm.Expression
address addressArg addressArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "address"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list addressArg, Elm.list addressArg0 ]


{-| Defines the main or important content in the document. There is only one
`main` element in the document.

main_: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
main_ : List Elm.Expression -> List Elm.Expression -> Elm.Expression
main_ main_Arg main_Arg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "main_"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list main_Arg, Elm.list main_Arg0 ]


{-| Represents a figure illustrated as part of the document.

figure: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
figure : List Elm.Expression -> List Elm.Expression -> Elm.Expression
figure figureArg figureArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "figure"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list figureArg, Elm.list figureArg0 ]


{-| Represents the legend of a figure.

figcaption: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
figcaption : List Elm.Expression -> List Elm.Expression -> Elm.Expression
figcaption figcaptionArg figcaptionArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "figcaption"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list figcaptionArg, Elm.list figcaptionArg0 ]


{-| Represents data with more than one dimension.

table: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
table : List Elm.Expression -> List Elm.Expression -> Elm.Expression
table tableArg tableArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "table"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list tableArg, Elm.list tableArg0 ]


{-| Represents the title of a table.

caption: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
caption : List Elm.Expression -> List Elm.Expression -> Elm.Expression
caption captionArg captionArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "caption"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list captionArg, Elm.list captionArg0 ]


{-| Represents a set of one or more columns of a table.

colgroup: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
colgroup : List Elm.Expression -> List Elm.Expression -> Elm.Expression
colgroup colgroupArg colgroupArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "colgroup"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list colgroupArg, Elm.list colgroupArg0 ]


{-| Represents a column of a table.

col: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
col : List Elm.Expression -> List Elm.Expression -> Elm.Expression
col colArg colArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "col"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list colArg, Elm.list colArg0 ]


{-| Represents the block of rows that describes the concrete data of a table.

tbody: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
tbody : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tbody tbodyArg tbodyArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "tbody"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list tbodyArg, Elm.list tbodyArg0 ]


{-| Represents the block of rows that describes the column labels of a table.

thead: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
thead : List Elm.Expression -> List Elm.Expression -> Elm.Expression
thead theadArg theadArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "thead"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list theadArg, Elm.list theadArg0 ]


{-| Represents the block of rows that describes the column summaries of a table.

tfoot: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
tfoot : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tfoot tfootArg tfootArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "tfoot"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list tfootArg, Elm.list tfootArg0 ]


{-| Represents a row of cells in a table.

tr: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
tr : List Elm.Expression -> List Elm.Expression -> Elm.Expression
tr trArg trArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "tr"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list trArg, Elm.list trArg0 ]


{-| Represents a data cell in a table.

td: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
td : List Elm.Expression -> List Elm.Expression -> Elm.Expression
td tdArg tdArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "td"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list tdArg, Elm.list tdArg0 ]


{-| Represents a header cell in a table.

th: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
th : List Elm.Expression -> List Elm.Expression -> Elm.Expression
th thArg thArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "th"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list thArg, Elm.list thArg0 ]


{-| Represents a set of controls.

fieldset: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
fieldset : List Elm.Expression -> List Elm.Expression -> Elm.Expression
fieldset fieldsetArg fieldsetArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "fieldset"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list fieldsetArg, Elm.list fieldsetArg0 ]


{-| Represents the caption for a `fieldset`.

legend: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
legend : List Elm.Expression -> List Elm.Expression -> Elm.Expression
legend legendArg legendArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "legend"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list legendArg, Elm.list legendArg0 ]


{-| Represents the caption of a form control.

label: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
label : List Elm.Expression -> List Elm.Expression -> Elm.Expression
label labelArg labelArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "label"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list labelArg, Elm.list labelArg0 ]


{-| Represents a set of predefined options for other controls.

datalist: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
datalist : List Elm.Expression -> List Elm.Expression -> Elm.Expression
datalist datalistArg datalistArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "datalist"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list datalistArg, Elm.list datalistArg0 ]


{-| Represents a set of options, logically grouped.

optgroup: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
optgroup : List Elm.Expression -> List Elm.Expression -> Elm.Expression
optgroup optgroupArg optgroupArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "optgroup"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list optgroupArg, Elm.list optgroupArg0 ]


{-| Represents the result of a calculation.

output: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
output : List Elm.Expression -> List Elm.Expression -> Elm.Expression
output outputArg outputArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "output"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list outputArg, Elm.list outputArg0 ]


{-| Represents the completion progress of a task.

progress: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
progress : List Elm.Expression -> List Elm.Expression -> Elm.Expression
progress progressArg progressArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "progress"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list progressArg, Elm.list progressArg0 ]


{-| Represents a scalar measurement (or a fractional value), within a known
range.

meter: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
meter : List Elm.Expression -> List Elm.Expression -> Elm.Expression
meter meterArg meterArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "meter"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list meterArg, Elm.list meterArg0 ]


{-| Represents a sound or audio stream.

audio: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
audio : List Elm.Expression -> List Elm.Expression -> Elm.Expression
audio audioArg audioArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "audio"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list audioArg, Elm.list audioArg0 ]


{-| Represents a video, the associated audio and captions, and controls.

video: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
video : List Elm.Expression -> List Elm.Expression -> Elm.Expression
video videoArg videoArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "video"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list videoArg, Elm.list videoArg0 ]


{-| Allows authors to specify alternative media resources for media elements
like `video` or `audio`.

source: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
source : List Elm.Expression -> List Elm.Expression -> Elm.Expression
source sourceArg sourceArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "source"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list sourceArg, Elm.list sourceArg0 ]


{-| Allows authors to specify timed text track for media elements like `video`
or `audio`.

track: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
track : List Elm.Expression -> List Elm.Expression -> Elm.Expression
track trackArg trackArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "track"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list trackArg, Elm.list trackArg0 ]


{-| Represents a integration point for an external, often non-HTML,
application or interactive content.

embed: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
embed : List Elm.Expression -> List Elm.Expression -> Elm.Expression
embed embedArg embedArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "embed"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list embedArg, Elm.list embedArg0 ]


{-| Represents an external resource, which is treated as an image, an HTML
sub-document, or an external resource to be processed by a plug-in.

object: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
object : List Elm.Expression -> List Elm.Expression -> Elm.Expression
object objectArg objectArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "object"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list objectArg, Elm.list objectArg0 ]


{-| Defines parameters for use by plug-ins invoked by `object` elements.

param: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
param : List Elm.Expression -> List Elm.Expression -> Elm.Expression
param paramArg paramArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "param"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list paramArg, Elm.list paramArg0 ]


{-| Defines an addition to the document.

ins: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
ins : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ins insArg insArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "ins"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list insArg, Elm.list insArg0 ]


{-| Defines a removal from the document.

del: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
del : List Elm.Expression -> List Elm.Expression -> Elm.Expression
del delArg delArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "del"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list delArg, Elm.list delArg0 ]


{-| Represents a side comment, that is, text like a disclaimer or a
copyright, which is not essential to the comprehension of the document.

small: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
small : List Elm.Expression -> List Elm.Expression -> Elm.Expression
small smallArg smallArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "small"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list smallArg, Elm.list smallArg0 ]


{-| Represents the title of a work.

cite: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
cite : List Elm.Expression -> List Elm.Expression -> Elm.Expression
cite citeArg citeArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "cite"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list citeArg, Elm.list citeArg0 ]


{-| Represents a term whose definition is contained in its nearest ancestor
content.

dfn: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
dfn : List Elm.Expression -> List Elm.Expression -> Elm.Expression
dfn dfnArg dfnArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "dfn"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list dfnArg, Elm.list dfnArg0 ]


{-| Represents an abbreviation or an acronym; the expansion of the
abbreviation can be represented in the title attribute.

abbr: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
abbr : List Elm.Expression -> List Elm.Expression -> Elm.Expression
abbr abbrArg abbrArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "abbr"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list abbrArg, Elm.list abbrArg0 ]


{-| Represents a date and time value; the machine-readable equivalent can be
represented in the datetime attribute.

time: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
time : List Elm.Expression -> List Elm.Expression -> Elm.Expression
time timeArg timeArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "time"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list timeArg, Elm.list timeArg0 ]


{-| Represents a variable. Specific cases where it should be used include an
actual mathematical expression or programming context, an identifier
representing a constant, a symbol identifying a physical quantity, a function
parameter, or a mere placeholder in prose.

var: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
var : List Elm.Expression -> List Elm.Expression -> Elm.Expression
var varArg varArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "var"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list varArg, Elm.list varArg0 ]


{-| Represents the output of a program or a computer.

samp: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
samp : List Elm.Expression -> List Elm.Expression -> Elm.Expression
samp sampArg sampArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "samp"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list sampArg, Elm.list sampArg0 ]


{-| Represents user input, often from the keyboard, but not necessarily; it
may represent other input, like transcribed voice commands.

kbd: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
kbd : List Elm.Expression -> List Elm.Expression -> Elm.Expression
kbd kbdArg kbdArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "kbd"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list kbdArg, Elm.list kbdArg0 ]


{-| Represents content that is no longer accurate or relevant.

s: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
s : List Elm.Expression -> List Elm.Expression -> Elm.Expression
s sArg sArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "s"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list sArg, Elm.list sArg0 ]


{-| Represents an inline quotation.

q: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
q : List Elm.Expression -> List Elm.Expression -> Elm.Expression
q qArg qArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "q"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list qArg, Elm.list qArg0 ]


{-| Represents text highlighted for reference purposes, that is for its
relevance in another context.

mark: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
mark : List Elm.Expression -> List Elm.Expression -> Elm.Expression
mark markArg markArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "mark"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list markArg, Elm.list markArg0 ]


{-| Represents content to be marked with ruby annotations, short runs of text
presented alongside the text. This is often used in conjunction with East Asian
language where the annotations act as a guide for pronunciation, like the
Japanese furigana.

ruby: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
ruby : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ruby rubyArg rubyArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "ruby"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list rubyArg, Elm.list rubyArg0 ]


{-| Represents the text of a ruby annotation.

rt: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
rt : List Elm.Expression -> List Elm.Expression -> Elm.Expression
rt rtArg rtArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "rt"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list rtArg, Elm.list rtArg0 ]


{-| Represents parenthesis around a ruby annotation, used to display the
annotation in an alternate way by browsers not supporting the standard display
for annotations.

rp: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
rp : List Elm.Expression -> List Elm.Expression -> Elm.Expression
rp rpArg rpArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "rp"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list rpArg, Elm.list rpArg0 ]


{-| Represents text that must be isolated from its surrounding for
bidirectional text formatting. It allows embedding a span of text with a
different, or unknown, directionality.

bdi: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
bdi : List Elm.Expression -> List Elm.Expression -> Elm.Expression
bdi bdiArg bdiArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "bdi"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list bdiArg, Elm.list bdiArg0 ]


{-| Represents the directionality of its children, in order to explicitly
override the Unicode bidirectional algorithm.

bdo: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
bdo : List Elm.Expression -> List Elm.Expression -> Elm.Expression
bdo bdoArg bdoArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "bdo"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list bdoArg, Elm.list bdoArg0 ]


{-| Represents a line break opportunity, that is a suggested point for
wrapping text in order to improve readability of text split on several lines.

wbr: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
wbr : List Elm.Expression -> List Elm.Expression -> Elm.Expression
wbr wbrArg wbrArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "wbr"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list wbrArg, Elm.list wbrArg0 ]


{-| Represents a widget from which the user can obtain additional information
or controls.

details: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
details : List Elm.Expression -> List Elm.Expression -> Elm.Expression
details detailsArg detailsArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "details"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list detailsArg, Elm.list detailsArg0 ]


{-| Represents a summary, caption, or legend for a given `details`.

summary: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
summary : List Elm.Expression -> List Elm.Expression -> Elm.Expression
summary summaryArg summaryArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "summary"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list summaryArg, Elm.list summaryArg0 ]


{-| Represents a command that the user can invoke.

menuitem: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
menuitem : List Elm.Expression -> List Elm.Expression -> Elm.Expression
menuitem menuitemArg menuitemArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "menuitem"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list menuitemArg, Elm.list menuitemArg0 ]


{-| Represents a list of commands.

menu: List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
-}
menu : List Elm.Expression -> List Elm.Expression -> Elm.Expression
menu menuArg menuArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html" ]
             , name = "menu"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.namedWith
                                 [ "Html" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          ]
                          (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                     )
             }
        )
        [ Elm.list menuArg, Elm.list menuArg0 ]


annotation_ :
    { html : Type.Annotation -> Type.Annotation
    , attribute : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { html =
        \htmlArg0 ->
            Type.alias
                moduleName_
                "Html"
                [ htmlArg0 ]
                (Type.namedWith [ "VirtualDom" ] "Node" [ Type.var "msg" ])
    , attribute =
        \attributeArg0 ->
            Type.alias
                moduleName_
                "Attribute"
                [ attributeArg0 ]
                (Type.namedWith [ "VirtualDom" ] "Attribute" [ Type.var "msg" ])
    }


call_ :
    { text : Elm.Expression -> Elm.Expression
    , node :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h1 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h2 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h3 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h4 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h5 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , h6 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , div : Elm.Expression -> Elm.Expression -> Elm.Expression
    , p : Elm.Expression -> Elm.Expression -> Elm.Expression
    , hr : Elm.Expression -> Elm.Expression -> Elm.Expression
    , pre : Elm.Expression -> Elm.Expression -> Elm.Expression
    , blockquote : Elm.Expression -> Elm.Expression -> Elm.Expression
    , span : Elm.Expression -> Elm.Expression -> Elm.Expression
    , a : Elm.Expression -> Elm.Expression -> Elm.Expression
    , code : Elm.Expression -> Elm.Expression -> Elm.Expression
    , em : Elm.Expression -> Elm.Expression -> Elm.Expression
    , strong : Elm.Expression -> Elm.Expression -> Elm.Expression
    , i : Elm.Expression -> Elm.Expression -> Elm.Expression
    , b : Elm.Expression -> Elm.Expression -> Elm.Expression
    , u : Elm.Expression -> Elm.Expression -> Elm.Expression
    , sub : Elm.Expression -> Elm.Expression -> Elm.Expression
    , sup : Elm.Expression -> Elm.Expression -> Elm.Expression
    , br : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ol : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ul : Elm.Expression -> Elm.Expression -> Elm.Expression
    , li : Elm.Expression -> Elm.Expression -> Elm.Expression
    , dl : Elm.Expression -> Elm.Expression -> Elm.Expression
    , dt : Elm.Expression -> Elm.Expression -> Elm.Expression
    , dd : Elm.Expression -> Elm.Expression -> Elm.Expression
    , img : Elm.Expression -> Elm.Expression -> Elm.Expression
    , iframe : Elm.Expression -> Elm.Expression -> Elm.Expression
    , canvas : Elm.Expression -> Elm.Expression -> Elm.Expression
    , math : Elm.Expression -> Elm.Expression -> Elm.Expression
    , form : Elm.Expression -> Elm.Expression -> Elm.Expression
    , input : Elm.Expression -> Elm.Expression -> Elm.Expression
    , textarea : Elm.Expression -> Elm.Expression -> Elm.Expression
    , button : Elm.Expression -> Elm.Expression -> Elm.Expression
    , select : Elm.Expression -> Elm.Expression -> Elm.Expression
    , option : Elm.Expression -> Elm.Expression -> Elm.Expression
    , section : Elm.Expression -> Elm.Expression -> Elm.Expression
    , nav : Elm.Expression -> Elm.Expression -> Elm.Expression
    , article : Elm.Expression -> Elm.Expression -> Elm.Expression
    , aside : Elm.Expression -> Elm.Expression -> Elm.Expression
    , header : Elm.Expression -> Elm.Expression -> Elm.Expression
    , footer : Elm.Expression -> Elm.Expression -> Elm.Expression
    , address : Elm.Expression -> Elm.Expression -> Elm.Expression
    , main_ : Elm.Expression -> Elm.Expression -> Elm.Expression
    , figure : Elm.Expression -> Elm.Expression -> Elm.Expression
    , figcaption : Elm.Expression -> Elm.Expression -> Elm.Expression
    , table : Elm.Expression -> Elm.Expression -> Elm.Expression
    , caption : Elm.Expression -> Elm.Expression -> Elm.Expression
    , colgroup : Elm.Expression -> Elm.Expression -> Elm.Expression
    , col : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tbody : Elm.Expression -> Elm.Expression -> Elm.Expression
    , thead : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tfoot : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tr : Elm.Expression -> Elm.Expression -> Elm.Expression
    , td : Elm.Expression -> Elm.Expression -> Elm.Expression
    , th : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fieldset : Elm.Expression -> Elm.Expression -> Elm.Expression
    , legend : Elm.Expression -> Elm.Expression -> Elm.Expression
    , label : Elm.Expression -> Elm.Expression -> Elm.Expression
    , datalist : Elm.Expression -> Elm.Expression -> Elm.Expression
    , optgroup : Elm.Expression -> Elm.Expression -> Elm.Expression
    , output : Elm.Expression -> Elm.Expression -> Elm.Expression
    , progress : Elm.Expression -> Elm.Expression -> Elm.Expression
    , meter : Elm.Expression -> Elm.Expression -> Elm.Expression
    , audio : Elm.Expression -> Elm.Expression -> Elm.Expression
    , video : Elm.Expression -> Elm.Expression -> Elm.Expression
    , source : Elm.Expression -> Elm.Expression -> Elm.Expression
    , track : Elm.Expression -> Elm.Expression -> Elm.Expression
    , embed : Elm.Expression -> Elm.Expression -> Elm.Expression
    , object : Elm.Expression -> Elm.Expression -> Elm.Expression
    , param : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ins : Elm.Expression -> Elm.Expression -> Elm.Expression
    , del : Elm.Expression -> Elm.Expression -> Elm.Expression
    , small : Elm.Expression -> Elm.Expression -> Elm.Expression
    , cite : Elm.Expression -> Elm.Expression -> Elm.Expression
    , dfn : Elm.Expression -> Elm.Expression -> Elm.Expression
    , abbr : Elm.Expression -> Elm.Expression -> Elm.Expression
    , time : Elm.Expression -> Elm.Expression -> Elm.Expression
    , var : Elm.Expression -> Elm.Expression -> Elm.Expression
    , samp : Elm.Expression -> Elm.Expression -> Elm.Expression
    , kbd : Elm.Expression -> Elm.Expression -> Elm.Expression
    , s : Elm.Expression -> Elm.Expression -> Elm.Expression
    , q : Elm.Expression -> Elm.Expression -> Elm.Expression
    , mark : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ruby : Elm.Expression -> Elm.Expression -> Elm.Expression
    , rt : Elm.Expression -> Elm.Expression -> Elm.Expression
    , rp : Elm.Expression -> Elm.Expression -> Elm.Expression
    , bdi : Elm.Expression -> Elm.Expression -> Elm.Expression
    , bdo : Elm.Expression -> Elm.Expression -> Elm.Expression
    , wbr : Elm.Expression -> Elm.Expression -> Elm.Expression
    , details : Elm.Expression -> Elm.Expression -> Elm.Expression
    , summary : Elm.Expression -> Elm.Expression -> Elm.Expression
    , menuitem : Elm.Expression -> Elm.Expression -> Elm.Expression
    , menu : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { text =
        \textArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "text"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ textArg ]
    , node =
        \nodeArg nodeArg0 nodeArg1 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "node"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ nodeArg, nodeArg0, nodeArg1 ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "map"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.function
                                      [ Type.var "a" ]
                                      (Type.var "msg")
                                  , Type.namedWith
                                      [ "Html" ]
                                      "Html"
                                      [ Type.var "a" ]
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ mapArg, mapArg0 ]
    , h1 =
        \h1Arg h1Arg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "h1"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ h1Arg, h1Arg0 ]
    , h2 =
        \h2Arg h2Arg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "h2"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ h2Arg, h2Arg0 ]
    , h3 =
        \h3Arg h3Arg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "h3"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ h3Arg, h3Arg0 ]
    , h4 =
        \h4Arg h4Arg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "h4"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ h4Arg, h4Arg0 ]
    , h5 =
        \h5Arg h5Arg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "h5"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ h5Arg, h5Arg0 ]
    , h6 =
        \h6Arg h6Arg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "h6"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ h6Arg, h6Arg0 ]
    , div =
        \divArg divArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "div"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ divArg, divArg0 ]
    , p =
        \pArg pArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "p"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ pArg, pArg0 ]
    , hr =
        \hrArg hrArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "hr"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ hrArg, hrArg0 ]
    , pre =
        \preArg preArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "pre"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ preArg, preArg0 ]
    , blockquote =
        \blockquoteArg blockquoteArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "blockquote"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ blockquoteArg, blockquoteArg0 ]
    , span =
        \spanArg spanArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "span"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ spanArg, spanArg0 ]
    , a =
        \aArg aArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "a"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ aArg, aArg0 ]
    , code =
        \codeArg codeArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "code"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ codeArg, codeArg0 ]
    , em =
        \emArg emArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "em"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ emArg, emArg0 ]
    , strong =
        \strongArg strongArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "strong"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ strongArg, strongArg0 ]
    , i =
        \iArg iArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "i"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ iArg, iArg0 ]
    , b =
        \bArg bArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "b"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ bArg, bArg0 ]
    , u =
        \uArg uArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "u"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ uArg, uArg0 ]
    , sub =
        \subArg subArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "sub"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ subArg, subArg0 ]
    , sup =
        \supArg supArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "sup"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ supArg, supArg0 ]
    , br =
        \brArg brArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "br"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ brArg, brArg0 ]
    , ol =
        \olArg olArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "ol"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ olArg, olArg0 ]
    , ul =
        \ulArg ulArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "ul"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ ulArg, ulArg0 ]
    , li =
        \liArg liArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "li"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ liArg, liArg0 ]
    , dl =
        \dlArg dlArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "dl"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ dlArg, dlArg0 ]
    , dt =
        \dtArg dtArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "dt"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ dtArg, dtArg0 ]
    , dd =
        \ddArg ddArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "dd"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ ddArg, ddArg0 ]
    , img =
        \imgArg imgArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "img"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ imgArg, imgArg0 ]
    , iframe =
        \iframeArg iframeArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "iframe"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ iframeArg, iframeArg0 ]
    , canvas =
        \canvasArg canvasArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "canvas"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ canvasArg, canvasArg0 ]
    , math =
        \mathArg mathArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "math"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ mathArg, mathArg0 ]
    , form =
        \formArg formArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "form"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ formArg, formArg0 ]
    , input =
        \inputArg inputArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "input"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ inputArg, inputArg0 ]
    , textarea =
        \textareaArg textareaArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "textarea"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ textareaArg, textareaArg0 ]
    , button =
        \buttonArg buttonArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "button"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ buttonArg, buttonArg0 ]
    , select =
        \selectArg selectArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "select"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ selectArg, selectArg0 ]
    , option =
        \optionArg optionArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "option"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ optionArg, optionArg0 ]
    , section =
        \sectionArg sectionArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "section"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ sectionArg, sectionArg0 ]
    , nav =
        \navArg navArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "nav"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ navArg, navArg0 ]
    , article =
        \articleArg articleArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "article"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ articleArg, articleArg0 ]
    , aside =
        \asideArg asideArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "aside"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ asideArg, asideArg0 ]
    , header =
        \headerArg headerArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "header"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ headerArg, headerArg0 ]
    , footer =
        \footerArg footerArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "footer"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ footerArg, footerArg0 ]
    , address =
        \addressArg addressArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "address"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ addressArg, addressArg0 ]
    , main_ =
        \main_Arg main_Arg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "main_"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ main_Arg, main_Arg0 ]
    , figure =
        \figureArg figureArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "figure"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ figureArg, figureArg0 ]
    , figcaption =
        \figcaptionArg figcaptionArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "figcaption"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ figcaptionArg, figcaptionArg0 ]
    , table =
        \tableArg tableArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "table"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ tableArg, tableArg0 ]
    , caption =
        \captionArg captionArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "caption"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ captionArg, captionArg0 ]
    , colgroup =
        \colgroupArg colgroupArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "colgroup"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ colgroupArg, colgroupArg0 ]
    , col =
        \colArg colArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "col"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ colArg, colArg0 ]
    , tbody =
        \tbodyArg tbodyArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "tbody"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ tbodyArg, tbodyArg0 ]
    , thead =
        \theadArg theadArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "thead"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ theadArg, theadArg0 ]
    , tfoot =
        \tfootArg tfootArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "tfoot"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ tfootArg, tfootArg0 ]
    , tr =
        \trArg trArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "tr"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ trArg, trArg0 ]
    , td =
        \tdArg tdArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "td"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ tdArg, tdArg0 ]
    , th =
        \thArg thArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "th"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ thArg, thArg0 ]
    , fieldset =
        \fieldsetArg fieldsetArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "fieldset"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ fieldsetArg, fieldsetArg0 ]
    , legend =
        \legendArg legendArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "legend"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ legendArg, legendArg0 ]
    , label =
        \labelArg labelArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "label"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ labelArg, labelArg0 ]
    , datalist =
        \datalistArg datalistArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "datalist"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ datalistArg, datalistArg0 ]
    , optgroup =
        \optgroupArg optgroupArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "optgroup"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ optgroupArg, optgroupArg0 ]
    , output =
        \outputArg outputArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "output"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ outputArg, outputArg0 ]
    , progress =
        \progressArg progressArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "progress"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ progressArg, progressArg0 ]
    , meter =
        \meterArg meterArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "meter"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ meterArg, meterArg0 ]
    , audio =
        \audioArg audioArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "audio"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ audioArg, audioArg0 ]
    , video =
        \videoArg videoArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "video"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ videoArg, videoArg0 ]
    , source =
        \sourceArg sourceArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "source"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ sourceArg, sourceArg0 ]
    , track =
        \trackArg trackArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "track"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ trackArg, trackArg0 ]
    , embed =
        \embedArg embedArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "embed"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ embedArg, embedArg0 ]
    , object =
        \objectArg objectArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "object"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ objectArg, objectArg0 ]
    , param =
        \paramArg paramArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "param"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ paramArg, paramArg0 ]
    , ins =
        \insArg insArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "ins"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ insArg, insArg0 ]
    , del =
        \delArg delArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "del"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ delArg, delArg0 ]
    , small =
        \smallArg smallArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "small"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ smallArg, smallArg0 ]
    , cite =
        \citeArg citeArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "cite"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ citeArg, citeArg0 ]
    , dfn =
        \dfnArg dfnArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "dfn"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ dfnArg, dfnArg0 ]
    , abbr =
        \abbrArg abbrArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "abbr"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ abbrArg, abbrArg0 ]
    , time =
        \timeArg timeArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "time"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ timeArg, timeArg0 ]
    , var =
        \varArg varArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "var"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ varArg, varArg0 ]
    , samp =
        \sampArg sampArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "samp"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ sampArg, sampArg0 ]
    , kbd =
        \kbdArg kbdArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "kbd"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ kbdArg, kbdArg0 ]
    , s =
        \sArg sArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "s"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ sArg, sArg0 ]
    , q =
        \qArg qArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "q"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ qArg, qArg0 ]
    , mark =
        \markArg markArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "mark"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ markArg, markArg0 ]
    , ruby =
        \rubyArg rubyArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "ruby"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ rubyArg, rubyArg0 ]
    , rt =
        \rtArg rtArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "rt"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ rtArg, rtArg0 ]
    , rp =
        \rpArg rpArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "rp"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ rpArg, rpArg0 ]
    , bdi =
        \bdiArg bdiArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "bdi"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ bdiArg, bdiArg0 ]
    , bdo =
        \bdoArg bdoArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "bdo"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ bdoArg, bdoArg0 ]
    , wbr =
        \wbrArg wbrArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "wbr"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ wbrArg, wbrArg0 ]
    , details =
        \detailsArg detailsArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "details"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ detailsArg, detailsArg0 ]
    , summary =
        \summaryArg summaryArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "summary"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ summaryArg, summaryArg0 ]
    , menuitem =
        \menuitemArg menuitemArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "menuitem"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ menuitemArg, menuitemArg0 ]
    , menu =
        \menuArg menuArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html" ]
                     , name = "menu"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ menuArg, menuArg0 ]
    }


values_ :
    { text : Elm.Expression
    , node : Elm.Expression
    , map : Elm.Expression
    , h1 : Elm.Expression
    , h2 : Elm.Expression
    , h3 : Elm.Expression
    , h4 : Elm.Expression
    , h5 : Elm.Expression
    , h6 : Elm.Expression
    , div : Elm.Expression
    , p : Elm.Expression
    , hr : Elm.Expression
    , pre : Elm.Expression
    , blockquote : Elm.Expression
    , span : Elm.Expression
    , a : Elm.Expression
    , code : Elm.Expression
    , em : Elm.Expression
    , strong : Elm.Expression
    , i : Elm.Expression
    , b : Elm.Expression
    , u : Elm.Expression
    , sub : Elm.Expression
    , sup : Elm.Expression
    , br : Elm.Expression
    , ol : Elm.Expression
    , ul : Elm.Expression
    , li : Elm.Expression
    , dl : Elm.Expression
    , dt : Elm.Expression
    , dd : Elm.Expression
    , img : Elm.Expression
    , iframe : Elm.Expression
    , canvas : Elm.Expression
    , math : Elm.Expression
    , form : Elm.Expression
    , input : Elm.Expression
    , textarea : Elm.Expression
    , button : Elm.Expression
    , select : Elm.Expression
    , option : Elm.Expression
    , section : Elm.Expression
    , nav : Elm.Expression
    , article : Elm.Expression
    , aside : Elm.Expression
    , header : Elm.Expression
    , footer : Elm.Expression
    , address : Elm.Expression
    , main_ : Elm.Expression
    , figure : Elm.Expression
    , figcaption : Elm.Expression
    , table : Elm.Expression
    , caption : Elm.Expression
    , colgroup : Elm.Expression
    , col : Elm.Expression
    , tbody : Elm.Expression
    , thead : Elm.Expression
    , tfoot : Elm.Expression
    , tr : Elm.Expression
    , td : Elm.Expression
    , th : Elm.Expression
    , fieldset : Elm.Expression
    , legend : Elm.Expression
    , label : Elm.Expression
    , datalist : Elm.Expression
    , optgroup : Elm.Expression
    , output : Elm.Expression
    , progress : Elm.Expression
    , meter : Elm.Expression
    , audio : Elm.Expression
    , video : Elm.Expression
    , source : Elm.Expression
    , track : Elm.Expression
    , embed : Elm.Expression
    , object : Elm.Expression
    , param : Elm.Expression
    , ins : Elm.Expression
    , del : Elm.Expression
    , small : Elm.Expression
    , cite : Elm.Expression
    , dfn : Elm.Expression
    , abbr : Elm.Expression
    , time : Elm.Expression
    , var : Elm.Expression
    , samp : Elm.Expression
    , kbd : Elm.Expression
    , s : Elm.Expression
    , q : Elm.Expression
    , mark : Elm.Expression
    , ruby : Elm.Expression
    , rt : Elm.Expression
    , rp : Elm.Expression
    , bdi : Elm.Expression
    , bdo : Elm.Expression
    , wbr : Elm.Expression
    , details : Elm.Expression
    , summary : Elm.Expression
    , menuitem : Elm.Expression
    , menu : Elm.Expression
    }
values_ =
    { text =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                         [ Type.string ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , node =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "node"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                         [ Type.function [ Type.var "a" ] (Type.var "msg")
                         , Type.namedWith [ "Html" ] "Html" [ Type.var "a" ]
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , h1 =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "h1"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , h2 =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "h2"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , h3 =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "h3"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , h4 =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "h4"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , h5 =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "h5"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , h6 =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "h6"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , div =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "div"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , p =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "p"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , hr =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "hr"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , pre =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "pre"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , blockquote =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "blockquote"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , span =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "span"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , a =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "a"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , code =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "code"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , em =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "em"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , strong =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "strong"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , i =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "i"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , b =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "b"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , u =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "u"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , sub =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "sub"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , sup =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "sup"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , br =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "br"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , ol =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "ol"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , ul =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "ul"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , li =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "li"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , dl =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "dl"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , dt =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "dt"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , dd =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "dd"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , img =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "img"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , iframe =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "iframe"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , canvas =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "canvas"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , math =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "math"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , form =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "form"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , input =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "input"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , textarea =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "textarea"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , button =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , select =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "select"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , option =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "option"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , section =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "section"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , nav =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "nav"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , article =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "article"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , aside =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "aside"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , header =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "header"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , footer =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "footer"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , address =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "address"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , main_ =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "main_"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , figure =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "figure"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , figcaption =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "figcaption"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , table =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "table"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , caption =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "caption"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , colgroup =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "colgroup"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , col =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "col"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , tbody =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "tbody"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , thead =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "thead"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , tfoot =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "tfoot"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , tr =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "tr"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , td =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "td"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , th =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "th"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , fieldset =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "fieldset"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , legend =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "legend"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , label =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "label"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , datalist =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "datalist"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , optgroup =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "optgroup"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , output =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "output"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , progress =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "progress"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , meter =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "meter"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , audio =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "audio"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , video =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "video"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , source =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "source"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , track =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "track"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , embed =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "embed"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , object =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "object"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , param =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "param"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , ins =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "ins"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , del =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "del"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , small =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "small"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , cite =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "cite"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , dfn =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "dfn"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , abbr =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "abbr"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , time =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "time"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , var =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "var"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , samp =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "samp"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , kbd =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "kbd"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , s =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "s"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , q =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "q"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , mark =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "mark"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , ruby =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "ruby"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , rt =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "rt"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , rp =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "rp"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , bdi =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "bdi"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , bdo =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "bdo"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , wbr =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "wbr"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , details =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "details"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , summary =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "summary"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , menuitem =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "menuitem"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    , menu =
        Elm.value
            { importFrom = [ "Html" ]
            , name = "menu"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.namedWith
                                [ "Html" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         ]
                         (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ])
                    )
            }
    }