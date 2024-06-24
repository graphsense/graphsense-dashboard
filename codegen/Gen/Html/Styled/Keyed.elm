module Gen.Html.Styled.Keyed exposing (call_, lazyNode, lazyNode2, lazyNode3, lazyNode4, lazyNode5, lazyNode6, moduleName_, node, ol, ul, values_)

{-| 
@docs moduleName_, node, ol, ul, lazyNode, lazyNode2, lazyNode3, lazyNode4, lazyNode5, lazyNode6, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Html", "Styled", "Keyed" ]


{-| Works just like `Html.node`, but you add a unique identifier to each child
node. You want this when you have a list of nodes that is changing: adding
nodes, removing nodes, etc. In these cases, the unique identifiers help make
the DOM modifications more efficient.

node: 
    String
    -> List (Html.Styled.Attribute msg)
    -> List ( String, Html.Styled.Html msg )
    -> Html.Styled.Html msg
-}
node : String -> List Elm.Expression -> List Elm.Expression -> Elm.Expression
node nodeArg nodeArg0 nodeArg1 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "node"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.tuple
                                 Type.string
                                 (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                 )
                              )
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.string nodeArg, Elm.list nodeArg0, Elm.list nodeArg1 ]


{-| ol: 
    List (Html.Styled.Attribute msg)
    -> List ( String, Html.Styled.Html msg )
    -> Html.Styled.Html msg
-}
ol : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ol olArg olArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "ol"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.tuple
                                 Type.string
                                 (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                 )
                              )
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.list olArg, Elm.list olArg0 ]


{-| ul: 
    List (Html.Styled.Attribute msg)
    -> List ( String, Html.Styled.Html msg )
    -> Html.Styled.Html msg
-}
ul : List Elm.Expression -> List Elm.Expression -> Elm.Expression
ul ulArg ulArg0 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "ul"
             , annotation =
                 Just
                     (Type.function
                          [ Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.tuple
                                 Type.string
                                 (Type.namedWith
                                    [ "Html", "Styled" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                 )
                              )
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.list ulArg, Elm.list ulArg0 ]


{-| Creates a node that has children that are both keyed **and** lazy.

The unique key for each child serves double duty:

  - The key helps the Elm runtime make DOM modifications more efficient
  - The key becomes the id of child and the css generated by lazy becomes scoped to that id allowing the browser to save time calculating styles

Some notes about using this function:

  - The key must be a valid HTML id
  - The key should be unique among other ids on the page and unique among the keys for other siblings
  - No other id attributes should be specified on the keyed child nodes - they will be ignored

lazyNode: 
    String
    -> List (Html.Styled.Attribute msg)
    -> (a -> Html.Styled.Html msg)
    -> List ( String, a )
    -> Html.Styled.Html msg
-}
lazyNode :
    String
    -> List Elm.Expression
    -> (Elm.Expression -> Elm.Expression)
    -> List Elm.Expression
    -> Elm.Expression
lazyNode lazyNodeArg lazyNodeArg0 lazyNodeArg1 lazyNodeArg2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "lazyNode"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.function
                              [ Type.var "a" ]
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          , Type.list (Type.tuple Type.string (Type.var "a"))
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.string lazyNodeArg
        , Elm.list lazyNodeArg0
        , Elm.functionReduced "lazyNodeUnpack" lazyNodeArg1
        , Elm.list lazyNodeArg2
        ]


{-| Same as `lazyNode`, but checks on 2 arguments.

lazyNode2: 
    String
    -> List (Html.Styled.Attribute msg)
    -> (a -> b -> Html.Styled.Html msg)
    -> List ( String, ( a, b ) )
    -> Html.Styled.Html msg
-}
lazyNode2 :
    String
    -> List Elm.Expression
    -> (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> List Elm.Expression
    -> Elm.Expression
lazyNode2 lazyNode2Arg lazyNode2Arg0 lazyNode2Arg1 lazyNode2Arg2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "lazyNode2"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.function
                              [ Type.var "a", Type.var "b" ]
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.tuple
                                 Type.string
                                 (Type.tuple (Type.var "a") (Type.var "b"))
                              )
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.string lazyNode2Arg
        , Elm.list lazyNode2Arg0
        , Elm.functionReduced
            "lazyNode2Unpack"
            (\functionReducedUnpack ->
               Elm.functionReduced
                   "unpack"
                   (lazyNode2Arg1 functionReducedUnpack)
            )
        , Elm.list lazyNode2Arg2
        ]


{-| Same as `lazyNode`, but checks on 3 arguments.

lazyNode3: 
    String
    -> List (Html.Styled.Attribute msg)
    -> (a -> b -> c -> Html.Styled.Html msg)
    -> List ( String, ( a, b, c ) )
    -> Html.Styled.Html msg
-}
lazyNode3 :
    String
    -> List Elm.Expression
    -> (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> List Elm.Expression
    -> Elm.Expression
lazyNode3 lazyNode3Arg lazyNode3Arg0 lazyNode3Arg1 lazyNode3Arg2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "lazyNode3"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.function
                              [ Type.var "a", Type.var "b", Type.var "c" ]
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.tuple
                                 Type.string
                                 (Type.triple
                                    (Type.var "a")
                                    (Type.var "b")
                                    (Type.var "c")
                                 )
                              )
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.string lazyNode3Arg
        , Elm.list lazyNode3Arg0
        , Elm.functionReduced
            "lazyNode3Unpack"
            (\functionReducedUnpack ->
               Elm.functionReduced
                   "unpack"
                   (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            ((lazyNode3Arg1 functionReducedUnpack)
                                 functionReducedUnpack0
                            )
                   )
            )
        , Elm.list lazyNode3Arg2
        ]


{-| Same as `lazyNode`, but checks on 4 arguments.

lazyNode4: 
    String
    -> List (Html.Styled.Attribute msg)
    -> (a -> b -> c -> d -> Html.Styled.Html msg)
    -> List ( String, { arg1 : a, arg2 : b, arg3 : c, arg4 : d } )
    -> Html.Styled.Html msg
-}
lazyNode4 :
    String
    -> List Elm.Expression
    -> (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> List Elm.Expression
    -> Elm.Expression
lazyNode4 lazyNode4Arg lazyNode4Arg0 lazyNode4Arg1 lazyNode4Arg2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "lazyNode4"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.function
                              [ Type.var "a"
                              , Type.var "b"
                              , Type.var "c"
                              , Type.var "d"
                              ]
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.tuple
                                 Type.string
                                 (Type.record
                                    [ ( "arg1", Type.var "a" )
                                    , ( "arg2", Type.var "b" )
                                    , ( "arg3", Type.var "c" )
                                    , ( "arg4", Type.var "d" )
                                    ]
                                 )
                              )
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.string lazyNode4Arg
        , Elm.list lazyNode4Arg0
        , Elm.functionReduced
            "lazyNode4Unpack"
            (\functionReducedUnpack ->
               Elm.functionReduced
                   "unpack"
                   (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_2_2_0_0 ->
                                 Elm.functionReduced
                                     "unpack"
                                     (((lazyNode4Arg1 functionReducedUnpack)
                                           functionReducedUnpack0
                                      )
                                          functionReducedUnpack_2_1_2_0_2_2_2_0_0
                                     )
                            )
                   )
            )
        , Elm.list lazyNode4Arg2
        ]


{-| Same as `lazyNode`, but checks on 5 arguments.

lazyNode5: 
    String
    -> List (Html.Styled.Attribute msg)
    -> (a -> b -> c -> d -> e -> Html.Styled.Html msg)
    -> List ( String, { arg1 : a, arg2 : b, arg3 : c, arg4 : d, arg5 : e } )
    -> Html.Styled.Html msg
-}
lazyNode5 :
    String
    -> List Elm.Expression
    -> (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> List Elm.Expression
    -> Elm.Expression
lazyNode5 lazyNode5Arg lazyNode5Arg0 lazyNode5Arg1 lazyNode5Arg2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "lazyNode5"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.function
                              [ Type.var "a"
                              , Type.var "b"
                              , Type.var "c"
                              , Type.var "d"
                              , Type.var "e"
                              ]
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.tuple
                                 Type.string
                                 (Type.record
                                    [ ( "arg1", Type.var "a" )
                                    , ( "arg2", Type.var "b" )
                                    , ( "arg3", Type.var "c" )
                                    , ( "arg4", Type.var "d" )
                                    , ( "arg5", Type.var "e" )
                                    ]
                                 )
                              )
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.string lazyNode5Arg
        , Elm.list lazyNode5Arg0
        , Elm.functionReduced
            "lazyNode5Unpack"
            (\functionReducedUnpack ->
               Elm.functionReduced
                   "unpack"
                   (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_2_2_0_0 ->
                                 Elm.functionReduced
                                     "unpack"
                                     (\functionReducedUnpack_2_1_2_1_2_0_2_2_2_0_0 ->
                                          Elm.functionReduced
                                              "unpack"
                                              ((((lazyNode5Arg1
                                                      functionReducedUnpack
                                                 )
                                                     functionReducedUnpack0
                                                )
                                                    functionReducedUnpack_2_1_2_0_2_2_2_0_0
                                               )
                                                   functionReducedUnpack_2_1_2_1_2_0_2_2_2_0_0
                                              )
                                     )
                            )
                   )
            )
        , Elm.list lazyNode5Arg2
        ]


{-| Same as `lazyNode`, but checks on 6 arguments.

lazyNode6: 
    String
    -> List (Html.Styled.Attribute msg)
    -> (a -> b -> c -> d -> e -> f -> Html.Styled.Html msg)
    -> List ( String, { arg1 : a
    , arg2 : b
    , arg3 : c
    , arg4 : d
    , arg5 : e
    , arg6 : f
    } )
    -> Html.Styled.Html msg
-}
lazyNode6 :
    String
    -> List Elm.Expression
    -> (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> List Elm.Expression
    -> Elm.Expression
lazyNode6 lazyNode6Arg lazyNode6Arg0 lazyNode6Arg1 lazyNode6Arg2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Html", "Styled", "Keyed" ]
             , name = "lazyNode6"
             , annotation =
                 Just
                     (Type.function
                          [ Type.string
                          , Type.list
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Attribute"
                                 [ Type.var "msg" ]
                              )
                          , Type.function
                              [ Type.var "a"
                              , Type.var "b"
                              , Type.var "c"
                              , Type.var "d"
                              , Type.var "e"
                              , Type.var "f"
                              ]
                              (Type.namedWith
                                 [ "Html", "Styled" ]
                                 "Html"
                                 [ Type.var "msg" ]
                              )
                          , Type.list
                              (Type.tuple
                                 Type.string
                                 (Type.record
                                    [ ( "arg1", Type.var "a" )
                                    , ( "arg2", Type.var "b" )
                                    , ( "arg3", Type.var "c" )
                                    , ( "arg4", Type.var "d" )
                                    , ( "arg5", Type.var "e" )
                                    , ( "arg6", Type.var "f" )
                                    ]
                                 )
                              )
                          ]
                          (Type.namedWith
                               [ "Html", "Styled" ]
                               "Html"
                               [ Type.var "msg" ]
                          )
                     )
             }
        )
        [ Elm.string lazyNode6Arg
        , Elm.list lazyNode6Arg0
        , Elm.functionReduced
            "lazyNode6Unpack"
            (\functionReducedUnpack ->
               Elm.functionReduced
                   "unpack"
                   (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_2_2_0_0 ->
                                 Elm.functionReduced
                                     "unpack"
                                     (\functionReducedUnpack_2_1_2_1_2_0_2_2_2_0_0 ->
                                          Elm.functionReduced
                                              "unpack"
                                              (\functionReducedUnpack_2_1_2_1_2_1_2_0_2_2_2_0_0 ->
                                                   Elm.functionReduced
                                                       "unpack"
                                                       (((((lazyNode6Arg1
                                                                functionReducedUnpack
                                                           )
                                                               functionReducedUnpack0
                                                          )
                                                              functionReducedUnpack_2_1_2_0_2_2_2_0_0
                                                         )
                                                             functionReducedUnpack_2_1_2_1_2_0_2_2_2_0_0
                                                        )
                                                            functionReducedUnpack_2_1_2_1_2_1_2_0_2_2_2_0_0
                                                       )
                                              )
                                     )
                            )
                   )
            )
        , Elm.list lazyNode6Arg2
        ]


call_ :
    { node :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , ol : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ul : Elm.Expression -> Elm.Expression -> Elm.Expression
    , lazyNode :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , lazyNode2 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , lazyNode3 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , lazyNode4 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , lazyNode5 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , lazyNode6 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    }
call_ =
    { node =
        \nodeArg nodeArg0 nodeArg1 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "node"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple
                                         Type.string
                                         (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                         )
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ nodeArg, nodeArg0, nodeArg1 ]
    , ol =
        \olArg olArg0 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "ol"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple
                                         Type.string
                                         (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                         )
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
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
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "ul"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple
                                         Type.string
                                         (Type.namedWith
                                            [ "Html", "Styled" ]
                                            "Html"
                                            [ Type.var "msg" ]
                                         )
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ ulArg, ulArg0 ]
    , lazyNode =
        \lazyNodeArg lazyNodeArg0 lazyNodeArg1 lazyNodeArg2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "lazyNode"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.function
                                      [ Type.var "a" ]
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple Type.string (Type.var "a"))
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ lazyNodeArg, lazyNodeArg0, lazyNodeArg1, lazyNodeArg2 ]
    , lazyNode2 =
        \lazyNode2Arg lazyNode2Arg0 lazyNode2Arg1 lazyNode2Arg2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "lazyNode2"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.function
                                      [ Type.var "a", Type.var "b" ]
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple
                                         Type.string
                                         (Type.tuple
                                            (Type.var "a")
                                            (Type.var "b")
                                         )
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ lazyNode2Arg, lazyNode2Arg0, lazyNode2Arg1, lazyNode2Arg2 ]
    , lazyNode3 =
        \lazyNode3Arg lazyNode3Arg0 lazyNode3Arg1 lazyNode3Arg2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "lazyNode3"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.function
                                      [ Type.var "a"
                                      , Type.var "b"
                                      , Type.var "c"
                                      ]
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple
                                         Type.string
                                         (Type.triple
                                            (Type.var "a")
                                            (Type.var "b")
                                            (Type.var "c")
                                         )
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ lazyNode3Arg, lazyNode3Arg0, lazyNode3Arg1, lazyNode3Arg2 ]
    , lazyNode4 =
        \lazyNode4Arg lazyNode4Arg0 lazyNode4Arg1 lazyNode4Arg2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "lazyNode4"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.function
                                      [ Type.var "a"
                                      , Type.var "b"
                                      , Type.var "c"
                                      , Type.var "d"
                                      ]
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple
                                         Type.string
                                         (Type.record
                                            [ ( "arg1", Type.var "a" )
                                            , ( "arg2", Type.var "b" )
                                            , ( "arg3", Type.var "c" )
                                            , ( "arg4", Type.var "d" )
                                            ]
                                         )
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ lazyNode4Arg, lazyNode4Arg0, lazyNode4Arg1, lazyNode4Arg2 ]
    , lazyNode5 =
        \lazyNode5Arg lazyNode5Arg0 lazyNode5Arg1 lazyNode5Arg2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "lazyNode5"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.function
                                      [ Type.var "a"
                                      , Type.var "b"
                                      , Type.var "c"
                                      , Type.var "d"
                                      , Type.var "e"
                                      ]
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple
                                         Type.string
                                         (Type.record
                                            [ ( "arg1", Type.var "a" )
                                            , ( "arg2", Type.var "b" )
                                            , ( "arg3", Type.var "c" )
                                            , ( "arg4", Type.var "d" )
                                            , ( "arg5", Type.var "e" )
                                            ]
                                         )
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ lazyNode5Arg, lazyNode5Arg0, lazyNode5Arg1, lazyNode5Arg2 ]
    , lazyNode6 =
        \lazyNode6Arg lazyNode6Arg0 lazyNode6Arg1 lazyNode6Arg2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Html", "Styled", "Keyed" ]
                     , name = "lazyNode6"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.string
                                  , Type.list
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Attribute"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.function
                                      [ Type.var "a"
                                      , Type.var "b"
                                      , Type.var "c"
                                      , Type.var "d"
                                      , Type.var "e"
                                      , Type.var "f"
                                      ]
                                      (Type.namedWith
                                         [ "Html", "Styled" ]
                                         "Html"
                                         [ Type.var "msg" ]
                                      )
                                  , Type.list
                                      (Type.tuple
                                         Type.string
                                         (Type.record
                                            [ ( "arg1", Type.var "a" )
                                            , ( "arg2", Type.var "b" )
                                            , ( "arg3", Type.var "c" )
                                            , ( "arg4", Type.var "d" )
                                            , ( "arg5", Type.var "e" )
                                            , ( "arg6", Type.var "f" )
                                            ]
                                         )
                                      )
                                  ]
                                  (Type.namedWith
                                       [ "Html", "Styled" ]
                                       "Html"
                                       [ Type.var "msg" ]
                                  )
                             )
                     }
                )
                [ lazyNode6Arg, lazyNode6Arg0, lazyNode6Arg1, lazyNode6Arg2 ]
    }


values_ :
    { node : Elm.Expression
    , ol : Elm.Expression
    , ul : Elm.Expression
    , lazyNode : Elm.Expression
    , lazyNode2 : Elm.Expression
    , lazyNode3 : Elm.Expression
    , lazyNode4 : Elm.Expression
    , lazyNode5 : Elm.Expression
    , lazyNode6 : Elm.Expression
    }
values_ =
    { node =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "node"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.tuple
                                Type.string
                                (Type.namedWith
                                   [ "Html", "Styled" ]
                                   "Html"
                                   [ Type.var "msg" ]
                                )
                             )
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    , ol =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "ol"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.tuple
                                Type.string
                                (Type.namedWith
                                   [ "Html", "Styled" ]
                                   "Html"
                                   [ Type.var "msg" ]
                                )
                             )
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    , ul =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "ul"
            , annotation =
                Just
                    (Type.function
                         [ Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.tuple
                                Type.string
                                (Type.namedWith
                                   [ "Html", "Styled" ]
                                   "Html"
                                   [ Type.var "msg" ]
                                )
                             )
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    , lazyNode =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "lazyNode"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.function
                             [ Type.var "a" ]
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         , Type.list (Type.tuple Type.string (Type.var "a"))
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    , lazyNode2 =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "lazyNode2"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.function
                             [ Type.var "a", Type.var "b" ]
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.tuple
                                Type.string
                                (Type.tuple (Type.var "a") (Type.var "b"))
                             )
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    , lazyNode3 =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "lazyNode3"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.function
                             [ Type.var "a", Type.var "b", Type.var "c" ]
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.tuple
                                Type.string
                                (Type.triple
                                   (Type.var "a")
                                   (Type.var "b")
                                   (Type.var "c")
                                )
                             )
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    , lazyNode4 =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "lazyNode4"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.function
                             [ Type.var "a"
                             , Type.var "b"
                             , Type.var "c"
                             , Type.var "d"
                             ]
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.tuple
                                Type.string
                                (Type.record
                                   [ ( "arg1", Type.var "a" )
                                   , ( "arg2", Type.var "b" )
                                   , ( "arg3", Type.var "c" )
                                   , ( "arg4", Type.var "d" )
                                   ]
                                )
                             )
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    , lazyNode5 =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "lazyNode5"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.function
                             [ Type.var "a"
                             , Type.var "b"
                             , Type.var "c"
                             , Type.var "d"
                             , Type.var "e"
                             ]
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.tuple
                                Type.string
                                (Type.record
                                   [ ( "arg1", Type.var "a" )
                                   , ( "arg2", Type.var "b" )
                                   , ( "arg3", Type.var "c" )
                                   , ( "arg4", Type.var "d" )
                                   , ( "arg5", Type.var "e" )
                                   ]
                                )
                             )
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    , lazyNode6 =
        Elm.value
            { importFrom = [ "Html", "Styled", "Keyed" ]
            , name = "lazyNode6"
            , annotation =
                Just
                    (Type.function
                         [ Type.string
                         , Type.list
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Attribute"
                                [ Type.var "msg" ]
                             )
                         , Type.function
                             [ Type.var "a"
                             , Type.var "b"
                             , Type.var "c"
                             , Type.var "d"
                             , Type.var "e"
                             , Type.var "f"
                             ]
                             (Type.namedWith
                                [ "Html", "Styled" ]
                                "Html"
                                [ Type.var "msg" ]
                             )
                         , Type.list
                             (Type.tuple
                                Type.string
                                (Type.record
                                   [ ( "arg1", Type.var "a" )
                                   , ( "arg2", Type.var "b" )
                                   , ( "arg3", Type.var "c" )
                                   , ( "arg4", Type.var "d" )
                                   , ( "arg5", Type.var "e" )
                                   , ( "arg6", Type.var "f" )
                                   ]
                                )
                             )
                         ]
                         (Type.namedWith
                              [ "Html", "Styled" ]
                              "Html"
                              [ Type.var "msg" ]
                         )
                    )
            }
    }