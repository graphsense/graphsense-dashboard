module Util exposing (allAndNotEmpty, and, andWithCmd, foldLigatures, n, removeLeading0x)


removeLeading0x : String -> String
removeLeading0x s =
    if String.startsWith "0x" s then
        s |> String.dropLeft 2

    else
        s


{-| Expand unicode typographic ligatures to their letter pairs.

PDF typography renders letter pairs like "ff" as a single ligature glyph
(U+FB00 "ﬀ"), so identifiers copy-pasted from reports may contain them.
Only the ligature codepoints are expanded — case is preserved, since
base58 identifiers are case-sensitive.

-}
foldLigatures : String -> String
foldLigatures s =
    List.foldl (\( lig, repl ) -> String.replace lig repl)
        s
        [ ( "ﬀ", "ff" )
        , ( "ﬁ", "fi" )
        , ( "ﬂ", "fl" )
        , ( "ﬃ", "ffi" )
        , ( "ﬄ", "ffl" )
        , ( "ﬅ", "st" )
        , ( "ﬆ", "st" )
        ]


n : m -> ( m, List eff )
n m =
    ( m, [] )


and : (m -> ( m, List eff )) -> ( m, List eff ) -> ( m, List eff )
and update ( m, eff ) =
    let
        ( m2, eff2 ) =
            update m
    in
    ( m2
    , eff ++ eff2
    )


andWithCmd : (m -> ( m, Cmd msg )) -> ( m, Cmd msg ) -> ( m, Cmd msg )
andWithCmd fun ( m, cmd ) =
    let
        ( m2, cmd2 ) =
            fun m
    in
    ( m2, Cmd.batch [ cmd, cmd2 ] )


allAndNotEmpty : (a -> Bool) -> List a -> Bool
allAndNotEmpty pred list =
    if List.isEmpty list then
        False

    else
        List.all pred list
