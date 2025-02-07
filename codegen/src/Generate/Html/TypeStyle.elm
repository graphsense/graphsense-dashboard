module Generate.Html.TypeStyle exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Generate.Util.Paint as Paint
import Types exposing (ColorMap)


toStyles : ColorMap -> TypeStyle -> List Elm.Expression
toStyles colorMap node =
    []
        |> m (List.singleton >> Css.fontFamilies) node.fontFamily
        |> m textAlign node.textAlignHorizontal
        --|> m italic node.italic
        |> m (round >> Css.int >> intOrAutoType >> Css.fontWeight) node.fontWeight
        |> m (Css.px >> Css.fontSize) node.fontSize
        |> a (Paint.toStylesString colorMap >> Maybe.map (Css.property "color")) node.fills
        |> m (Css.px >> Css.letterSpacing) node.letterSpacing
        |> m (Css.px >> Css.lineHeight) node.lineHeightPx
        |> mm textAutoResize node.textAutoResize
        |> m textDecoration node.textDecoration


textDecoration : TypeStyleTextDecoration -> Elm.Expression
textDecoration arg1 =
    case arg1 of
        TypeStyleTextDecorationNONE ->
            Css.property "text-decoration" "none"

        TypeStyleTextDecorationSTRIKETHROUGH ->
            Css.property "text-decoration" "strike-through"

        TypeStyleTextDecorationUNDERLINE ->
            Css.property "text-decoration" "underline"


textAlign : TypeStyleTextAlignHorizontal -> Elm.Expression
textAlign arg1 =
    case arg1 of
        TypeStyleTextAlignHorizontalCENTER ->
            Css.property "text-align" "center"

        TypeStyleTextAlignHorizontalLEFT ->
            Css.property "text-align" "left"

        TypeStyleTextAlignHorizontalRIGHT ->
            Css.property "text-align" "right"

        TypeStyleTextAlignHorizontalJUSTIFIED ->
            Css.property "text-align" "justifed"


textAutoResize : TypeStyleTextAutoResize -> List Elm.Expression
textAutoResize arg1 =
    case arg1 of
        TypeStyleTextAutoResizeHEIGHT ->
            [ Css.property "white-space" "wrap"
            , Css.property "word-break" "break-word"
            ]

        _ ->
            [ Css.whiteSpace Css.noWrap
            ]



{-
   , fontWeight : Maybe Float
   , fontSize : Maybe Float
   , textCase : Maybe TypeStyleTextCase
   , textDecoration : Maybe TypeStyleTextDecoration
   , textAutoResize : Maybe TypeStyleTextAutoResize
   , textTruncation : Maybe TypeStyleTextTruncation
   , maxLines : Maybe Float
   , textAlignHorizontal : Maybe TypeStyleTextAlignHorizontal
   , textAlignVertical : Maybe TypeStyleTextAlignVertical
   , letterSpacing : Maybe Float
   , fills : Maybe (List Paint)
   , hyperlink : Maybe Hyperlink
   , opentypeFlags : Maybe Float
   , lineHeightPx : Maybe Float
   , lineHeightPercent : Maybe Float
   , lineHeightPercentFontSize : Maybe Float
   , lineHeightUnit : Maybe TypeStyleLineHeightUnit
-}
