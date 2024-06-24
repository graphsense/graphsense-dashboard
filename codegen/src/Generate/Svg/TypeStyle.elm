module Generate.Svg.TypeStyle exposing (..)

import Api.Raw exposing (..)
import Elm
import Gen.Css as Css
import Generate.Util.Paint as Paint
import Generate.Util exposing (..)


toCss : TypeStyle -> List Elm.Expression
toCss node =
    []
        |> m (List.singleton >> Css.fontFamilies) node.fontFamily
        --|> m italic node.italic
        |> m (round >> Css.int >> intOrAutoType >> Css.fontWeight) node.fontWeight
        |> m (Css.px >> Css.fontSize) node.fontSize
        |> a (Paint.toCss >> Maybe.map Css.color) node.fills
        |> m (Css.px >> Css.letterSpacing) node.letterSpacing
        |> m (Css.px >> Css.lineHeight) node.lineHeightPx



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
