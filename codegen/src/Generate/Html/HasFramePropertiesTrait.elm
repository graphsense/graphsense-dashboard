module Generate.Html.HasFramePropertiesTrait exposing (..)

import Api.Raw exposing (HasFramePropertiesTrait)
import Elm
import Gen.Css as Css
import Generate.Util.RGBA as RGBA
import Generate.Util exposing (..)


toCss : HasFramePropertiesTrait -> List Elm.Expression
toCss node =
    []
        |> m (RGBA.toCss >> Css.backgroundColor) node.backgroundColor
        |> m (Css.px >> Css.paddingLeft) node.paddingLeft
        |> m (Css.px >> Css.paddingRight) node.paddingRight
        |> m (Css.px >> Css.paddingTop) node.paddingTop
        |> m (Css.px >> Css.paddingBottom) node.paddingBottom



{-

   { clipsContent : Bool
   , background : Maybe (List Paint)
   , layoutGrids : Maybe (List LayoutGrid)
   , overflowDirection : Maybe OverflowDirection
   , layoutMode : Maybe LayoutMode
   , primaryAxisSizingMode : Maybe PrimaryAxisSizingMode
   , counterAxisSizingMode : Maybe CounterAxisSizingMode
   , primaryAxisAlignItems : Maybe PrimaryAxisAlignItems
   , counterAxisAlignItems : Maybe CounterAxisAlignItems
   , paddingLeft : Maybe Float
   , paddingRight : Maybe Float
   , paddingTop : Maybe Float
   , paddingBottom : Maybe Float
   , itemSpacing : Maybe Float
   , itemReverseZIndex : Maybe Bool
   , strokesIncludedInLayout : Maybe Bool
   , layoutWrap : Maybe LayoutWrap
   , counterAxisSpacing : Maybe Float
   , counterAxisAlignContent : Maybe CounterAxisAlignContent
-}
