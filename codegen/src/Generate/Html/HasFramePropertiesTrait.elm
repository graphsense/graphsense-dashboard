module Generate.Html.HasFramePropertiesTrait exposing (..)

import Api.Raw exposing (..)
import Basics.Extra exposing (flip)
import Elm
import Gen.Css as Css
import Generate.Util exposing (..)
import Generate.Util.RGBA as RGBA


toStyles : HasFramePropertiesTrait -> List Elm.Expression
toStyles node =
    []
        |> m (RGBA.toStyles >> Css.backgroundColor) node.backgroundColor
        |> m (Css.px >> Css.paddingLeft) node.paddingLeft
        |> m (Css.px >> Css.paddingRight) node.paddingRight
        |> m (Css.px >> Css.paddingTop) node.paddingTop
        |> m (Css.px >> Css.paddingBottom) node.paddingBottom
        |> mm layoutMode node.layoutMode
        |> m primaryAxisAlignItems node.primaryAxisAlignItems
        |> m counterAxisAlignItems node.counterAxisAlignItems
        |> a2 gap node.primaryAxisAlignItems node.itemSpacing


gap : PrimaryAxisAlignItems -> Float -> Maybe Elm.Expression
gap axis itemSpacing =
    if axis == PrimaryAxisAlignItemsSPACEBETWEEN then
        Nothing

    else
        String.fromFloat itemSpacing
            ++ "px"
            |> Css.property "gap"
            |> Just


primaryAxisAlignItems : PrimaryAxisAlignItems -> Elm.Expression
primaryAxisAlignItems axis =
    case axis of
        PrimaryAxisAlignItemsMIN ->
            Css.call_.justifyContent Css.start

        PrimaryAxisAlignItemsMAX ->
            Css.call_.justifyContent Css.end

        PrimaryAxisAlignItemsCENTER ->
            Css.call_.justifyContent Css.center

        PrimaryAxisAlignItemsSPACEBETWEEN ->
            Css.call_.justifyContent Css.spaceBetween


counterAxisAlignItems : CounterAxisAlignItems -> Elm.Expression
counterAxisAlignItems axis =
    case axis of
        CounterAxisAlignItemsMIN ->
            Css.call_.alignItems Css.start

        CounterAxisAlignItemsMAX ->
            Css.call_.alignItems Css.end

        CounterAxisAlignItemsCENTER ->
            Css.call_.alignItems Css.center

        CounterAxisAlignItemsBASELINE ->
            Css.call_.alignItems Css.baseline


layoutMode : LayoutMode -> List Elm.Expression
layoutMode mode =
    case mode of
        LayoutModeHORIZONTAL ->
            [ Css.displayFlex
            , Css.flexDirection Css.row
            ]

        LayoutModeVERTICAL ->
            [ Css.displayFlex
            , Css.flexDirection Css.column
            ]

        LayoutModeNONE ->
            [ Css.position Css.relative
            ]



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
