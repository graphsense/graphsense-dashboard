module View.Pathfinder.Details exposing (closeAttrs, valuesToCell)

import Api.Data
import Config.View as View
import Css
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Model.Currency as Currency
import Msg.Pathfinder exposing (Msg(..))
import Svg.Styled
import View.Locale as Locale


valuesToCell : View.Config -> Currency.AssetIdentifier -> Api.Data.Values -> { firstRowText : String, secondRowText : String, secondRowVisible : Bool }
valuesToCell vc asset value =
    { firstRowText = Locale.currency vc.locale [ ( asset, value ) ]
    , secondRowText = ""
    , secondRowVisible = False
    }


closeAttrs : List (Svg.Styled.Attribute Msg)
closeAttrs =
    [ css
        [ Css.cursor Css.pointer
        , Css.important <| Css.right <| Css.px 6
        , Css.important <| Css.top <| Css.px 0
        , Css.important <| Css.left <| Css.unset
        ]
    , onClick UserClosedDetailsView
    ]
