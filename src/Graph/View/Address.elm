module Graph.View.Address exposing (address)

import Graph.Css as Css
import Graph.Model.Address exposing (Address)
import Graph.Model.Id as Id
import Graph.Msg exposing (Msg(..))
import Graph.View.Config as Graph exposing (AddressLabelType(..))
import Graph.View.Label as Label
import Graph.View.Util exposing (translate)
import Json.Decode as Dec
import Locale.View as Locale
import Svg.Styled exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events exposing (..)
import View.Config exposing (Config)


address : Config -> Graph.Config -> Address -> Svg Msg
address vc gc addr =
    g
        [ Css.addressRoot vc |> css
        , x <| String.fromFloat addr.x
        , y <| String.fromFloat addr.y
        , UserClickedAddress addr.id
            |> onClick
        , UserRightClickedAddress addr.id
            |> Dec.succeed
            |> on "contextmenu"
        , UserHoversAddress addr.id
            |> onMouseOver
        , UserLeavesAddress addr.id
            |> onMouseOut
        ]
        [ rect
            [ width <| String.fromFloat Graph.addressWidth
            , height <| String.fromFloat Graph.addressHeight
            ]
            []
        , label vc gc addr
        , flags vc gc addr
        ]


label : Config -> Graph.Config -> Address -> Svg Msg
label vc gc addr =
    g
        [ Css.addressLabel vc |> css
        , Graph.addressHeight
            / 2
            + Graph.labelHeight
            / 3
            |> translate Graph.padding
            |> transform
        ]
        [ getLabel vc gc addr
            |> Label.label vc gc
        ]


getLabel : Config -> Graph.Config -> Address -> String
getLabel vc gc addr =
    case gc.addressLabelType of
        ID ->
            addr.address.address
                |> String.left 8

        Balance ->
            addr.address.balance
                |> Locale.currency vc.locale (Id.currency addr.id)

        Tag ->
            "todo"


flags : Config -> Graph.Config -> Address -> Svg Msg
flags vc gc addr =
    g
        [ Css.addressFlags vc |> css
        , Graph.padding
            / 2
            |> translate (Graph.addressWidth - Graph.padding / 2)
            |> transform
        ]
        []
