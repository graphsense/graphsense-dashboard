module Scenario.AlignHorizontallyTest exposing (suite)

{-| "Align horizontally" moves every selected node onto the median row of the
selection. The overlap pass that follows must move the nodes that were _not_
selected out of the way, never the aligned ones -- otherwise an unselected node
sitting in the same column just above the target row pushes the aligned node
back off it, and the command looks like it did nothing for that node.
-}

import Animation
import Config.Pathfinder exposing (nodeXOffset, nodeYOffset)
import Dict
import Expect
import Init.Pathfinder.Address as Address
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..))
import Msg.Pathfinder exposing (Msg(..))
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)


left : Id
left =
    ( "btc", "left" )


right : Id
right =
    ( "btc", "right" )


bystander : Id
bystander =
    ( "btc", "bystander" )


{-| `left` and `right` are selected, two columns apart, four rows apart; their
median row is y = 2 \* nodeYOffset. `bystander` is not selected and sits in
`right`'s column half a row above that median.
-}
graph : App
graph =
    App.init
        |> App.mapModel
            (\m ->
                let
                    network =
                        m.network
                in
                { m
                    | network =
                        { network
                            | addresses =
                                Dict.fromList
                                    [ ( left, Address.init left { x = 0, y = 0 } )
                                    , ( right, Address.init right { x = 2 * nodeXOffset, y = 4 * nodeYOffset } )
                                    , ( bystander, Address.init bystander { x = 2 * nodeXOffset, y = 1.5 * nodeYOffset } )
                                    ]
                        }
                    , selection = MultiSelect [ MSelectedAddress left, MSelectedAddress right ]
                }
            )


yOf : Id -> App -> Maybe Float
yOf id app =
    App.model app
        |> .network
        |> .addresses
        |> Dict.get id
        |> Maybe.map (.y >> Animation.getTo)


aligned : App
aligned =
    App.step UserClickedContextMenuAlignHorizontally graph


suite : Test
suite =
    describe "align horizontally"
        [ test "puts both selected nodes on the median row" <|
            \_ ->
                ( yOf left aligned, yOf right aligned )
                    |> Expect.equal ( Just (2 * nodeYOffset), Just (2 * nodeYOffset) )
        , test "moves the unselected node out of the way instead" <|
            \_ ->
                yOf bystander aligned
                    |> Expect.notEqual (Just (1.5 * nodeYOffset))
        , test "keeps the unselected node in its column" <|
            \_ ->
                App.model aligned
                    |> .network
                    |> .addresses
                    |> Dict.get bystander
                    |> Maybe.map .x
                    |> Expect.equal (Just (2 * nodeXOffset))
        ]
