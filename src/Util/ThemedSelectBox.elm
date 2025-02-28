module Util.ThemedSelectBox exposing
    ( Model
    , Msg(..)
    , OutMsg(..)
    , close
    , empty
    , init
    , update
    , view
    )

import Css
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onMouseLeave)
import List.Extra
import RecordSetter as Rs
import Theme.Html.SelectionControls as Sc
import Util.Css
import Util.View


type Msg a
    = Select a
    | Open
    | Close


type OutMsg a
    = Selected a
    | NoSelection


type Model a
    = SelectBox (SelectBoxModel a)


type alias SelectBoxModel a =
    { options : List a
    , open : Bool
    }


init : List a -> Model a
init options =
    SelectBox
        { options = options
        , open = False
        }


empty : Model a
empty =
    SelectBox { options = [], open = False }


close : Model a -> Model a
close (SelectBox m) =
    { m | open = False } |> SelectBox


open : Model a -> Model a
open (SelectBox m) =
    { m | open = True }
        |> SelectBox


select : Model a -> Model a
select (SelectBox m) =
    { m | open = not m.open }
        |> SelectBox


update : Msg a -> Model a -> ( Model a, OutMsg a )
update msg model =
    case msg of
        Select x ->
            ( select model
            , Selected x
            )

        Open ->
            ( open model, NoSelection )

        Close ->
            ( close model, NoSelection )


type alias Config a =
    { optionToLabel : a -> String
    }


view : Config a -> Model a -> a -> Html (Msg a)
view config (SelectBox sBox) selected =
    let
        selectedItem =
            List.Extra.find ((==) selected) sBox.options

        createRow sItem hoverEffect x =
            let
                itemAttributes =
                    [ Css.cursor Css.pointer
                        :: Css.property "user-select" "none"
                        :: (if hoverEffect then
                                [ Css.hover Sc.dropDownLabelsStateHoverSizeNormal_details.styles ]

                            else
                                []
                           )
                        |> css
                    , Util.View.onClickWithStop (Select x)
                    ]
            in
            if Just x == sItem then
                Sc.dropDownLabelsStateActiveSizeNormalWithAttributes
                    (Sc.dropDownLabelsStateActiveSizeNormalAttributes
                        |> Rs.s_stateActiveSizeNormal itemAttributes
                    )
                    { stateActiveSizeNormal = { dropDownText = config.optionToLabel x } }

            else
                Sc.dropDownLabelsStateNeutralSizeNormalWithAttributes
                    (Sc.dropDownLabelsStateNeutralSizeNormalAttributes
                        |> Rs.s_stateNeutralSizeNormal itemAttributes
                    )
                    { stateNeutralSizeNormal = { dropDownText = config.optionToLabel x } }

        selectedLabel =
            selectedItem
                |> Maybe.map config.optionToLabel
                |> Maybe.withDefault ""
    in
    if sBox.open then
        let
            dropdownOverlayCss =
                [ Css.position Css.absolute
                , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1))
                , Css.top (Css.px Sc.dropDownClosed_details.height)
                , Css.width (Css.px Sc.dropDownClosed_details.width)
                , Css.property "user-select" "none"
                , Css.height Css.auto
                ]

            dropDownList =
                sBox.options
                    |> List.map (createRow selectedItem True)
        in
        Sc.dropDownOpenWithAttributes
            (Sc.dropDownOpenAttributes
                |> Rs.s_dropDownOpen
                    [ Util.View.onClickWithStop Close
                    , onMouseLeave Close
                    , Util.View.pointer
                    , css
                        [ Sc.dropDownClosed_details.height
                            |> Css.px
                            |> Css.height
                            |> Css.important
                        ]
                    ]
                |> Rs.s_dropDownList [ css dropdownOverlayCss ]
            )
            { dropDownList = dropDownList
            }
            { dropDownHeaderOpen =
                { text = selectedLabel
                }
            }

    else
        Sc.dropDownClosedWithAttributes
            (Sc.dropDownClosedAttributes
                |> Rs.s_dropDownClosed
                    [ Util.View.onClickWithStop Open
                    , Util.View.pointer
                    ]
            )
            { dropDownClosed = { text = selectedLabel } }
