module Util.ThemedSelectBox exposing
    ( Model
    , Msg(..)
    , OutMsg(..)
    , SelectOption
    , close
    , empty
    , fromList
    , init
    , mapLabel
    , update
    , view
    )

import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import List.Extra
import RecordSetter as Rs
import Theme.Html.SelectionControls as Sc
import Util.Css
import Util.View


type alias SelectOption =
    { value : String
    , label : String
    }


type Msg
    = Select String
    | Open
    | Close


type OutMsg
    = Selected String


type Model
    = SelectBox SelectBoxModel


type alias SelectBoxModel =
    { options : List SelectOption
    , open : Bool
    }


fromList : List ( String, String ) -> Model
fromList lst =
    lst |> List.map (\( x, y ) -> { value = x, label = y }) |> init


init : List SelectOption -> Model
init options =
    SelectBox
        { options = options
        , open = False
        }


mapLabel : (String -> String) -> Model -> Model
mapLabel f (SelectBox m) =
    SelectBox
        { m
            | options = m.options |> List.map (\x -> { x | label = f x.label })
        }


empty : Model
empty =
    SelectBox { options = [], open = False }


close : Model -> Model
close (SelectBox m) =
    { m | open = False } |> SelectBox


open : Model -> Model
open (SelectBox m) =
    { m | open = True }
        |> SelectBox


select : Model -> Model
select (SelectBox m) =
    { m | open = not m.open }
        |> SelectBox


update : Msg -> Model -> ( Model, Maybe OutMsg )
update msg model =
    case msg of
        Select x ->
            ( select model
            , Selected x |> Just
            )

        Open ->
            ( open model, Nothing )

        Close ->
            ( close model, Nothing )


view : Model -> String -> Html Msg
view (SelectBox sBox) selected =
    let
        selectedItem =
            List.Extra.find (.value >> (==) selected) sBox.options

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
                    , Util.View.onClickWithStop (Select x.value)
                    ]
            in
            if Just x.value == (sItem |> Maybe.map .value) then
                Sc.dropDownLabelsStateActiveSizeNormalWithAttributes
                    (Sc.dropDownLabelsStateActiveSizeNormalAttributes
                        |> Rs.s_stateActiveSizeNormal itemAttributes
                    )
                    { stateActiveSizeNormal = { dropDownText = x.label } }

            else
                Sc.dropDownLabelsStateNeutralSizeNormalWithAttributes
                    (Sc.dropDownLabelsStateNeutralSizeNormalAttributes
                        |> Rs.s_stateNeutralSizeNormal itemAttributes
                    )
                    { stateNeutralSizeNormal = { dropDownText = x.label } }

        selectedRow =
            selectedItem
                |> Maybe.map (createRow Nothing False)
                |> Maybe.withDefault Util.View.none
    in
    if sBox.open then
        let
            dropdownOverlayCss =
                [ Css.position Css.absolute
                , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1))
                , Css.top (Css.px Sc.dropDownClosed_details.height)
                , Css.width (Css.px Sc.dropDownClosed_details.width)
                , Css.property "user-select" "none"
                ]

            dropDownList =
                sBox.options
                    |> List.map (createRow selectedItem True)
                    |> div
                        [ (Sc.dropDownOpenDropDownListNormalDropDownListNormal_details.styles ++ dropdownOverlayCss)
                            |> css
                        ]
        in
        Sc.dropDownOpenWithInstances
            (Sc.dropDownOpenAttributes
                |> Rs.s_dropDownOpen
                    [ Util.View.onClickWithStop Close
                    , css [ Css.cursor Css.pointer ]
                    ]
            )
            (Sc.dropDownOpenInstances
                |> Rs.s_dropDownListNormal (Just dropDownList)
                |> Rs.s_text (Just selectedRow)
            )
            { dropDownHeaderOpen = { text = "" }
            , dropDownLabels3 = { variant = Util.View.none }
            , dropDownLabels4 = { variant = Util.View.none }
            , dropDownLabels5 = { variant = Util.View.none }
            }

    else
        Sc.dropDownClosedWithInstances
            (Sc.dropDownClosedAttributes
                |> Rs.s_dropDownClosed
                    [ Util.View.onClickWithStop Open
                    , css [ Css.cursor Css.pointer ]
                    ]
            )
            (Sc.dropDownClosedInstances
                |> Rs.s_text (Just selectedRow)
            )
            { dropDownClosed = { text = "" } }
