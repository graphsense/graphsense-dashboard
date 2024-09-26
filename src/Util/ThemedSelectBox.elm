module Util.ThemedSelectBox exposing (Model, Msg(..), SelectOption, empty, fromList, init, mapLabel, select, update, view)

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


select : String -> Model -> Model
select _ (SelectBox m) =
    { m | open = not m.open } |> SelectBox


update : Msg -> Model -> Model
update msg m =
    case msg of
        Select x ->
            select x m


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
            Sc.dropDownOpenAttributes
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
            Sc.dropDownClosedAttributes
            (Sc.dropDownClosedInstances
                |> Rs.s_text (Just selectedRow)
            )
            { dropDownClosed = { text = "" } }
