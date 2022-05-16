module View.Graph.Browser exposing (browser)

import Config.Graph as Graph
import Config.View as View
import Css as CssStyled
import Css.Browser as Css
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model.Graph.Address exposing (..)
import Model.Graph.Browser as Browser exposing (..)
import Model.Graph.Entity exposing (Entity)
import Msg.Graph exposing (Msg(..))
import Util.View exposing (none, toCssColor)
import View.Locale as Locale


type Value
    = String String
    | EntityId Entity


browser : View.Config -> Graph.Config -> Browser.Model -> Html Msg
browser vc gc model =
    div
        [ Css.root vc |> css
        ]
        [ div
            [ Css.frame vc model.visible |> css
            ]
            (case model.type_ of
                Browser.None ->
                    []

                Browser.Address address ->
                    [ browseAddress vc gc address
                    ]

                Browser.Entity entity ->
                    [ browseEntity vc gc entity
                    ]
            )
        ]


browse : View.Config -> Graph.Config -> List ( String, Value ) -> Html Msg
browse vc gc rows =
    List.map (browseRow vc gc) rows
        |> div
            [ Css.propertyBoxTable vc |> css
            ]


browseRow : View.Config -> Graph.Config -> ( String, Value ) -> Html Msg
browseRow vc gc ( key, value ) =
    div
        [ Css.propertyBoxRow vc |> css
        ]
        [ span
            [ Css.propertyBoxKey vc |> css
            ]
            [ Locale.text vc.locale key
            ]
        , span
            [ Css.propertyBoxValue vc |> css
            ]
            [ browseValue vc gc value
            ]
        ]


browseValue : View.Config -> Graph.Config -> Value -> Html Msg
browseValue vc gc value =
    case value of
        String str ->
            text str

        EntityId entity ->
            div
                []
                [ entity.category
                    |> Maybe.andThen
                        (\cat ->
                            Dict.get cat gc.colors
                                |> Maybe.map
                                    (\color ->
                                        span
                                            [ css
                                                [ toCssColor color
                                                    |> CssStyled.color
                                                ]
                                            ]
                                            [ text cat
                                            ]
                                    )
                        )
                    |> Maybe.withDefault none
                , span
                    [ Css.propertyBoxEntityId vc |> css
                    ]
                    [ String.fromInt entity.entity.entity
                        |> text
                    ]
                ]


browseAddress : View.Config -> Graph.Config -> Address -> Html Msg
browseAddress vc gc address =
    browse vc
        gc
        [ ( "Address", String address.address.address )
        , ( "Currency", address.address.currency |> String.toUpper |> String )
        , ( "Tags"
          , Maybe.map List.length address.address.tags
                |> Maybe.withDefault 0
                |> String.fromInt
                |> String
          )
        ]


browseEntity : View.Config -> Graph.Config -> Entity -> Html Msg
browseEntity vc gc entity =
    browse vc
        gc
        [ ( "Entity", EntityId entity )
        , ( "Currency", entity.entity.currency |> String.toUpper |> String )
        , ( "Address Tags"
          , Maybe.map (.addressTags >> List.length) entity.entity.tags
                |> Maybe.withDefault 0
                |> String.fromInt
                |> String
          )
        ]
