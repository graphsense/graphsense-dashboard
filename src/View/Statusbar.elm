module View.Statusbar exposing (..)

import Config.View as View
import Css.Statusbar as Css
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import List.Extra
import Model exposing (Msg(..))
import Model.Graph.Id as Id
import Model.Graph.Search as Search
import Model.Statusbar exposing (..)
import Util.View exposing (firstToUpper, loadingSpinner)
import View.Locale as Locale


view : View.Config -> Model -> Html Msg
view vc model =
    div
        [ Css.root vc |> css
        ]
        (model.messages
            |> Dict.toList
            |> List.head
            |> Maybe.map (message vc)
            |> Maybe.withDefault []
        )


message : View.Config -> ( String, List String ) -> List (Html Msg)
message vc ( key, values ) =
    [ loadingSpinner vc Css.loadingSpinner
    , values
        |> List.map (Locale.string vc.locale)
        |> Locale.interpolated vc.locale (firstToUpper key)
        |> text
        |> List.singleton
        |> span []
    ]
