module View.Pathfinder.Error exposing (..)

import Config.View as View
import Html.Styled as Html exposing (..)
import Model.Direction as Direction
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id as Id
import Msg.Pathfinder exposing (Msg(..))
import View.Locale as Locale


view : View.Config -> Error -> Html Msg
view vc error =
    case error of
        InternalError err ->
            internalError vc err

        Errors err ->
            List.map (view vc >> List.singleton >> div []) err
                |> div []


internalError : View.Config -> InternalError -> Html Msg
internalError vc error =
    case error of
        AddressNotFoundInDict id ->
            "address not found found in dict: "
                ++ Id.toString id
                |> Locale.string vc.locale
                |> text

        TxValuesEmpty direction id ->
            Direction.toString direction
                ++ " tx values are empty: "
                ++ Id.toString id
                |> Locale.string vc.locale
                |> text
