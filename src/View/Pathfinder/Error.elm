module View.Pathfinder.Error exposing (..)

import Config.View as View
import Html.Styled as Html exposing (..)
import Model.Direction as Direction
import Model.Pathfinder.Error exposing (..)
import Model.Pathfinder.Id as Id
import Msg.Pathfinder exposing (Msg(..))
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Svg
import View.Locale as Locale


view : View.Config -> Error -> Html Msg
view vc error =
    case error of
        InternalError err ->
            internalError vc err

        Errors err ->
            List.map (view vc >> List.singleton >> div []) err
                |> div []


svg : View.Config -> Error -> Svg Msg
svg vc error =
    let
        root =
            Svg.text_ [ Svg.x "0", Svg.y "0" ]
    in
    case error of
        InternalError err ->
            internalErrorToString vc err
                |> Svg.text
                |> List.singleton
                |> root

        Errors err ->
            List.map (toString vc >> Svg.text) err
                |> root


toString : View.Config -> Error -> String
toString vc err =
    case err of
        InternalError e ->
            internalErrorToString vc e

        Errors es ->
            List.map (toString vc) es
                |> String.join ", "


internalError : View.Config -> InternalError -> Html Msg
internalError vc =
    internalErrorToString vc
        >> text


internalErrorToString : View.Config -> InternalError -> String
internalErrorToString vc error =
    case error of
        AddressNotFoundInDict id ->
            "address not found found in dict: "
                ++ Id.toString id
                |> Locale.string vc.locale

        TxValuesEmpty direction id ->
            Direction.toString direction
                ++ " tx values are empty: "
                ++ Id.toString id
                |> Locale.string vc.locale

        NoTxInputsOutputsFoundInDict id ->
            "no tx input/output addresses found in dict for "
                ++ Id.toString id
                |> Locale.string vc.locale
