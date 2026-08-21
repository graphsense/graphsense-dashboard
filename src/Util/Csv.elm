module Util.Csv exposing (int, string, valuesWithBaseCurrencyFloat)

import Api.Data
import Model.Currency exposing (AssetIdentifier)
import Model.Locale exposing (Model, ValueDetail(..))
import View.Locale exposing (coinWithoutCode)


int : Int -> String
int =
    String.fromInt


string : String -> String
string =
    identity


float : Float -> String
float =
    String.fromFloat


prefix : String -> String -> String
prefix key key2 =
    key ++ "_" ++ key2


valuesWithBaseCurrencyFloat : String -> Api.Data.Values -> Model -> AssetIdentifier -> List ( String, String )
valuesWithBaseCurrencyFloat key v locModel asset =
    let
        -- Always export exact values and in coin denomination
        nlocModel =
            { locModel | valueDetail = Exact }
    in
    ( prefix key "raw", int v.value )
        :: (( prefix key "in_base_currency", string (coinWithoutCode nlocModel asset v.value) )
                :: List.map (\f -> ( prefix key f.code, float f.value )) v.fiatValues
           )
