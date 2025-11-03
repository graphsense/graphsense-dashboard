module Util.Csv exposing (a0, bool, float, int, prefix, string, timestamp, values, valuesWithBaseCurrencyFloat)

import Api.Data
import Model.Currency exposing (AssetIdentifier, assetFromBase)
import Model.Locale exposing (Model, ValueDetail(..))
import View.Locale exposing (coinWithoutCode)


int : Int -> String
int =
    String.fromInt


bool : Bool -> String
bool b =
    if b then
        "true"

    else
        "false"


string : String -> String
string =
    identity


float : Float -> String
float =
    String.fromFloat


timestamp : Model -> Int -> String
timestamp =
    View.Locale.timestamp


prefix : String -> String -> ( String, List String )
prefix key key2 =
    ( key ++ "_" ++ key2, [] )


valuesWithBaseCurrencyFloat : String -> Api.Data.Values -> Model -> AssetIdentifier -> List ( ( String, List String ), String )
valuesWithBaseCurrencyFloat key v locModel asset =
    let
        -- Always export exact values and in coin denomination
        nlocModel =
            { locModel | valueDetail = Exact }
    in
    ( ( key, [] ), int v.value )
        :: (( prefix key "in_base_currency", string (coinWithoutCode nlocModel asset v.value) )
                :: List.map (\f -> ( prefix key f.code, float f.value )) v.fiatValues
           )


values : String -> Api.Data.Values -> List ( ( String, List String ), String )
values key v =
    ( ( key, [] ), int v.value )
        :: List.map (\f -> ( prefix key f.code, float f.value )) v.fiatValues


a0 : String -> String
a0 s =
    s ++ " {0}"
