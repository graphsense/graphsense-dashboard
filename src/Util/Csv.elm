module Util.Csv exposing (..)

import Api.Data
import Model.Currency exposing (Currency(..))
import Model.Locale exposing (..)
import View.Locale exposing (currencyWithoutCode)


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


valuesWithBaseCurrencyFloat : String -> Api.Data.Values -> Model -> String -> List ( ( String, List String ), String )
valuesWithBaseCurrencyFloat key v locModel currency =
    let
        -- Always export exact values and in coin denomination
        nlocModel =
            { locModel | valueDetail = Exact, currency = Coin }
    in
    ( ( key, [] ), int v.value )
        :: (( prefix key "in_base_currency", string (currencyWithoutCode nlocModel [ ( currency, v ) ]) )
                :: List.map (\f -> ( prefix key f.code, float f.value )) v.fiatValues
           )


values : String -> Api.Data.Values -> List ( ( String, List String ), String )
values key v =
    ( ( key, [] ), int v.value )
        :: List.map (\f -> ( prefix key f.code, float f.value )) v.fiatValues


a0 : String -> String
a0 s =
    s ++ " {0}"
