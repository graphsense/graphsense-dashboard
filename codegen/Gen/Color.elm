module Gen.Color exposing (annotation_, black, blue, brown, call_, charcoal, darkBlue, darkBrown, darkCharcoal, darkGray, darkGreen, darkGrey, darkOrange, darkPurple, darkRed, darkYellow, fromHsla, fromRgba, gray, green, grey, hsl, hsla, lightBlue, lightBrown, lightCharcoal, lightGray, lightGreen, lightGrey, lightOrange, lightPurple, lightRed, lightYellow, moduleName_, orange, purple, red, rgb, rgb255, rgba, toCssString, toHsla, toRgba, values_, white, yellow)

{-| 
@docs moduleName_, rgb255, rgb, rgba, hsl, hsla, fromRgba, fromHsla, toCssString, toRgba, toHsla, red, orange, yellow, green, blue, purple, brown, lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown, darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown, white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black, lightGray, gray, darkGray, annotation_, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Color" ]


{-| Creates a color from RGB (red, green, blue) integer values between 0 and 255.

This is a convenience function if you find passing RGB channels as integers scaled to 255 more intuitive.

See also:

If you want to provide RGB values as `Float` values between 0.0 and 1.0, see [`rgb`](#rgb).

rgb255: Int -> Int -> Int -> Color.Color
-}
rgb255 : Int -> Int -> Int -> Elm.Expression
rgb255 rgb255Arg rgb255Arg0 rgb255Arg1 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "rgb255"
             , annotation =
                 Just
                     (Type.function
                          [ Type.int, Type.int, Type.int ]
                          (Type.namedWith [ "Color" ] "Color" [])
                     )
             }
        )
        [ Elm.int rgb255Arg, Elm.int rgb255Arg0, Elm.int rgb255Arg1 ]


{-| Creates a color from RGB (red, green, blue) values between 0.0 and 1.0 (inclusive).

This is a convenience function for making a color value with full opacity.

See also:

If you want to pass RGB values as `Int` values between 0 and 255, see [`rgb255`](#rgb255).

If you need to provide an alpha value, see [`rgba`](#rgba).

If you want to be more explicit with parameter names, see [`fromRgba`](#fromRgba).

rgb: Float -> Float -> Float -> Color.Color
-}
rgb : Float -> Float -> Float -> Elm.Expression
rgb rgbArg rgbArg0 rgbArg1 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "rgb"
             , annotation =
                 Just
                     (Type.function
                          [ Type.float, Type.float, Type.float ]
                          (Type.namedWith [ "Color" ] "Color" [])
                     )
             }
        )
        [ Elm.float rgbArg, Elm.float rgbArg0, Elm.float rgbArg1 ]


{-| Creates a color from RGBA (red, green, blue, alpha) values between 0.0 and 1.0 (inclusive).

See also:

If you want to be more concise and want full alpha, see [`rgb`](#rgb).

If you want to be more explicit with parameter names, see [`fromRgba`](#fromRgba).

rgba: Float -> Float -> Float -> Float -> Color.Color
-}
rgba : Float -> Float -> Float -> Float -> Elm.Expression
rgba rgbaArg rgbaArg0 rgbaArg1 rgbaArg2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "rgba"
             , annotation =
                 Just
                     (Type.function
                          [ Type.float, Type.float, Type.float, Type.float ]
                          (Type.namedWith [ "Color" ] "Color" [])
                     )
             }
        )
        [ Elm.float rgbaArg
        , Elm.float rgbaArg0
        , Elm.float rgbaArg1
        , Elm.float rgbaArg2
        ]


{-| Creates a color from [HSL](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness)
values between 0.0 and 1.0 (inclusive).

See also:

If you need to provide an alpha value, see [`hsla`](#hsla).

If you want to be more explicit with parameter names, see [`fromHsla`](#fromHsla).

hsl: Float -> Float -> Float -> Color.Color
-}
hsl : Float -> Float -> Float -> Elm.Expression
hsl hslArg hslArg0 hslArg1 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "hsl"
             , annotation =
                 Just
                     (Type.function
                          [ Type.float, Type.float, Type.float ]
                          (Type.namedWith [ "Color" ] "Color" [])
                     )
             }
        )
        [ Elm.float hslArg, Elm.float hslArg0, Elm.float hslArg1 ]


{-| Creates a color from [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
values between 0.0 and 1.0 (inclusive).

See also:

If you want to be more concise and want full alpha, see [`hsl`](#hsl).

If you want to be more explicit with parameter names, see [`fromHsla`](#fromHsla).

hsla: Float -> Float -> Float -> Float -> Color.Color
-}
hsla : Float -> Float -> Float -> Float -> Elm.Expression
hsla hslaArg hslaArg0 hslaArg1 hslaArg2 =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "hsla"
             , annotation =
                 Just
                     (Type.function
                          [ Type.float, Type.float, Type.float, Type.float ]
                          (Type.namedWith [ "Color" ] "Color" [])
                     )
             }
        )
        [ Elm.float hslaArg
        , Elm.float hslaArg0
        , Elm.float hslaArg1
        , Elm.float hslaArg2
        ]


{-| Creates a color from a record of RGBA values (red, green, blue, alpha) between 0.0 and 1.0 (inclusive).

The RGB values are interpreted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified by the HTML, [CSS](https://www.w3.org/TR/css-color-3/#rgb-color),
and [SVG](https://www.w3.org/Graphics/SVG/1.1/color.html) specs
(and is also widely considered the default color space for digital images that do not explicitly contain color space information).

This is a strict function that will force you to name all channel parameters, to avoid mixing them up.

See also:

If you want to be more concise, see [`rgba`](#rgba) or [`rgb`](#rgb).

fromRgba: { red : Float, green : Float, blue : Float, alpha : Float } -> Color.Color
-}
fromRgba :
    { red : Float, green : Float, blue : Float, alpha : Float }
    -> Elm.Expression
fromRgba fromRgbaArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "fromRgba"
             , annotation =
                 Just
                     (Type.function
                          [ Type.record
                              [ ( "red", Type.float )
                              , ( "green", Type.float )
                              , ( "blue", Type.float )
                              , ( "alpha", Type.float )
                              ]
                          ]
                          (Type.namedWith [ "Color" ] "Color" [])
                     )
             }
        )
        [ Elm.record
            [ Tuple.pair "red" (Elm.float fromRgbaArg.red)
            , Tuple.pair "green" (Elm.float fromRgbaArg.green)
            , Tuple.pair "blue" (Elm.float fromRgbaArg.blue)
            , Tuple.pair "alpha" (Elm.float fromRgbaArg.alpha)
            ]
        ]


{-| Creates a color from [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
values between 0.0 and 1.0 (inclusive).

See also:

If you want to be more concise, see [`hsla`](#hsla) or [`hsl`](#hsl).

fromHsla: 
    { hue : Float, saturation : Float, lightness : Float, alpha : Float }
    -> Color.Color
-}
fromHsla :
    { hue : Float, saturation : Float, lightness : Float, alpha : Float }
    -> Elm.Expression
fromHsla fromHslaArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "fromHsla"
             , annotation =
                 Just
                     (Type.function
                          [ Type.record
                              [ ( "hue", Type.float )
                              , ( "saturation", Type.float )
                              , ( "lightness", Type.float )
                              , ( "alpha", Type.float )
                              ]
                          ]
                          (Type.namedWith [ "Color" ] "Color" [])
                     )
             }
        )
        [ Elm.record
            [ Tuple.pair "hue" (Elm.float fromHslaArg.hue)
            , Tuple.pair "saturation" (Elm.float fromHslaArg.saturation)
            , Tuple.pair "lightness" (Elm.float fromHslaArg.lightness)
            , Tuple.pair "alpha" (Elm.float fromHslaArg.alpha)
            ]
        ]


{-| Converts a color to a string suitable for use in CSS.
The string will conform to [CSS Color Module Level 3](https://www.w3.org/TR/css-color-3/),
which is supported by all current web browsers, all versions of Firefox,
all versions of Chrome, IE 9+, and all common mobile browsers
([browser support details](https://caniuse.com/#feat=css3-colors)).

    Html.Attributes.style "background-color" (Color.toCssString Color.lightPurple)

Note: the current implementation produces a string in the form
`rgba(rr.rr%,gg.gg%,bb.bb%,a.aaa)`, but this may change in the
future, and you should not rely on this implementation detail.

toCssString: Color.Color -> String
-}
toCssString : Elm.Expression -> Elm.Expression
toCssString toCssStringArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "toCssString"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Color" ] "Color" [] ]
                          Type.string
                     )
             }
        )
        [ toCssStringArg ]


{-| Extract the RGBA (red, green, blue, alpha) components from a color.
The component values will be between 0.0 and 1.0 (inclusive).

The RGB values are interpreted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified by the HTML, [CSS](https://www.w3.org/TR/css-color-3/#rgb-color),
and [SVG](https://www.w3.org/Graphics/SVG/1.1/color.html) specs
(and is also widely considered the default color space for digital images that do not explicitly contain color space information).

toRgba: Color.Color -> { red : Float, green : Float, blue : Float, alpha : Float }
-}
toRgba : Elm.Expression -> Elm.Expression
toRgba toRgbaArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "toRgba"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Color" ] "Color" [] ]
                          (Type.record
                               [ ( "red", Type.float )
                               , ( "green", Type.float )
                               , ( "blue", Type.float )
                               , ( "alpha", Type.float )
                               ]
                          )
                     )
             }
        )
        [ toRgbaArg ]


{-| Extract the [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
components from a color.
The component values will be between 0.0 and 1.0 (inclusive).

toHsla: 
    Color.Color
    -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
-}
toHsla : Elm.Expression -> Elm.Expression
toHsla toHslaArg =
    Elm.apply
        (Elm.value
             { importFrom = [ "Color" ]
             , name = "toHsla"
             , annotation =
                 Just
                     (Type.function
                          [ Type.namedWith [ "Color" ] "Color" [] ]
                          (Type.record
                               [ ( "hue", Type.float )
                               , ( "saturation", Type.float )
                               , ( "lightness", Type.float )
                               , ( "alpha", Type.float )
                               ]
                          )
                     )
             }
        )
        [ toHslaArg ]


{-| red: Color.Color -}
red : Elm.Expression
red =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "red"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| orange: Color.Color -}
orange : Elm.Expression
orange =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "orange"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| yellow: Color.Color -}
yellow : Elm.Expression
yellow =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "yellow"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| green: Color.Color -}
green : Elm.Expression
green =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "green"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| blue: Color.Color -}
blue : Elm.Expression
blue =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "blue"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| purple: Color.Color -}
purple : Elm.Expression
purple =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "purple"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| brown: Color.Color -}
brown : Elm.Expression
brown =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "brown"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightRed: Color.Color -}
lightRed : Elm.Expression
lightRed =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightRed"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightOrange: Color.Color -}
lightOrange : Elm.Expression
lightOrange =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightOrange"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightYellow: Color.Color -}
lightYellow : Elm.Expression
lightYellow =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightYellow"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightGreen: Color.Color -}
lightGreen : Elm.Expression
lightGreen =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightGreen"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightBlue: Color.Color -}
lightBlue : Elm.Expression
lightBlue =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightBlue"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightPurple: Color.Color -}
lightPurple : Elm.Expression
lightPurple =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightPurple"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightBrown: Color.Color -}
lightBrown : Elm.Expression
lightBrown =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightBrown"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkRed: Color.Color -}
darkRed : Elm.Expression
darkRed =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkRed"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkOrange: Color.Color -}
darkOrange : Elm.Expression
darkOrange =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkOrange"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkYellow: Color.Color -}
darkYellow : Elm.Expression
darkYellow =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkYellow"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkGreen: Color.Color -}
darkGreen : Elm.Expression
darkGreen =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkGreen"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkBlue: Color.Color -}
darkBlue : Elm.Expression
darkBlue =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkBlue"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkPurple: Color.Color -}
darkPurple : Elm.Expression
darkPurple =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkPurple"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkBrown: Color.Color -}
darkBrown : Elm.Expression
darkBrown =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkBrown"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| white: Color.Color -}
white : Elm.Expression
white =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "white"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightGrey: Color.Color -}
lightGrey : Elm.Expression
lightGrey =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightGrey"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| grey: Color.Color -}
grey : Elm.Expression
grey =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "grey"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkGrey: Color.Color -}
darkGrey : Elm.Expression
darkGrey =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkGrey"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightCharcoal: Color.Color -}
lightCharcoal : Elm.Expression
lightCharcoal =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightCharcoal"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| charcoal: Color.Color -}
charcoal : Elm.Expression
charcoal =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "charcoal"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkCharcoal: Color.Color -}
darkCharcoal : Elm.Expression
darkCharcoal =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkCharcoal"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| black: Color.Color -}
black : Elm.Expression
black =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "black"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| lightGray: Color.Color -}
lightGray : Elm.Expression
lightGray =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "lightGray"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| gray: Color.Color -}
gray : Elm.Expression
gray =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "gray"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


{-| darkGray: Color.Color -}
darkGray : Elm.Expression
darkGray =
    Elm.value
        { importFrom = [ "Color" ]
        , name = "darkGray"
        , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
        }


annotation_ : { color : Type.Annotation }
annotation_ =
    { color = Type.namedWith [ "Color" ] "Color" [] }


call_ :
    { rgb255 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , rgb : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , rgba :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , hsl : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , hsla :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , fromRgba : Elm.Expression -> Elm.Expression
    , fromHsla : Elm.Expression -> Elm.Expression
    , toCssString : Elm.Expression -> Elm.Expression
    , toRgba : Elm.Expression -> Elm.Expression
    , toHsla : Elm.Expression -> Elm.Expression
    }
call_ =
    { rgb255 =
        \rgb255Arg rgb255Arg0 rgb255Arg1 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "rgb255"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.int, Type.int, Type.int ]
                                  (Type.namedWith [ "Color" ] "Color" [])
                             )
                     }
                )
                [ rgb255Arg, rgb255Arg0, rgb255Arg1 ]
    , rgb =
        \rgbArg rgbArg0 rgbArg1 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "rgb"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.float, Type.float, Type.float ]
                                  (Type.namedWith [ "Color" ] "Color" [])
                             )
                     }
                )
                [ rgbArg, rgbArg0, rgbArg1 ]
    , rgba =
        \rgbaArg rgbaArg0 rgbaArg1 rgbaArg2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "rgba"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.float
                                  , Type.float
                                  , Type.float
                                  , Type.float
                                  ]
                                  (Type.namedWith [ "Color" ] "Color" [])
                             )
                     }
                )
                [ rgbaArg, rgbaArg0, rgbaArg1, rgbaArg2 ]
    , hsl =
        \hslArg hslArg0 hslArg1 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "hsl"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.float, Type.float, Type.float ]
                                  (Type.namedWith [ "Color" ] "Color" [])
                             )
                     }
                )
                [ hslArg, hslArg0, hslArg1 ]
    , hsla =
        \hslaArg hslaArg0 hslaArg1 hslaArg2 ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "hsla"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.float
                                  , Type.float
                                  , Type.float
                                  , Type.float
                                  ]
                                  (Type.namedWith [ "Color" ] "Color" [])
                             )
                     }
                )
                [ hslaArg, hslaArg0, hslaArg1, hslaArg2 ]
    , fromRgba =
        \fromRgbaArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "fromRgba"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.record
                                      [ ( "red", Type.float )
                                      , ( "green", Type.float )
                                      , ( "blue", Type.float )
                                      , ( "alpha", Type.float )
                                      ]
                                  ]
                                  (Type.namedWith [ "Color" ] "Color" [])
                             )
                     }
                )
                [ fromRgbaArg ]
    , fromHsla =
        \fromHslaArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "fromHsla"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.record
                                      [ ( "hue", Type.float )
                                      , ( "saturation", Type.float )
                                      , ( "lightness", Type.float )
                                      , ( "alpha", Type.float )
                                      ]
                                  ]
                                  (Type.namedWith [ "Color" ] "Color" [])
                             )
                     }
                )
                [ fromHslaArg ]
    , toCssString =
        \toCssStringArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "toCssString"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Color" ] "Color" [] ]
                                  Type.string
                             )
                     }
                )
                [ toCssStringArg ]
    , toRgba =
        \toRgbaArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "toRgba"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Color" ] "Color" [] ]
                                  (Type.record
                                       [ ( "red", Type.float )
                                       , ( "green", Type.float )
                                       , ( "blue", Type.float )
                                       , ( "alpha", Type.float )
                                       ]
                                  )
                             )
                     }
                )
                [ toRgbaArg ]
    , toHsla =
        \toHslaArg ->
            Elm.apply
                (Elm.value
                     { importFrom = [ "Color" ]
                     , name = "toHsla"
                     , annotation =
                         Just
                             (Type.function
                                  [ Type.namedWith [ "Color" ] "Color" [] ]
                                  (Type.record
                                       [ ( "hue", Type.float )
                                       , ( "saturation", Type.float )
                                       , ( "lightness", Type.float )
                                       , ( "alpha", Type.float )
                                       ]
                                  )
                             )
                     }
                )
                [ toHslaArg ]
    }


values_ :
    { rgb255 : Elm.Expression
    , rgb : Elm.Expression
    , rgba : Elm.Expression
    , hsl : Elm.Expression
    , hsla : Elm.Expression
    , fromRgba : Elm.Expression
    , fromHsla : Elm.Expression
    , toCssString : Elm.Expression
    , toRgba : Elm.Expression
    , toHsla : Elm.Expression
    , red : Elm.Expression
    , orange : Elm.Expression
    , yellow : Elm.Expression
    , green : Elm.Expression
    , blue : Elm.Expression
    , purple : Elm.Expression
    , brown : Elm.Expression
    , lightRed : Elm.Expression
    , lightOrange : Elm.Expression
    , lightYellow : Elm.Expression
    , lightGreen : Elm.Expression
    , lightBlue : Elm.Expression
    , lightPurple : Elm.Expression
    , lightBrown : Elm.Expression
    , darkRed : Elm.Expression
    , darkOrange : Elm.Expression
    , darkYellow : Elm.Expression
    , darkGreen : Elm.Expression
    , darkBlue : Elm.Expression
    , darkPurple : Elm.Expression
    , darkBrown : Elm.Expression
    , white : Elm.Expression
    , lightGrey : Elm.Expression
    , grey : Elm.Expression
    , darkGrey : Elm.Expression
    , lightCharcoal : Elm.Expression
    , charcoal : Elm.Expression
    , darkCharcoal : Elm.Expression
    , black : Elm.Expression
    , lightGray : Elm.Expression
    , gray : Elm.Expression
    , darkGray : Elm.Expression
    }
values_ =
    { rgb255 =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "rgb255"
            , annotation =
                Just
                    (Type.function
                         [ Type.int, Type.int, Type.int ]
                         (Type.namedWith [ "Color" ] "Color" [])
                    )
            }
    , rgb =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "rgb"
            , annotation =
                Just
                    (Type.function
                         [ Type.float, Type.float, Type.float ]
                         (Type.namedWith [ "Color" ] "Color" [])
                    )
            }
    , rgba =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "rgba"
            , annotation =
                Just
                    (Type.function
                         [ Type.float, Type.float, Type.float, Type.float ]
                         (Type.namedWith [ "Color" ] "Color" [])
                    )
            }
    , hsl =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "hsl"
            , annotation =
                Just
                    (Type.function
                         [ Type.float, Type.float, Type.float ]
                         (Type.namedWith [ "Color" ] "Color" [])
                    )
            }
    , hsla =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "hsla"
            , annotation =
                Just
                    (Type.function
                         [ Type.float, Type.float, Type.float, Type.float ]
                         (Type.namedWith [ "Color" ] "Color" [])
                    )
            }
    , fromRgba =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "fromRgba"
            , annotation =
                Just
                    (Type.function
                         [ Type.record
                             [ ( "red", Type.float )
                             , ( "green", Type.float )
                             , ( "blue", Type.float )
                             , ( "alpha", Type.float )
                             ]
                         ]
                         (Type.namedWith [ "Color" ] "Color" [])
                    )
            }
    , fromHsla =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "fromHsla"
            , annotation =
                Just
                    (Type.function
                         [ Type.record
                             [ ( "hue", Type.float )
                             , ( "saturation", Type.float )
                             , ( "lightness", Type.float )
                             , ( "alpha", Type.float )
                             ]
                         ]
                         (Type.namedWith [ "Color" ] "Color" [])
                    )
            }
    , toCssString =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "toCssString"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Color" ] "Color" [] ]
                         Type.string
                    )
            }
    , toRgba =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "toRgba"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Color" ] "Color" [] ]
                         (Type.record
                              [ ( "red", Type.float )
                              , ( "green", Type.float )
                              , ( "blue", Type.float )
                              , ( "alpha", Type.float )
                              ]
                         )
                    )
            }
    , toHsla =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "toHsla"
            , annotation =
                Just
                    (Type.function
                         [ Type.namedWith [ "Color" ] "Color" [] ]
                         (Type.record
                              [ ( "hue", Type.float )
                              , ( "saturation", Type.float )
                              , ( "lightness", Type.float )
                              , ( "alpha", Type.float )
                              ]
                         )
                    )
            }
    , red =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "red"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , orange =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "orange"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , yellow =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "yellow"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , green =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "green"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , blue =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "blue"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , purple =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "purple"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , brown =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "brown"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightRed =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightRed"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightOrange =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightOrange"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightYellow =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightYellow"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightGreen =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightGreen"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightBlue =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightBlue"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightPurple =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightPurple"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightBrown =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightBrown"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkRed =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkRed"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkOrange =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkOrange"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkYellow =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkYellow"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkGreen =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkGreen"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkBlue =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkBlue"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkPurple =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkPurple"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkBrown =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkBrown"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , white =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "white"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightGrey =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightGrey"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , grey =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "grey"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkGrey =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkGrey"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightCharcoal =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightCharcoal"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , charcoal =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "charcoal"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkCharcoal =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkCharcoal"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , black =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "black"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , lightGray =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "lightGray"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , gray =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "gray"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    , darkGray =
        Elm.value
            { importFrom = [ "Color" ]
            , name = "darkGray"
            , annotation = Just (Type.namedWith [ "Color" ] "Color" [])
            }
    }