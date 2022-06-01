# elm-hovercard

This module makes rendering hovercards like [Wikipedia's](https://anandchowdhary.github.io/hovercard/) easy. Given a [Browser.Dom.Element](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#Element) it is positioned above or below the element automatically.

```elm
hovercard
    -- configuration
    { maxWidth = 100
    , maxHeight = 100
    , tickLength = 16
    , borderColor = Color.black
    , backgroundColor = Color.lightBlue
    , borderWidth = 2
    }
    -- Browser.Dom.Element representing
    -- viewport and position of the element
    -- eg. the red square in the image below
    element
    -- additional styles for the hovercard, eg. a shadow
    [ style "box-shadow" "5px 5px 5px 0px rgba(0,0,0,0.25)"
    ]
    -- the content of the hovercard
    [ div
        []
        [ text "Lorem ipsum dolor sit amet"
        ]
    ]
```

![image](https://user-images.githubusercontent.com/1172181/123420146-7694dc80-d5bb-11eb-99ef-cdb93b9b2ec4.png)

## Complete example

Live on [Ellie](https://ellie-app.com/dyzNJcZv2D6a1)!
