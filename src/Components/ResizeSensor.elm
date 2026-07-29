module Components.ResizeSensor exposing (Size, view)

{-| Invisible sensor that reports size changes of its parent element.

Backed by the `<resize-sensor>` custom element defined in main.js, which
attaches a ResizeObserver to its parent and re-emits size changes as
`sensor-resize` CustomEvents. Place the sensor inside the element whose size
you want to track; it does not affect layout.

The observer also fires once right after mounting, so consumers receive the
initial size without a separate measurement.

@docs Size, view

-}

import Css
import Html.Styled exposing (Html, node)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (on)
import Json.Decode


{-| Content-box size of the observed (parent) element in CSS pixels.
-}
type alias Size =
    { width : Float
    , height : Float
    }


{-| Decoder for the `sensor-resize` event payload. Use directly when attaching
the listener yourself; otherwise prefer `view`.
-}
decoder : Json.Decode.Decoder Size
decoder =
    Json.Decode.map2 Size
        (Json.Decode.at [ "detail", "width" ] Json.Decode.float)
        (Json.Decode.at [ "detail", "height" ] Json.Decode.float)


{-| Render the sensor. Place it as a child of the element to observe.
-}
view : (Size -> msg) -> Html msg
view toMsg =
    node "resize-sensor"
        [ css [ Css.display Css.none ]
        , on "sensor-resize" (Json.Decode.map toMsg decoder)
        ]
        []
