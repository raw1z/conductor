module ProgressRing exposing (viewProgress)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


viewProgress : Int -> Int -> Float -> Html msg
viewProgress radius thickness progress =
    let
        realRadius =
            radius - 2 * thickness

        circumference =
            toFloat radius * 2 * pi

        dashOffset =
            round <| circumference - (circumference * progress)

        dashArray =
            String.fromInt (round circumference)
                ++ " "
                ++ String.fromInt (round circumference)
    in
    svg
        [ width <| String.fromInt (radius * 2)
        , height <| String.fromInt (radius * 2)
        ]
        [ circle
            [ fill "transparent"
            , stroke "white"
            , strokeWidth "1"
            , r <| String.fromInt realRadius
            , cx <| String.fromInt radius
            , cy <| String.fromInt radius
            ]
            []
        , circle
            [ class "progress-ring__circle"
            , fill "transparent"
            , stroke "white"
            , strokeWidth <| String.fromInt thickness
            , strokeLinecap "round"
            , strokeDasharray dashArray
            , strokeDashoffset <| String.fromInt dashOffset
            , r <| String.fromInt realRadius
            , cx <| String.fromInt radius
            , cy <| String.fromInt radius
            ]
            []
        ]
