module Main exposing (main)

import Angle
import BoundingBox2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import LSystem
import LSystem.Rules as Rules
import Pixels
import Point2d
import Polyline2d
import Svg
import Svg.Attributes as SvgAttr
import Turtle


main : Html msg
main =
    let
        length =
            Pixels.pixels 10

        angle =
            Angle.degrees 90

        rules =
            Rules.empty
                |> Rules.add 'X' "X+YF+"
                |> Rules.add 'Y' "-FX-Y"

        axiom =
            "FX"

        iteration =
            3

        commands =
            LSystem.iteration rules axiom iteration

        drawing =
            Turtle.commands length angle commands
                |> Turtle.draw
                |> List.map (Polyline2d.rotateAround Point2d.origin (Angle.degrees 180))

        boundingBox =
            Turtle.boundingBox drawing
                |> Maybe.withDefault
                    (BoundingBox2d.from
                        (Point2d.pixels -400 -300)
                        (Point2d.pixels 400 300)
                    )

        viewBox =
            Turtle.viewBox length boundingBox
    in
    Html.div []
        [ Html.span [] [ Html.text <| axiom ++ " => " ++ commands ]
        , Svg.svg
            [ HtmlAttr.style "width" "100vw"
            , HtmlAttr.style "height" "100vh"
            , SvgAttr.viewBox viewBox
            ]
            [ List.map (Svg.polyline2d [])
                drawing
                |> Svg.g
                    [ SvgAttr.stroke "black"
                    , SvgAttr.fill "none"
                    , SvgAttr.strokeLinejoin "round"
                    , SvgAttr.strokeLinecap "round"
                    ]
            ]
        ]
