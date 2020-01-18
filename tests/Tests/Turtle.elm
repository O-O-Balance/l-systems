module Tests.Turtle exposing (suite)

import BoundingBox2d exposing (BoundingBox2d)
import Expect exposing (Expectation)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Test exposing (Test, describe, test)
import Tests.Common as Common
import Turtle


lineSegmentsEqualWithin : Quantity Float units -> LineSegment2d units coordinates -> LineSegment2d units coordinates -> Expectation
lineSegmentsEqualWithin tolerance expected actual =
    actual
        |> Expect.all
            [ Expect.true "Expected start points to be equal within tolerance"
                << Point2d.equalWithin tolerance (LineSegment2d.startPoint expected)
                << LineSegment2d.startPoint
            , Expect.true "Expected end points to be equal within tolerance"
                << Point2d.equalWithin tolerance (LineSegment2d.endPoint expected)
                << LineSegment2d.endPoint
            ]


allLineSegmentsEqualWithin : Quantity Float units -> List (LineSegment2d units coordinates) -> List (LineSegment2d units coordinates) -> Expectation
allLineSegmentsEqualWithin tolerance expected actual =
    let
        lengthExpectation : List (LineSegment2d units coordinates) -> Expectation
        lengthExpectation =
            Expect.equal
                (List.length expected)
                << List.length

        segmentExpectations : List (a -> Expectation)
        segmentExpectations =
            List.map always <|
                List.map2
                    (lineSegmentsEqualWithin tolerance)
                    expected
                    actual
    in
    actual
        |> Expect.all
            (lengthExpectation :: segmentExpectations)


suite : Test
suite =
    describe "The Turtle module"
        [ describe "Functions for obtaining bounding and view boxes" <|
            let
                singletonBox : BoundingBox2d units coordinates
                singletonBox =
                    BoundingBox2d.singleton Point2d.origin

                box1 : BoundingBox2d Pixels coordinates
                box1 =
                    BoundingBox2d.fromExtrema
                        { minX = Pixels.pixels -5
                        , minY = Pixels.pixels -5
                        , maxX = Pixels.pixels 20
                        , maxY = Pixels.pixels 10
                        }
            in
            [ describe "Turtle.boundingBox" <|
                let
                    emptyPolyline : Polyline2d units coordinates
                    emptyPolyline =
                        Polyline2d.fromVertices []

                    singletonPolyline : Polyline2d units coordinates
                    singletonPolyline =
                        Polyline2d.fromVertices [ Point2d.origin ]

                    oneLine : Polyline2d Pixels coordinates
                    oneLine =
                        Polyline2d.fromVertices
                            [ Point2d.pixels 10 5
                            , Point2d.pixels 20 5
                            ]

                    twoLines : Polyline2d Pixels coordinates
                    twoLines =
                        Polyline2d.fromVertices
                            [ Point2d.pixels -5 10
                            , Point2d.pixels -5 -5
                            , Point2d.pixels 10 -5
                            ]

                    box2 : BoundingBox2d Pixels coordinates
                    box2 =
                        BoundingBox2d.fromExtrema
                            { minX = Pixels.pixels 10
                            , minY = Pixels.pixels 5
                            , maxX = Pixels.pixels 20
                            , maxY = Pixels.pixels 5
                            }

                    box3 : BoundingBox2d Pixels coordinates
                    box3 =
                        BoundingBox2d.fromExtrema
                            { minX = Pixels.pixels -5
                            , minY = Pixels.pixels -5
                            , maxX = Pixels.pixels 10
                            , maxY = Pixels.pixels 10
                            }

                    box4 : BoundingBox2d Pixels coordinates
                    box4 =
                        BoundingBox2d.fromExtrema
                            { minX = Pixels.pixels 0
                            , minY = Pixels.pixels 0
                            , maxX = Pixels.pixels 20
                            , maxY = Pixels.pixels 5
                            }
                in
                [ test "returns Maybe.Nothing when passed an empty list" <|
                    \_ ->
                        []
                            |> Turtle.boundingBox
                            |> Expect.equal Nothing
                , test "returns Maybe.Nothing when passed a list containing only empty polylines" <|
                    \_ ->
                        [ emptyPolyline ]
                            |> Turtle.boundingBox
                            |> Expect.equal Nothing
                , test "returns a singleton bounding when box passed a list containing only one singleton polyline" <|
                    \_ ->
                        [ singletonPolyline ]
                            |> Turtle.boundingBox
                            |> Expect.equal (Just singletonBox)
                , test "returns a bounding box with zero area when effectively given only one line segment" <|
                    \_ ->
                        [ oneLine ]
                            |> Turtle.boundingBox
                            |> Expect.equal (Just box2)
                , test "returns a bounding box with non-zero area when given a 2D shape" <|
                    \_ ->
                        [ twoLines ]
                            |> Turtle.boundingBox
                            |> Expect.equal (Just box3)
                , test "returns a bounding box containing both polylines when given two" <|
                    \_ ->
                        [ oneLine, singletonPolyline ]
                            |> Turtle.boundingBox
                            |> Expect.equal (Just box4)
                , test "returns a bounding box containing all three polylines when given three" <|
                    \_ ->
                        [ singletonPolyline, oneLine, twoLines ]
                            |> Turtle.boundingBox
                            |> Expect.equal (Just box1)
                , test "returns the same bounding box when given a list plus the empty polyline" <|
                    \_ ->
                        [ emptyPolyline, singletonPolyline, oneLine, twoLines ]
                            |> Turtle.boundingBox
                            |> Expect.equal (Just box1)
                ]
            , describe "Turtle.viewBox"
                [ test "returns an empty view box when given a singleton bounding box" <|
                    \_ ->
                        Turtle.viewBox Quantity.zero singletonBox
                            |> Expect.equal "0 0 0 0"
                , test "expands an empty viewbox by the given margin" <|
                    \_ ->
                        Turtle.viewBox (Pixels.pixels 5) singletonBox
                            |> Expect.equal "-5 -5 10 10"
                , test "returns a non-empty view box when given a non-empty bounding box" <|
                    \_ ->
                        Turtle.viewBox Quantity.zero box1
                            |> Expect.equal "-5 -5 25 15"
                , test "expands a non-empty viewbox by the given margin" <|
                    \_ ->
                        Turtle.viewBox (Pixels.pixels 10) box1
                            |> Expect.equal "-15 -15 45 35"
                ]
            ]
        , describe "Functions for working with the Command type"
            [ describe "Turtle.draw" <|
                let
                    lengthInPixels : Float
                    lengthInPixels =
                        Pixels.inPixels Common.length

                    tolerance : Quantity Float Pixels
                    tolerance =
                        Pixels.pixels 0.0000001
                in
                [ test "returns an empty list of polylines when given an empty list of commands" <|
                    \_ ->
                        []
                            |> Turtle.draw
                            |> Expect.equal []
                , test "returns an empty list of polylines when given a list of rotate commands" <|
                    \_ ->
                        Common.commandSequence1
                            |> Turtle.draw
                            |> Expect.equal []
                , test "returns an empty list of polylines when given a list of rotate and move commands" <|
                    \_ ->
                        Common.commandSequence2
                            |> Turtle.draw
                            |> Expect.equal []
                , test "returns an empty list of polylines when given a list of move commands" <|
                    \_ ->
                        Common.commandSequence3
                            |> Turtle.draw
                            |> Expect.equal []
                , test "returns a list of polylines equivalent to a single line segment when given a single draw command" <|
                    \_ ->
                        Common.commandSequence4
                            |> Turtle.draw
                            |> List.concatMap Polyline2d.segments
                            |> allLineSegmentsEqualWithin tolerance
                                (Polyline2d.segments <|
                                    Polyline2d.fromVertices
                                        [ Point2d.origin, Point2d.pixels 0 lengthInPixels ]
                                )
                , test "returns a list of polylines equivalent to as many line segments as draw commands were given" <|
                    \_ ->
                        Common.commandSequence5
                            |> Turtle.draw
                            |> List.concatMap Polyline2d.segments
                            |> allLineSegmentsEqualWithin tolerance
                                (Polyline2d.segments <|
                                    Polyline2d.fromVertices
                                        [ Point2d.origin
                                        , Point2d.pixels 0 lengthInPixels
                                        , Point2d.pixels 0 <| lengthInPixels * 2
                                        , Point2d.pixels 0 <| lengthInPixels * 3
                                        ]
                                )
                , test "returns a list of polylines forming a square when given alternating draw and rotate left commands" <|
                    \_ ->
                        Common.commandSequence6
                            |> Turtle.draw
                            |> List.concatMap Polyline2d.segments
                            |> allLineSegmentsEqualWithin tolerance
                                (Polyline2d.segments <|
                                    Polyline2d.fromVertices
                                        [ Point2d.origin
                                        , Point2d.pixels 0 lengthInPixels
                                        , Point2d.pixels -lengthInPixels lengthInPixels
                                        , Point2d.pixels -lengthInPixels 0
                                        , Point2d.origin
                                        ]
                                )
                , test "returns a list of polylines forming separate shapes when draw commands are interlaced with move commands" <|
                    \_ ->
                        Common.commandSequence7
                            |> Turtle.draw
                            |> List.concatMap Polyline2d.segments
                            |> allLineSegmentsEqualWithin tolerance
                                (List.concatMap Polyline2d.segments
                                    [ Polyline2d.fromVertices
                                        [ Point2d.origin
                                        , Point2d.pixels 0 lengthInPixels
                                        , Point2d.pixels -lengthInPixels lengthInPixels
                                        ]
                                    , Polyline2d.fromVertices
                                        [ Point2d.pixels (-lengthInPixels * 2) 0
                                        , Point2d.pixels (-lengthInPixels * 2) -lengthInPixels
                                        , Point2d.pixels -lengthInPixels -lengthInPixels
                                        ]
                                    ]
                                )
                ]
            , describe "Turtle.rotate"
                [ test "returns the same result as Turtle.rotateLeft" <|
                    \_ ->
                        Turtle.rotate Common.angle
                            |> Expect.equal (Turtle.rotateLeft Common.angle)
                ]
            ]
        ]
