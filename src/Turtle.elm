module Turtle exposing (Command, boundingBox, draw, line, move, pop, push, rotate, rotateLeft, rotateRight, viewBox)

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity(..))
import Vector2d


type Command units
    = Draw (Quantity Float units)
    | Move (Quantity Float units)
    | Rotate Angle
    | Push
    | Pop


type Turtle units coordinates
    = Turtle
        { position : Point2d units coordinates
        , facing : Direction2d coordinates
        , previous : Maybe (Turtle units coordinates)
        }


line : Quantity Float units -> Command units
line length =
    Draw length


move : Quantity Float units -> Command units
move distance =
    Move distance


rotate : Angle -> Command units
rotate =
    rotateLeft


rotateLeft : Angle -> Command units
rotateLeft angle =
    Rotate angle


rotateRight : Angle -> Command units
rotateRight angle =
    Rotate <| Quantity.negate angle


push : Command units
push =
    Push


pop : Command units
pop =
    Pop


start : Turtle units coordinates
start =
    Turtle
        { position = Point2d.origin
        , facing = Direction2d.y
        , previous = Nothing
        }


draw : List (Command units) -> List (Polyline2d units coordinates)
draw commandsList =
    List.foldl
        (\command accumulator ->
            let
                turtle =
                    case accumulator.turtle of
                        Turtle record ->
                            record

                newPosition : Quantity Float units -> Point2d units coordinates
                newPosition length =
                    Point2d.translateBy
                        (Vector2d.withLength length turtle.facing)
                        turtle.position
            in
            case command of
                Draw length ->
                    { accumulator
                        | turtle = Turtle { turtle | position = newPosition length }
                        , segments =
                            ( turtle.position, newPosition length )
                                :: accumulator.segments
                    }

                Move length ->
                    { accumulator
                        | turtle = Turtle { turtle | position = newPosition length }
                    }

                Rotate angle ->
                    { accumulator
                        | turtle = Turtle { turtle | facing = Direction2d.rotateBy angle turtle.facing }
                    }

                Push ->
                    { accumulator
                        | turtle = Turtle { turtle | previous = Just (Turtle turtle) }
                    }

                Pop ->
                    case turtle.previous of
                        Just previous ->
                            { accumulator | turtle = previous }

                        Nothing ->
                            accumulator
        )
        { turtle = start, segments = [] }
        commandsList
        |> .segments
        |> List.foldr
            (\( p, q ) lines ->
                case lines of
                    (previousQ :: previousP :: otherPoints) :: otherLines ->
                        if previousQ == p then
                            if Direction2d.from previousP previousQ == Direction2d.from previousP q then
                                (q :: previousP :: otherPoints) :: otherLines

                            else
                                (q :: previousQ :: previousP :: otherPoints) :: otherLines

                        else
                            [ q, p ] :: lines

                    _ ->
                        [ [ q, p ] ]
            )
            []
        |> List.reverse
        |> List.map (List.reverse >> Polyline2d.fromVertices)


boundingBox : List (Polyline2d units coordinates) -> Maybe (BoundingBox2d units coordinates)
boundingBox drawing =
    List.map Polyline2d.boundingBox drawing
        |> List.filterMap identity
        |> BoundingBox2d.aggregateN


viewBox : Quantity Float units -> BoundingBox2d units coordinates -> String
viewBox margin fromBoundingBox =
    let
        translateByMargin : Quantity Float units -> Quantity Float units
        translateByMargin coordinate =
            Quantity.minus margin coordinate

        expandByMargin : Quantity Float units -> Quantity Float units
        expandByMargin dimension =
            Quantity.twice margin
                |> Quantity.plus dimension

        unwrap : Quantity Float units -> Float
        unwrap (Quantity float) =
            float

        ( boundingBoxWidth, boundingBoxHeight ) =
            BoundingBox2d.dimensions fromBoundingBox
    in
    String.join " " <|
        List.map (unwrap >> String.fromFloat)
            [ translateByMargin <| BoundingBox2d.minX fromBoundingBox
            , translateByMargin <| BoundingBox2d.minY fromBoundingBox
            , expandByMargin boundingBoxWidth
            , expandByMargin boundingBoxHeight
            ]
