module Turtle exposing (Command, boundingBox, commands, draw, line, move, rotate, rotateLeft, rotateRight, viewBox)

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity(..))
import Vector2d


type CommandMapping units
    = CommandMapping (Dict Char (Command units))


type Command units
    = Draw (Quantity Float units)
    | Move (Quantity Float units)
    | Rotate Angle


type Turtle units coordinates
    = Turtle (Point2d units coordinates) (Direction2d coordinates)


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
    Rotate <| Quantity.negate angle


rotateRight : Angle -> Command units
rotateRight angle =
    Rotate angle


commandMapping : Quantity Float units -> Angle -> CommandMapping units
commandMapping length angle =
    CommandMapping <|
        Dict.fromList
            [ ( 'F', line length )
            , ( 'f', move length )
            , ( '+', rotateLeft angle )
            , ( '-', rotateRight angle )
            ]


commands : Quantity Float units -> Angle -> String -> List (Command units)
commands length angle commandsString =
    case commandMapping length angle of
        CommandMapping dictionary ->
            String.foldl
                (\commandCharacter previousCommands ->
                    case Dict.get commandCharacter dictionary of
                        Just command ->
                            List.append previousCommands [ command ]

                        Nothing ->
                            previousCommands
                )
                []
                commandsString


turtle : Turtle units coordinates
turtle =
    Turtle Point2d.origin Direction2d.y


draw : List (Command units) -> List (Polyline2d units coordinates)
draw commandsList =
    List.foldl
        (\command ( Turtle position facing, lines ) ->
            let
                newPosition : Quantity Float units -> Point2d units coordinates
                newPosition length =
                    Point2d.translateBy
                        (Vector2d.withLength length facing)
                        position
            in
            case command of
                Draw length ->
                    ( Turtle (newPosition length) facing
                    , List.append lines
                        [ Polyline2d.fromVertices [ position, newPosition length ] ]
                    )

                Move length ->
                    ( Turtle (newPosition length) facing
                    , lines
                    )

                Rotate angle ->
                    ( Turtle position <|
                        Direction2d.rotateBy angle facing
                    , lines
                    )
        )
        ( turtle, [] )
        commandsList
        |> Tuple.second


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
