module LSystem exposing (CommandMapping, Productions, apply, commandMapping, commands, iteration, render)

import Angle exposing (Angle)
import LSystem.Rules as Rules exposing (Rules)
import Quantity exposing (Quantity)
import Turtle exposing (Command)


type alias CommandMapping units =
    Rules (List (Command units))


type alias Productions =
    Rules String


render : appendable -> (Char -> appendable) -> Rules appendable -> String -> appendable
render initial default rules axiom =
    String.foldl
        (\char accumulator ->
            case Rules.get char rules of
                Just correspondingValue ->
                    accumulator ++ correspondingValue

                Nothing ->
                    accumulator ++ default char
        )
        initial
        axiom


commandMapping : Quantity Float units -> Angle -> CommandMapping units
commandMapping length angle =
    Rules.empty
        |> Rules.add 'F' [ Turtle.line length ]
        |> Rules.add 'f' [ Turtle.move length ]
        |> Rules.add '+' [ Turtle.rotateLeft angle ]
        |> Rules.add '-' [ Turtle.rotateRight angle ]
        |> Rules.add '[' [ Turtle.push ]
        |> Rules.add ']' [ Turtle.pop ]


commands : Quantity Float units -> Angle -> String -> List (Command units)
commands length angle =
    commandMapping length angle
        |> render [] (always [])


apply : Productions -> String -> String
apply =
    render "" String.fromChar


iteration : Productions -> String -> Int -> String
iteration rules axiom count =
    if count > 0 then
        iteration
            rules
            (apply rules axiom)
            (count - 1)

    else
        axiom
