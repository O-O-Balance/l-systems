module Tests.Common exposing (angle, commandSequence1, commandSequence2, commandSequence3, commandSequence4, commandSequence5, commandSequence6, commandSequence7, emptyRules, length, rules, rulesDict)

import Angle exposing (Angle)
import Dict exposing (Dict)
import LSystem exposing (Productions)
import LSystem.Rules as Rules
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Turtle exposing (Command)


emptyRules : Productions
emptyRules =
    Rules.empty


rules : Productions
rules =
    Rules.empty
        |> Rules.add 'A' "AB"
        |> Rules.add 'B' "A"
        |> Rules.add 'C' ""


rulesDict : Dict Char String
rulesDict =
    Dict.fromList
        [ ( 'A', "AB" )
        , ( 'B', "A" )
        , ( 'C', "" )
        ]


length : Quantity Float Pixels
length =
    Pixels.pixels 10


angle : Angle
angle =
    Angle.degrees 90


commandSequence1 : List (Command Pixels)
commandSequence1 =
    [ Turtle.rotateLeft angle
    , Turtle.rotateRight angle
    , Turtle.rotateLeft angle
    ]


commandSequence2 : List (Command Pixels)
commandSequence2 =
    [ Turtle.move length
    , Turtle.move length
    ]


commandSequence3 : List (Command Pixels)
commandSequence3 =
    [ Turtle.rotateLeft angle
    , Turtle.move length
    , Turtle.rotateRight angle
    , Turtle.rotateRight angle
    , Turtle.move length
    , Turtle.rotateLeft angle
    , Turtle.rotateLeft angle
    , Turtle.move length
    ]


commandSequence4 : List (Command Pixels)
commandSequence4 =
    [ Turtle.line length ]


commandSequence5 : List (Command Pixels)
commandSequence5 =
    [ Turtle.line length
    , Turtle.line length
    , Turtle.line length
    ]


commandSequence6 : List (Command Pixels)
commandSequence6 =
    [ Turtle.line length
    , Turtle.rotateLeft angle
    , Turtle.line length
    , Turtle.rotateLeft angle
    , Turtle.line length
    , Turtle.rotateLeft angle
    , Turtle.line length
    ]


commandSequence7 : List (Command Pixels)
commandSequence7 =
    [ Turtle.line length
    , Turtle.rotateLeft angle
    , Turtle.line length
    , Turtle.move length
    , Turtle.rotateLeft angle
    , Turtle.move length
    , Turtle.line length
    , Turtle.rotateLeft angle
    , Turtle.line length
    ]
