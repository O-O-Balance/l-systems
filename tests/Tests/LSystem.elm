module Tests.LSystem exposing (suite)

import Expect
import LSystem
import Test exposing (Test, describe, test)
import Tests.Common as Common


suite : Test
suite =
    describe "The LSystem module"
        [ describe "LSystem.apply"
            [ test "replaces variables with the substitution defined for them by the production rules" <|
                \_ ->
                    "ABA"
                        |> LSystem.apply Common.rules
                        |> Expect.equal "ABAAB"
            , test "returns variables that map to the empty string with the empty string" <|
                \_ ->
                    "CC"
                        |> LSystem.apply Common.rules
                        |> Expect.equal ""
            , test "leaves constants unchanged" <|
                \_ ->
                    "AxBA"
                        |> LSystem.apply Common.rules
                        |> Expect.equal "ABxAAB"
            , test "leaves strings with no variables unchanged" <|
                \_ ->
                    "xy"
                        |> LSystem.apply Common.rules
                        |> Expect.equal "xy"
            , test "leaves strings unchanged when passed Rules.empty as its first argument" <|
                \_ ->
                    "ABA"
                        |> LSystem.apply Common.emptyRules
                        |> Expect.equal "ABA"
            , test "returns the empty string when passed the empty string as its second argument" <|
                \_ ->
                    ""
                        |> LSystem.apply Common.rules
                        |> Expect.equal ""
            ]
        , describe "LSystem.iteration"
            [ test "returns the same results as repeated applications of LSystem.apply" <|
                \_ ->
                    List.range -1 5
                        |> List.map (LSystem.iteration Common.rules "A")
                        |> Expect.equal [ "A", "A", "AB", "ABA", "ABAAB", "ABAABABA", "ABAABABAABAAB" ]
            ]
        , describe "LSystem.commands"
            [ test "returns an empty list of commands when given an empty command string" <|
                \_ ->
                    ""
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal []
            , test "returns an empty list of commands when given a string of no-ops" <|
                \_ ->
                    "abc"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal []
            , test "returns rotate commands for '+' and '-' characters" <|
                \_ ->
                    "+-+"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal Common.commandSequence1
            , test "returns move commands for 'f' characters" <|
                \_ ->
                    "ff"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal Common.commandSequence2
            , test "returns move and rotate commands when given a string of 'f', '+', '-' and no-ops" <|
                \_ ->
                    "a+f-b-f+c+f"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal Common.commandSequence3
            , test "returns a single draw command when given a single 'F' character" <|
                \_ ->
                    "F"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal Common.commandSequence4
            , test "returns draw commands when for 'F' characters" <|
                \_ ->
                    "FFF"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal Common.commandSequence5
            , test "returns draw and rotate left commands when given a string of 'F' and '+'" <|
                \_ ->
                    "F+F+F+F"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal Common.commandSequence6
            , test "returns draw, move and rotate left commands when given a string of 'F', 'f' and '+'" <|
                \_ ->
                    "F+Ff+fF+F"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal Common.commandSequence7
            , test "returns draw, move and rotate left commands when given a string of 'F', 'f', '+' and no-ops" <|
                \_ ->
                    "aFbc1+Fdf+ef4F+5F"
                        |> LSystem.commands Common.length Common.angle
                        |> Expect.equal Common.commandSequence7
            ]
        ]
