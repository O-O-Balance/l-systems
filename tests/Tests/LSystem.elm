module Tests.LSystem exposing (suite)

import Expect
import LSystem
import LSystem.Rules as Rules exposing (Rules)
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        rules : Rules
        rules =
            Rules.empty
                |> Rules.add 'A' "AB"
                |> Rules.add 'B' "A"
    in
    describe "The LSystem module"
        [ describe "LSystem.apply"
            [ test "replaces variables with the substitution defined for them by the production rules" <|
                \_ ->
                    "ABA"
                        |> LSystem.apply rules
                        |> Expect.equal "ABAAB"
            , test "leaves constants unchanged" <|
                \_ ->
                    "AxBA"
                        |> LSystem.apply rules
                        |> Expect.equal "ABxAAB"
            , test "leaves strings with no variables unchanged" <|
                \_ ->
                    "xy"
                        |> LSystem.apply rules
                        |> Expect.equal "xy"
            , test "leaves strings unchanged when passed Rules.empty as its first argument" <|
                \_ ->
                    "ABA"
                        |> LSystem.apply Rules.empty
                        |> Expect.equal "ABA"
            , test "returns the empty string when passed the empty string as its second argument" <|
                \_ ->
                    ""
                        |> LSystem.apply rules
                        |> Expect.equal ""
            ]
        , describe "LSystem.iteration"
            [ test "returns the same results as repeated applications of LSystem.apply" <|
                \_ ->
                    List.range -1 5
                        |> List.map (LSystem.iteration rules "A")
                        |> Expect.equal [ "A", "A", "AB", "ABA", "ABAAB", "ABAABABA", "ABAABABAABAAB" ]
            ]
        ]
