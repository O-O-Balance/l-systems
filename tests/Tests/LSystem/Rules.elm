module Tests.LSystem.Rules exposing (suite)

import Dict exposing (Dict)
import Expect
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
                |> Rules.add 'C' ""

        rulesDict : Dict Char String
        rulesDict =
            Dict.fromList
                [ ( 'A', "AB" )
                , ( 'B', "A" )
                , ( 'C', "" )
                ]
    in
    describe "The LSystem.Rules module"
        [ describe "Rules.add"
            [ test "keeps only one rule when the same rule is added twice" <|
                \_ ->
                    Rules.empty
                        |> Rules.add 'A' "AB"
                        |> Rules.add 'A' "AB"
                        |> Rules.toDict
                        |> Expect.equal (Dict.singleton 'A' "AB")
            , test "keeps only the most-recently added rule for each variable" <|
                \_ ->
                    Rules.empty
                        |> Rules.add 'A' "AB"
                        |> Rules.add 'A' "C"
                        |> Rules.toDict
                        |> Expect.equal (Dict.singleton 'A' "C")
            ]
        , describe "Rules.get"
            [ test "returns the substitution string corresponding to a variable according to the production rules" <|
                \_ ->
                    Rules.get 'A' rules
                        |> Expect.equal "AB"
            , test "returns the constant when one is passed to it" <|
                \_ ->
                    Rules.get 'D' rules
                        |> Expect.equal "D"
            , test "returns the constant passed to it when given Rules.empty as its second argument" <|
                \_ ->
                    Rules.get 'A' Rules.empty
                        |> Expect.equal "A"
            , test "returns the empty string for a variable with that substitution string" <|
                \_ ->
                    Rules.get 'C' rules
                        |> Expect.equal ""
            ]
        , describe "Rules.toDict"
            [ test "returns an empty dictionary for Rules.empty" <|
                \_ ->
                    Rules.empty
                        |> Rules.toDict
                        |> Expect.equal Dict.empty
            , test "returns a dictionary containing a variable-substitution mapping for each rule" <|
                \_ ->
                    rules
                        |> Rules.toDict
                        |> Expect.equal rulesDict
            ]
        , describe "Rules.fromDict"
            [ test "returns Rules.empty when passed an empty dictionary" <|
                \_ ->
                    Dict.empty
                        |> Rules.fromDict
                        |> Expect.equal Rules.empty
            , test "returns a Rules value containing a Rule for each variable-substitution mapping in the dictionary" <|
                \_ ->
                    rulesDict
                        |> Rules.fromDict
                        |> Expect.equal rules
            ]
        ]
