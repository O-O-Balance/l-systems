module Tests.LSystem.Rules exposing (suite)

import Dict exposing (Dict)
import Expect
import LSystem.Rules as Rules
import Test exposing (Test, describe, test)
import Tests.Common as Common


suite : Test
suite =
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
            [ test "returns Maybe.Just the substitution string corresponding to a variable according to the production rules" <|
                \_ ->
                    Rules.get 'A' Common.rules
                        |> Expect.equal (Just "AB")
            , test "returns Maybe.Nothing when a constant is passed to it" <|
                \_ ->
                    Rules.get 'D' Common.rules
                        |> Expect.equal Nothing
            , test "returns Maybe.Nothing when given Rules.empty as its second argument" <|
                \_ ->
                    Rules.get 'A' Common.emptyRules
                        |> Expect.equal Nothing
            , test "returns Maybe.Just the empty string for a variable with that substitution string" <|
                \_ ->
                    Rules.get 'C' Common.rules
                        |> Expect.equal (Just "")
            ]
        , describe "Rules.toDict"
            [ test "returns an empty dictionary for Rules.empty" <|
                \_ ->
                    Common.emptyRules
                        |> Rules.toDict
                        |> Expect.equal Dict.empty
            , test "returns a dictionary containing a variable-substitution mapping for each rule" <|
                \_ ->
                    Common.rules
                        |> Rules.toDict
                        |> Expect.equal Common.rulesDict
            ]
        , describe "Rules.fromDict"
            [ test "returns Rules.empty when passed an empty dictionary" <|
                \_ ->
                    Dict.empty
                        |> Rules.fromDict
                        |> Expect.equal Common.emptyRules
            , test "returns a Rules value containing a Rule for each variable-substitution mapping in the dictionary" <|
                \_ ->
                    Common.rulesDict
                        |> Rules.fromDict
                        |> Expect.equal Common.rules
            ]
        ]
