module LSystem exposing (apply, iteration)

import LSystem.Rules as Rules exposing (Rules)


apply : Rules -> String -> String
apply rules axiom =
    String.foldl
        (\variable result ->
            result
                ++ Rules.get variable rules
        )
        ""
        axiom


iteration : Rules -> String -> Int -> String
iteration rules axiom count =
    if count > 0 then
        iteration
            rules
            (apply rules axiom)
            (count - 1)

    else
        axiom
