module LSystem exposing (Rules, apply, iteration)

import Dict exposing (Dict)


type alias Rules =
    Dict Char String


apply : Rules -> String -> String
apply rules axiom =
    String.foldl
        (\variable result ->
            result
                ++ Maybe.withDefault
                    (String.fromChar variable)
                    (Dict.get variable rules)
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
