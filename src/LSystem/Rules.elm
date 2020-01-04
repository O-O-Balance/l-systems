module LSystem.Rules exposing (Rules, add, empty, fromDict, get, toDict)

import Dict exposing (Dict)


type Rules
    = Rules (Dict Char String)


empty : Rules
empty =
    Rules Dict.empty


add : Char -> String -> Rules -> Rules
add variable substitution (Rules dictionary) =
    Rules <|
        Dict.insert variable substitution dictionary


fromDict : Dict Char String -> Rules
fromDict dictionary =
    Rules dictionary


toDict : Rules -> Dict Char String
toDict (Rules dictionary) =
    dictionary


get : Char -> Rules -> String
get variable (Rules dictionary) =
    Maybe.withDefault
        (String.fromChar variable)
        (Dict.get variable dictionary)
