module LSystem.Rules exposing (Rules, add, empty, fromDict, get, toDict)

import Dict exposing (Dict)


type Rules a
    = Rules (Dict Char a)


empty : Rules a
empty =
    Rules Dict.empty


add : Char -> a -> Rules a -> Rules a
add variable substitution (Rules dictionary) =
    Rules <|
        Dict.insert variable substitution dictionary


fromDict : Dict Char a -> Rules a
fromDict dictionary =
    Rules dictionary


toDict : Rules a -> Dict Char a
toDict (Rules dictionary) =
    dictionary


get : Char -> Rules a -> Maybe a
get variable (Rules dictionary) =
    Dict.get variable dictionary
