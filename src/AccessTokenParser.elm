module AccessTokenParser exposing (parse)

import String
import Dict


parse : String -> Maybe String
parse hash =
    case String.toList hash of
        '#' :: values ->
            extractAccessToken (String.fromList values)

        _ ->
            Nothing


extractAccessToken : String -> Maybe String
extractAccessToken hash =
    let
        eachParam =
            (String.split "&" hash)

        eachPair =
            List.map (splitAtFirst '=') eachParam

        allParams =
            Dict.fromList eachPair
    in
        Dict.get "access_token" allParams


splitAtFirst : Char -> String -> ( String, String )
splitAtFirst c s =
    case (firstOccurrence c s) of
        Nothing ->
            ( s, "" )

        Just i ->
            ( (String.left i s), (String.dropLeft (i + 1) s) )


firstOccurrence : Char -> String -> Maybe Int
firstOccurrence c s =
    case (String.indexes (String.fromChar c) s) of
        [] ->
            Nothing

        head :: _ ->
            Just head
