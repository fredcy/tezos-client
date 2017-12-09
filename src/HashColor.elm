module HashColor exposing (Model, init, toColor, toColorMemo)

import Array
import Dict exposing (Dict)
import ParseInt
import Sha256


type alias Model =
    Dict String String


init =
    Dict.empty


saturations =
    Array.fromList [ 35, 50, 65 ]


lightnesses =
    --Array.fromList [ 35, 50, 65 ]
    -- tweak to make less light so it looks better against white background
    Array.fromList [ 25, 40, 55 ]


{-| Generate a CSS color value (as a string) from some hash-like string.
This follows the scheme of <https://github.com/zenozeng/color-hash>.
-}
toColor : String -> String
toColor hash =
    let
        hashInt =
            hash
                |> Sha256.sha256
                -- rehashed (could have been any hash to hex)
                |> String.left 14
                -- kept implicit numeric value in reasonable range
                |> ParseInt.parseIntHex
                -- should never fail here but it is a Result value so ...
                |> Result.withDefault 0

        hue =
            hashInt % 359

        hash2 =
            hashInt // 360

        saturation =
            Array.get (hash2 % Array.length saturations) saturations |> Maybe.withDefault 50

        hash3 =
            hash2 // Array.length saturations

        lightness =
            Array.get (hash3 % Array.length lightnesses) lightnesses |> Maybe.withDefault 50
    in
        "hsl(" ++ toString hue ++ "," ++ toString saturation ++ "%," ++ toString lightness ++ "%)"


toColorMemo : Model -> String -> ( String, Model )
toColorMemo dict hash =
    case Dict.get hash dict of
        Just color ->
            ( color, dict )

        Nothing ->
            let
                color =
                    toColor hash
            in
                ( color, Dict.insert hash color dict )
