module Request exposing (decodeDebug)

import Json.Decode as Decode


{-| This decoder is useful for debugging. It is basically the same as just
`Decode.value` except that it has the side-effect of logging the decoded value
along with a message.
-}
decodeDebug : String -> Decode.Decoder Decode.Value
decodeDebug message =
    Decode.value
        |> Decode.andThen
            (\value ->
                let
                    _ =
                        Debug.log message value
                in
                    Decode.value
            )
