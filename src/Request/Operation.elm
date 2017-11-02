module Request.Operation exposing (..)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Data.Chain as Chain exposing (BlockID, Operation, ParsedOperation, SubOperation(..))
import Data.Request exposing (URL)
import Request.Lib


{-| Get parsed operations for block.
-}
getBlockOperations : URL -> BlockID -> Http.Request Chain.BlockOperations
getBlockOperations nodeUrl blockHash =
    let
        _ =
            Debug.crash "getBlockOperations"

        url =
            nodeUrl ++ "/blocks/" ++ blockHash ++ "/proto/operations"
    in
        Http.post url Data.Request.emptyJsonBody decodeBlockOperationDetails


decodeBlockOperationDetails : Decode.Decoder Chain.BlockOperations
decodeBlockOperationDetails =
    -- I don't understand why the RPC response data has two levels of lists. Anyway...
    Decode.field "ok" (Decode.list (Decode.list decodeParsedOperation))


decodeParsedOperation : Decode.Decoder ParsedOperation
decodeParsedOperation =
    Decode.succeed ParsedOperation
        |> Decode.required "hash" Decode.string
        |> Decode.required "net_id" Decode.string
        |> Decode.required "branch" Decode.string
        |> Decode.required "operations" (Decode.list decodeSubOperation)
        |> Decode.optional "source" (Decode.map Just Decode.string) Nothing
        |> Decode.optional "signature" (Decode.map Just Decode.string) Nothing


decodeSubOperation : Decode.Decoder SubOperation
decodeSubOperation =
    Decode.oneOf
        [ decodeEndorsement
        , Decode.map Unknown Decode.value
        ]


decodeEndorsement : Decode.Decoder SubOperation
decodeEndorsement =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "endorsement" ->
                        (Decode.map2 Endorsement
                            (Decode.field "block" Decode.string)
                            (Decode.field "slot" Decode.int)
                        )

                    "seed_nonce_revelation" ->
                        Decode.map2 SeedNonceRevelation
                            (Decode.field "level" Decode.int)
                            (Decode.field "nonce" Decode.string)

                    "transaction" ->
                        Decode.map2 Transaction
                            (Decode.field "destination" Decode.string)
                            (Decode.field "amount" Decode.int)

                    "faucet" ->
                        Decode.map2 Faucet
                            (Decode.field "id" Decode.string)
                            (Decode.field "nonce" Decode.string)

                    "delegation" ->
                        Decode.map Delegation
                            (Decode.field "delegate" Decode.string)

                    _ ->
                        Request.Lib.decodeDebug "unknown kind" |> Decode.map Unknown
            )
