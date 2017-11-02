module Api exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias OperationGroup =
    { hash : String
    , source : String
    , operations : List Operation
    }


type Operation
    = Endorsement
        { block : String
        , slot : Int
        }
    | SeedNonceRevelation
        { level : Int
        , nonce : String
        }
    | Transaction
        { amount : Int
        , destination : String
        }
    | Unknown Decode.Value


example =
    Endorsement { block = "foo", slot = 42 }


requestBlockOperations : String -> String -> Http.Request (List OperationGroup)
requestBlockOperations nodeUrl blockHash =
    Http.get (nodeUrl ++ "/api/block/" ++ blockHash ++ "/operations") (Decode.list decodeOperationGroup)


decodeOperationGroup : Decode.Decoder OperationGroup
decodeOperationGroup =
    Decode.succeed OperationGroup
        |> Decode.required "Hash" Decode.string
        |> Decode.required "Source" Decode.string
        |> Decode.required "Operations" (Decode.list decodeOperation)


decodeOperation : Decode.Decoder Operation
decodeOperation =
    Decode.field "Kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "endorsement" ->
                        Decode.field "Operation" decodeEndorsement

                    "seed_nonce_revelation" ->
                        Decode.field "Operation" decodeSeedNonceRevelation

                    "transaction" ->
                        Decode.field "Operation" decodeTransaction

                    _ ->
                        Decode.value |> Decode.map Unknown
            )


decodeEndorsement : Decoder Operation
decodeEndorsement =
    Decode.map2
        (\block slot -> Endorsement { block = block, slot = slot })
        (Decode.field "Block" Decode.string)
        (Decode.field "Slot" Decode.int)


decodeTransaction : Decoder Operation
decodeTransaction =
    Decode.map2
        (\amount dest -> Transaction { amount = amount, destination = dest })
        (Decode.field "Amount" Decode.int)
        (Decode.field "Destination" Decode.string)


decodeSeedNonceRevelation : Decoder Operation
decodeSeedNonceRevelation =
    Decode.map2
        (\level nonce -> SeedNonceRevelation { level = level, nonce = nonce })
        (Decode.field "Level" Decode.int)
        (Decode.field "Nonce" Decode.string)
