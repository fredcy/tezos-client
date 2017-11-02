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
    | Origination
        { manager : String
        , balance : Int
        , spendable : Bool
        , delegatable : Bool
        }
    | Delegation { delegate : String }
    | Faucet
        { id : String
        , nonce : String
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

                    "faucet" ->
                        Decode.field "Operation" decodeFaucet

                    "origination" ->
                        Decode.field "Operation" decodeOrigination

                    "delegation" ->
                        Decode.field "Operation" decodeDelegation

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


decodeOrigination : Decoder Operation
decodeOrigination =
    Decode.map4
        (\m b s d -> Origination { manager = m, balance = b, spendable = s, delegatable = d })
        (Decode.field "Manager" Decode.string)
        (Decode.field "Balance" Decode.int)
        (Decode.field "Spendable" Decode.bool)
        (Decode.field "Delegatable" Decode.bool)


decodeDelegation : Decoder Operation
decodeDelegation =
    Decode.map
        (\d -> Delegation { delegate = d })
        (Decode.field "Delegate" Decode.string)


decodeFaucet : Decoder Operation
decodeFaucet =
    Decode.map2
        (\id nonce -> Faucet { id = id, nonce = nonce })
        (Decode.field "Id" Decode.string)
        (Decode.field "Nonce" Decode.string)


decodeSeedNonceRevelation : Decoder Operation
decodeSeedNonceRevelation =
    Decode.map2
        (\level nonce -> SeedNonceRevelation { level = level, nonce = nonce })
        (Decode.field "Level" Decode.int)
        (Decode.field "Nonce" Decode.string)
