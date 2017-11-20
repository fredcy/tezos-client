module Data.Michelson exposing (AST(..), Program, Script, decodeScript)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Script =
    { code : AST
    , storage : AST
    }


type alias Storage =
    Decode.Value


type alias Program =
    AST


type AST
    = PrimT String (List AST)
    | SeqT (List AST)
    | StringT String
    | IntT Int


decodeScript : Decode.Decoder Script
decodeScript =
    Decode.succeed Script
        |> Decode.required "code" decodeAST
        |> Decode.required "storage" decodeAST


decodeAST : Decoder AST
decodeAST =
    Decode.lazy
        (\() ->
            Decode.oneOf [ decodeIntT, decodeStringT, decodePrimT, decodeSeqT ]
        )


decodeIntT : Decoder AST
decodeIntT =
    Decode.field "int" Decode.string
        |> Decode.map String.toInt
        |> Decode.andThen
            (\result ->
                case result of
                    Ok int ->
                        Decode.succeed (IntT int)

                    Err _ ->
                        Decode.fail "bad int"
            )


decodeStringT : Decode.Decoder AST
decodeStringT =
    Decode.field "string" Decode.string |> Decode.map StringT


decodePrimT : Decode.Decoder AST
decodePrimT =
    Decode.succeed PrimT
        |> Decode.required "prim" Decode.string
        |> Decode.required "args" (Decode.lazy (\() -> (Decode.list decodeAST)))


decodeSeqT : Decoder AST
decodeSeqT =
    Decode.list (Decode.lazy (\() -> decodeAST)) |> Decode.map SeqT
