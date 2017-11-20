module Data.Michelson exposing (AST(..), Program, Script, decodeScript)

import Json.Decode as Decode
import Json.Decode.Pipeline as Decode


type alias Script =
    { code : Code
    , storage : Storage
    }


type alias Code =
    Decode.Value


type alias Storage =
    Decode.Value



-- TODO : parse new program representation, "Micheline"


decodeScript : Decode.Decoder Script
decodeScript =
    Decode.succeed Script
        |> Decode.required "code" Decode.value
        |> Decode.required "storage" decodeStorage


decodeStorage : Decode.Decoder Storage
decodeStorage =
    Decode.value


decodeProgram : Decode.Decoder Program
decodeProgram =
    decodeSeqT


type alias Program =
    AST


type AST
    = IntT Int
    | StringT String
    | SeqT (List AST)
    | PrimT String
    | PrimArgT String AST
    | EmptyT


decodeAST : Decode.Decoder AST
decodeAST =
    Decode.lazy
        (\_ ->
            Decode.oneOf
                [ decodeIntT, decodeStringT, decodeSeqT, decodePrimT, decodePrimArgT ]
        )


decodeSeqT : Decode.Decoder AST
decodeSeqT =
    Decode.list
        (Decode.lazy (\_ -> decodeAST))
        |> Decode.map SeqT


decodeStringT : Decode.Decoder AST
decodeStringT =
    Decode.field "string" Decode.string |> Decode.map StringT


decodeIntT : Decode.Decoder AST
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


decodePrimT : Decode.Decoder AST
decodePrimT =
    Decode.string |> Decode.map PrimT


decodePrimArgT : Decode.Decoder AST
decodePrimArgT =
    Decode.keyValuePairs (Decode.lazy (\_ -> decodeAST))
        |> Decode.andThen
            (\kvPairs ->
                case kvPairs of
                    [ ( prim, ast ) ] ->
                        Decode.succeed (PrimArgT prim ast)

                    [] ->
                        Decode.succeed EmptyT

                    _ ->
                        Decode.fail "bad kvPairs"
            )
