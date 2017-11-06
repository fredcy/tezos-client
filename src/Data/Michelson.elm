module Data.Michelson exposing (AST(..), Program, Script, decodeScript)

import Json.Decode as Decode
import Json.Decode.Pipeline as Decode


type alias Script =
    { code : Code
    , storage : Storage
    }


type alias Code =
    { code : Program

    -- TODO : decode structure of the following
    , argType : Decode.Value
    , retType : Decode.Value
    , storageType : Decode.Value
    }


type alias Storage =
    { storage : Decode.Value
    , storageType : Decode.Value
    }


decodeScript : Decode.Decoder Script
decodeScript =
    Decode.succeed Script
        |> Decode.required "code" decodeCode
        |> Decode.required "storage" decodeStorage


decodeCode : Decode.Decoder Code
decodeCode =
    Decode.succeed Code
        |> Decode.required "code" decodeProgram
        |> Decode.required "argType" Decode.value
        |> Decode.required "retType" Decode.value
        |> Decode.required "storageType" Decode.value


decodeStorage : Decode.Decoder Storage
decodeStorage =
    Decode.succeed Storage
        |> Decode.required "storage" Decode.value
        |> Decode.required "storageType" Decode.value


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
