module Request.Operation exposing (..)

import Http
import Json.Encode as Encode
import Data.Chain as Chain exposing (BlockID, Operation)
import Data.Request exposing (URL)


getBlockOperations : URL -> BlockID -> Http.Request Chain.BlockOperations
getBlockOperations nodeUrl blockHash =
    let
        url =
            nodeUrl ++ "/blocks/" ++ blockHash ++ "/proto/operations"
    in
        Http.post url Data.Request.emptyJsonBody Chain.decodeBlockOperationDetails


{-| Form RPC command to parse the given operation.
Used ???
-}
getParsed : String -> Operation -> Http.Request Chain.ParsedOperation
getParsed nodeUrl operation =
    let
        url =
            nodeUrl ++ "/blocks/head/proto/helpers/parse/operation"

        body =
            [ ( "data", Encode.string operation.data )
            , ( "net_id", Encode.string operation.netID )
            ]
                |> Encode.object
                |> Http.jsonBody
    in
        Http.post url body Chain.decodeParsedOperation
