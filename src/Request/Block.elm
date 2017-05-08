module Request.Block exposing (..)

import Json.Encode as Encode
import Http
import Data.Chain as Chain exposing (BlockID)
import Data.Request exposing (URL, emptyJsonBody)


getHeads : URL -> Http.Request Chain.BlocksData
getHeads nodeUrl =
    let
        url =
            nodeUrl ++ "/blocks"
    in
        Http.post url emptyJsonBody Chain.decodeBlocks


getChainStartingAt : URL -> Int -> BlockID -> Http.Request Chain.BlocksData
getChainStartingAt nodeUrl length blockhash =
    let
        body =
            [ ( "include_ops", Encode.bool True )
            , ( "length", Encode.int length )
            , ( "heads", Encode.list [ Encode.string blockhash ] )
            ]
                |> Encode.object
                |> Http.jsonBody

        url =
            nodeUrl ++ "/blocks"
    in
        Http.post url body Chain.decodeBlocks
