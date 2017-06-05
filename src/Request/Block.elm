module Request.Block exposing (..)

import Json.Encode as Encode
import Http
import Data.Chain as Chain exposing (BlockID, Contract, ContractID)
import Data.Request exposing (URL, emptyJsonBody)


{-| Construct request for all blockchain heads. Since we call this often to
determine the main head I avoid requesting operation details to keep the
response size smaller.
-}
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


getContracts : URL -> Http.Request Chain.Contracts
getContracts nodeUrl =
    let
        url =
            nodeUrl ++ "/blocks/head/proto/context/contracts"
    in
        Http.post url emptyJsonBody Chain.decodeContracts


getKeys : URL -> Http.Request (List Chain.Key)
getKeys nodeUrl =
    let
        url =
            nodeUrl ++ "/blocks/head/proto/context/keys"
    in
        Http.post url emptyJsonBody Chain.decodeKeys


{-| TODO : move to different module?
-}
getPeers : URL -> Http.Request (List Chain.Peer)
getPeers nodeUrl =
    let
        url =
            nodeUrl ++ "/network/peer_id"
    in
        Http.post url emptyJsonBody Chain.decodePeers


getContract : URL -> ContractID -> Http.Request Contract
getContract nodeUrl contractId =
    let
        url =
            nodeUrl ++ "/blocks/head/proto/context/contracts/" ++ contractId
    in
        Http.post url emptyJsonBody Chain.decodeContract
