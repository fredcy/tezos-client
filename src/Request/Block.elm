module Request.Block exposing (getContract, getContractIDs, getChainStartingAt, getHeads, getKeys, getPeers, requestAccounts, requestChainSummary, requestChainSummary2, requestDelegations, requestTransactions)

import Json.Encode as Encode
import Json.Decode as Decode
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


getContractIDs : URL -> Http.Request (List Chain.ContractID)
getContractIDs nodeUrl =
    let
        url =
            nodeUrl ++ "/blocks/head/proto/context/contracts"
    in
        Http.post url emptyJsonBody Chain.decodeContractIDs


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


{-| Request Contract data in such a way that the raw response string is available to save and view.
-}
getContract : URL -> ContractID -> Http.Request Contract
getContract nodeUrl contractId =
    let
        url =
            nodeUrl ++ "/blocks/head/proto/context/contracts/" ++ contractId
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Content-Type" "application/json" ]
            , url = url
            , body = emptyJsonBody
            , expect = Http.expectStringResponse Chain.decodeContractResponse
            , timeout = Nothing
            , withCredentials = False
            }


requestAccounts : URL -> Http.Request (List Chain.AccountSummary)
requestAccounts nodeUrl =
    Http.get (nodeUrl ++ "/api/accounts") (Decode.list Chain.decodeAccountSummary)


requestTransactions : URL -> Chain.AccountID -> Http.Request (List Chain.TransactionSummary)
requestTransactions nodeUrl accountHash =
    Http.get (nodeUrl ++ "/api/account/" ++ accountHash) (Decode.list Chain.decodeTransaction)


requestChainSummary : URL -> Http.Request (List Chain.BlockSummary)
requestChainSummary nodeUrl =
    Http.get (nodeUrl ++ "/api/chainSummary") (Decode.list Chain.decodeBlockSummary)


requestChainSummary2 : URL -> Int -> Http.Request (List Chain.BlockSummary)
requestChainSummary2 nodeUrl length =
    let
        queryString =
            "?length=" ++ toString length
    in
        Http.get (nodeUrl ++ "/api/chainSummary" ++ queryString) (Decode.list Chain.decodeBlockSummary)


requestDelegations : URL -> Http.Request (List Chain.DelegationSummary)
requestDelegations nodeUrl =
    Http.get (nodeUrl ++ "/api/delegations") (Decode.list Chain.decodeDelegationSummary)
