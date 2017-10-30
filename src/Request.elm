module Request exposing (..)

import Http
import Json.Decode as Decode
import Data.Chain as Chain
import Request.Block
import Request.Operation


type alias Model base =
    { base
        | chain : Chain.Model
        , nodeUrl : String
    }


type alias Response =
    Result Http.Error ResponseData


type ResponseData
    = Blocks Chain.BlocksData
    | BlockOperations Chain.BlockID Chain.BlockOperations
    | Heads Chain.BlocksData
    | Head Chain.Block
    | AccountSummaries (List Chain.AccountSummary)
    | TransactionSummaries String (List Chain.TransactionSummary)
    | ChainSummary (List Chain.BlockSummary)


handleResponse : Response -> Model base -> ( Model base, Cmd Response, Maybe Http.Error )
handleResponse response model =
    case response of
        Ok responseData ->
            let
                ( newModel, cmd ) =
                    handleResponseData responseData model
            in
                ( newModel, cmd, Nothing )

        Err error ->
            ( model, Cmd.none, Just error )


handleResponseData : ResponseData -> Model base -> ( Model base, Cmd Response )
handleResponseData responseData model =
    case responseData of
        Heads blocksData ->
            let
                newModel =
                    { model | chain = Chain.loadHeads model.chain blocksData }
            in
                ( newModel
                , List.head newModel.chain.heads
                    |> Maybe.map (getBranch 24 model)
                    |> Maybe.withDefault Cmd.none
                )

        Blocks blocksData ->
            let
                newModel =
                    { model | chain = Chain.loadBlocks model.chain blocksData }

                blocksToGet =
                    Chain.blocksNeedingOperations newModel.chain

                getBlockOperations blockHash =
                    Request.Operation.getBlockOperations model.nodeUrl blockHash
                        |> Http.send (Result.map (BlockOperations blockHash))
            in
                ( newModel
                , Cmd.batch (List.map getBlockOperations blocksToGet)
                )

        BlockOperations blockid operationsData ->
            ( { model | chain = Chain.addBlockOperations model.chain blockid operationsData }
            , Cmd.none
            )

        Head block ->
            -- TODO
            let
                _ =
                    Debug.log "head" block
            in
                ( model, Cmd.none )

        AccountSummaries summaries ->
            ( { model | chain = Chain.setAccountSummaries model.chain summaries }
            , Cmd.none
            )

        TransactionSummaries accountHash transactions ->
            ( { model | chain = Chain.setAccountInfo model.chain accountHash transactions }
            , Cmd.none
            )

        ChainSummary blockSummaries ->
            let
                _ =
                    Debug.log "blockSummaries" blockSummaries
            in
                ( { model | chain = Chain.loadBlockSummaries model.chain blockSummaries }
                , Cmd.none
                )


getBlockOperationDetails : Model b -> Chain.BlockID -> Cmd Response
getBlockOperationDetails model blockHash =
    if Chain.blockNeedsOperations model.chain blockHash then
        Request.Operation.getBlockOperations model.nodeUrl blockHash
            |> Http.send (Result.map (BlockOperations blockHash))
    else
        Cmd.none


{-| Request chain starting at given block (hash) if necessary. If we already
have some blocks stored, request only what is needed to get to some target
length.
-}
getBranch : Int -> Model base -> Chain.BlockID -> Cmd Response
getBranch desiredLength model blockhash =
    let
        branchList =
            Chain.getBranchList model.chain blockhash

        toGet =
            desiredLength - List.length branchList
    in
        if toGet > 0 then
            let
                startHash =
                    List.reverse branchList
                        |> List.head
                        |> Maybe.map .predecessor
                        |> Maybe.withDefault blockhash
            in
                Request.Block.getChainStartingAt model.nodeUrl toGet startHash
                    |> Http.send (Result.map Blocks)
        else
            Cmd.none
