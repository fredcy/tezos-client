module Request exposing (Response, ResponseData(..), getBranch, handleResponse)

import Http
import InfiniteScroll
import Data.Chain as Chain
import Request.Block


type alias Model base msg =
    { base
        | chain : Chain.Model
        , nodeUrl : String
        , infiniteScroll : InfiniteScroll.Model msg
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


handleResponse : Response -> Model base msg -> ( Model base msg, Cmd Response, Maybe Http.Error )
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


handleResponseData : ResponseData -> Model base msg -> ( Model base msg, Cmd Response )
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
            in
                ( newModel, Cmd.none )

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
            ( { model
                | chain = Chain.loadBlockSummaries model.chain blockSummaries
                , infiniteScroll = InfiniteScroll.stopLoading model.infiniteScroll
              }
            , Cmd.none
            )


{-| Request chain starting at given block (hash) if necessary. If we already
have some blocks stored, request only what is needed to get to some target
length.
-}
getBranch : Int -> Model base msg -> Chain.BlockID -> Cmd Response
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
