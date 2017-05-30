module Update exposing (update, Msg(..), setRoute)

import Date
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Decode
import Http
import List.Extra as List
import Set
import Task
import Time exposing (Time)
import Model exposing (..)
import Data.Schema as Schema exposing (SchemaData, SchemaName, decodeSchema, collapseTrees)
import Data.Chain as Chain exposing (Block, BlockID, Operation, OperationID, decodeBlocks)
import Data.Request exposing (URL)
import Page exposing (Page)
import Request.Block
import Request.Operation
import Request.Schema exposing (getSchema)
import Route exposing (Route)


type Msg
    = LoadBlocks (Result Http.Error Chain.BlocksData)
    | LoadSchema SchemaName (Result Http.Error SchemaData)
    | LoadOperation (Result Http.Error Operation)
    | LoadBlockOperations BlockID (Result Http.Error Chain.BlockOperations)
    | LoadParsedOperation OperationID (Result Http.Error Chain.ParsedOperation)
    | SchemaMsg SchemaName Schema.Msg
    | ShowBlock BlockID
    | ShowOperation OperationID
    | ShowBranch BlockID
    | LoadHeads (Result Http.Error Chain.BlocksData)
    | LoadContracts (Result Http.Error Chain.Contracts)
    | LoadKeys (Result Http.Error (List Chain.Key))
    | Tick Time
    | SetRoute (Maybe Route)
    | ClearErrors
    | Monitor Decode.Value
    | Now Time
    | LoadChainAt BlockID (Result Http.Error Chain.BlocksData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) of
        ( LoadBlocks blocksMaybe, _ ) ->
            case blocksMaybe of
                Ok blockChains ->
                    loadBlocks model blockChains

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( LoadSchema schemaName schemaMaybe, _ ) ->
            case schemaMaybe of
                Ok schemaData ->
                    ( { model | schemaData = Dict.insert schemaName (collapseTrees schemaData) model.schemaData }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( SchemaMsg name msg, _ ) ->
            let
                newSchemaMaybe : Maybe SchemaData
                newSchemaMaybe =
                    Dict.get name model.schemaData |> Maybe.map (Schema.update msg)
            in
                case newSchemaMaybe of
                    Just newSchema ->
                        ( { model | schemaData = Dict.insert name newSchema model.schemaData }, Cmd.none )

                    Nothing ->
                        let
                            _ =
                                Debug.log "Failed to find schema" name
                        in
                            ( model, Cmd.none )

        ( LoadOperation operationResult, _ ) ->
            case operationResult of
                Ok operation ->
                    let
                        newChain =
                            Chain.loadOperation model.chain operation
                    in
                        ( { model | chain = newChain }, Cmd.none )

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( LoadParsedOperation operationId parseResult, _ ) ->
            case parseResult of
                Ok parse ->
                    let
                        newChain =
                            Chain.loadParsedOperation model.chain operationId parse
                    in
                        ( { model | chain = newChain }, Cmd.none )

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( LoadBlockOperations blockhash result, _ ) ->
            case result of
                Ok operationListList ->
                    ( { model | chain = Chain.addBlockOperations model.chain blockhash operationListList }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( LoadHeads headsResult, _ ) ->
            case headsResult of
                Ok heads ->
                    loadHeads model heads

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( LoadChainAt hash blocksResult, _ ) ->
            case blocksResult of
                Ok blocks ->
                    loadBlocks model blocks

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( LoadContracts contractsResult, _ ) ->
            case contractsResult of
                Ok contracts ->
                    ( { model | chain = Chain.loadContracts model.chain contracts }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | errors = HttpError error :: model.errors
                        , chain = Chain.loadContractError model.chain error
                      }
                    , Cmd.none
                    )

        ( LoadKeys keysResult, _ ) ->
            case keysResult of
                Ok keys ->
                    ( { model | chain = Chain.loadKeys model.chain keys }, Cmd.none )

                Err error ->
                    ( { model
                        | errors = HttpError error :: model.errors
                        , chain = Chain.loadKeysError model.chain error
                      }
                    , Cmd.none
                    )

        ( ShowBlock blockhash, _ ) ->
            ( model
            , Cmd.batch
                [ getBlockOperationDetails model blockhash
                , Route.newUrl (Route.Block blockhash)
                ]
            )

        ( ShowOperation operationId, _ ) ->
            ( model, Route.newUrl (Route.Operation operationId) )

        ( ShowBranch hash, _ ) ->
            ( model
            , Route.newUrl (Route.ChainAt hash)
            )

        ( Tick time, _ ) ->
            ( { model | now = Date.fromTime time }
            , Request.Block.getHeads model.nodeUrl |> Http.send LoadHeads
            )

        ( Now time, _ ) ->
            ( { model | now = Date.fromTime time }, Cmd.none )

        ( SetRoute route, _ ) ->
            setRoute route model

        ( ClearErrors, _ ) ->
            ( { model | errors = [] }, Cmd.none )

        ( Monitor data, _ ) ->
            let
                ( newModel, cmd ) =
                    updateMonitor data model
            in
                ( newModel, Cmd.batch [ cmd, Task.perform Now Time.now ] )


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute routeMaybe model =
    case routeMaybe of
        Nothing ->
            ( { model | pageState = Loaded Page.NotFound }, Cmd.none )

        Just Route.Home ->
            ( { model | pageState = Loaded Page.Home }, Cmd.none )

        Just (Route.Block hash) ->
            ( { model | pageState = Loaded (Page.Block hash) }
            , getBlock model hash
            )

        Just Route.Operations ->
            ( { model | pageState = Loaded Page.Operations }, Cmd.none )

        Just (Route.Operation operationId) ->
            ( { model | pageState = Loaded (Page.Operation operationId) }, Cmd.none )

        Just Route.Heads ->
            ( { model | pageState = Loaded Page.Heads }, Cmd.none )

        Just (Route.ChainAt hash) ->
            ( { model | pageState = Loaded (Page.ChainAt hash) }
            , getBranch model hash
            )

        Just Route.Contracts ->
            ( { model
                | pageState = Loaded Page.Contracts
                , chain = Chain.loadingContracts model.chain
              }
            , getContracts model
            )

        Just Route.Keys ->
            ( { model
                | pageState = Loaded Page.Keys
                , chain = Chain.loadingKeys model.chain
              }
            , getKeys model
            )

        Just Route.Schema ->
            let
                schemaQuery1 =
                    "/describe"

                schemaQuery2 =
                    "/describe/blocks/head/proto"
            in
                ( { model | pageState = Loaded Page.Schema }
                , Cmd.batch
                    [ getSchema model.nodeUrl schemaQuery1 |> Http.send (LoadSchema schemaQuery1)
                    , getSchema model.nodeUrl schemaQuery2 |> Http.send (LoadSchema schemaQuery2)
                    ]
                )

        Just Route.Debug ->
            ( { model | pageState = Loaded Page.Debug }, Cmd.none )

        Just Route.Errors ->
            ( { model | pageState = Loaded Page.Errors }, Cmd.none )


loadHeads : Model -> Chain.BlocksData -> ( Model, Cmd Msg )
loadHeads model headsData =
    let
        newChain : Chain.Model
        newChain =
            Chain.loadHeads model.chain headsData

        showBranch : Maybe BlockID
        showBranch =
            List.head newChain.heads
    in
        ( { model | chain = newChain }
        , showBranch |> Maybe.map (getBranch model) |> Maybe.withDefault Cmd.none
        )


loadBlocks : Model -> Chain.BlocksData -> ( Model, Cmd Msg )
loadBlocks model blocksData =
    let
        newChain =
            Chain.loadBlocks model.chain blocksData

        newModel =
            { model | chain = newChain }
    in
        ( newModel, getAllBlocksOperations newModel )


getBlock : Model -> BlockID -> Cmd Msg
getBlock model hash =
    case Dict.get hash model.chain.blocks of
        Nothing ->
            -- request block and some predecessors in anticipation of user following the chain
            Request.Block.getChainStartingAt model.nodeUrl 4 hash
                |> Http.send LoadBlocks

        _ ->
            Cmd.none


{-| Request chain starting at given block (hash) if necessary. If we already have some blocks stored, request only what is needed to get to some target length.
-}
getBranch : Model -> BlockID -> Cmd Msg
getBranch model blockhash =
    let
        branchList =
            Chain.getBranchList model.chain blockhash

        desiredLength =
            24

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
                Request.Block.getChainStartingAt model.nodeUrl toGet startHash |> Http.send LoadBlocks
        else
            Cmd.none


getBlockOperationDetails : Model -> BlockID -> Cmd Msg
getBlockOperationDetails model blockHash =
    if Chain.blockNeedsOperations model.chain blockHash then
        Request.Operation.getBlockOperations model.nodeUrl blockHash
            |> Http.send (LoadBlockOperations blockHash)
    else
        Cmd.none


getAllBlocksOperations : Model -> Cmd Msg
getAllBlocksOperations model =
    let
        blocksToGet =
            Chain.blocksNeedingOperations model.chain

        getBlockOperations blockHash =
            Request.Operation.getBlockOperations model.nodeUrl blockHash
                |> Http.send (LoadBlockOperations blockHash)
    in
        Cmd.batch (List.map getBlockOperations blocksToGet)


getContracts : Model -> Cmd Msg
getContracts model =
    Request.Block.getContracts model.nodeUrl |> Http.send LoadContracts


getKeys : Model -> Cmd Msg
getKeys model =
    Request.Block.getKeys model.nodeUrl |> Http.send LoadKeys


updateMonitor : Decode.Value -> Model -> ( Model, Cmd Msg )
updateMonitor data model =
    let
        blocksResult =
            Decode.decodeValue decodeBlocks data

        newModel =
            case blocksResult of
                Ok blocks ->
                    { model | chain = Chain.updateMonitor model.chain blocks }

                Err error ->
                    { model | errors = OtherError error :: model.errors }

        blockHashes =
            blocksResult
                |> Result.map (List.concat >> (List.map .hash))
                |> Result.withDefault []

        cmd =
            blockHashes
                |> List.map (getBlockOperationDetails model)
                |> Cmd.batch
    in
        ( newModel, cmd )
