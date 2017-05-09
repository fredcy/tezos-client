module Update exposing (update, Msg(..))

import Date
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Decode
import Http
import List.Extra as List
import Set
import Time exposing (Time)
import Model exposing (..)
import Data.Schema as Schema exposing (SchemaData, decodeSchema, collapseTrees)
import Data.Chain as Chain exposing (Block, BlockID, Operation, OperationID)
import Data.Request exposing (URL)
import Request.Block
import Request.Operation
import Route exposing (Route)


type Msg
    = LoadBlocks (Result Http.Error Chain.BlocksData)
    | LoadSchema SchemaName (Result Http.Error SchemaData)
    | LoadOperation (Result Http.Error Operation)
    | LoadBlockOperations BlockID (Result Http.Error Chain.BlockOperations)
    | LoadParsedOperation OperationID (Result Http.Error Chain.ParsedOperation)
    | SchemaMsg SchemaName Schema.Msg
    | ShowBlock BlockID
    | ShowBranch BlockID
    | LoadHeads (Result Http.Error Chain.BlocksData)
    | Tick Time
    | SetRoute (Maybe Route)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) |> Debug.log "update" of
        ( LoadBlocks blocksMaybe, _ ) ->
            case blocksMaybe of
                Ok blockChains ->
                    loadBlocks model blockChains

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        ( LoadSchema schemaName schemaMaybe, _ ) ->
            case schemaMaybe of
                Ok schemaData ->
                    ( { model | schemaData = Dict.insert schemaName (collapseTrees schemaData) model.schemaData }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

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
                    ( { model | errors = error :: model.errors }, Cmd.none )

        ( LoadParsedOperation operationId parseResult, _ ) ->
            case parseResult of
                Ok parse ->
                    let
                        newChain =
                            Chain.loadParsedOperation model.chain operationId parse
                    in
                        ( { model | chain = newChain }, Cmd.none )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        ( LoadBlockOperations blockhash result, _ ) ->
            case result of
                Ok operationListList ->
                    ( { model | chain = Chain.addBlockOperations model.chain blockhash operationListList }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        ( LoadHeads headsResult, _ ) ->
            case headsResult of
                Ok heads ->
                    loadHeads model heads

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        ( ShowBlock blockhash, _ ) ->
            ( { model | showBlock = Just blockhash }
            , getBlockOperationDetails model blockhash
            )

        ( ShowBranch hash, _ ) ->
            ( { model | showBranch = Just hash }
            , getBranch model hash
            )

        ( Tick time, _ ) ->
            ( { model | now = Date.fromTime time }
            , Request.Block.getHeads model.nodeUrl |> Http.send LoadHeads
            )

        ( SetRoute route, _ ) ->
            setRoute route model


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute routeMaybe model =
    case routeMaybe of
        Nothing ->
            ( { model | pageState = Loaded NotFound }, Cmd.none )

        Just (Route.Home) ->
            ( { model | pageState = Loaded Home }, Cmd.none )

        Just (Route.Schema) ->
            ( { model | pageState = Loaded Schema }, Cmd.none )


loadHeads : Model -> Chain.BlocksData -> ( Model, Cmd Msg )
loadHeads model headsData =
    let
        newChain : Chain.Model
        newChain =
            Chain.loadHeads model.chain headsData

        showBranch : Maybe BlockID
        showBranch =
            if model.showBranch == Nothing then
                -- default to displaying first branch
                Chain.head newChain
            else
                model.showBranch
    in
        ( { model | chain = newChain, showBranch = showBranch }
        , showBranch |> Maybe.map (getBranch model) |> Maybe.withDefault Cmd.none
        )


loadBlocks : Model -> Chain.BlocksData -> ( Model, Cmd Msg )
loadBlocks model blocksData =
    let
        newChain =
            Chain.loadBlocks model.chain blocksData
    in
        ( { model | chain = newChain }, getAllBlocksOperations model )


{-| Request chain starting at given block (hash) if necessary. If we already have some blocks stored, request only what is needed to get to some target length.
-}
getBranch : Model -> BlockID -> Cmd Msg
getBranch model blockhash =
    let
        branchList =
            Chain.getBranchList model.chain blockhash

        desiredLength =
            20

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


{-| Obsolete???
-}
getParseOperationCommand : String -> Operation -> Cmd Msg
getParseOperationCommand nodeUrl operation =
    Request.Operation.getParsed nodeUrl operation
        |> Http.send (LoadParsedOperation operation.hash)


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
