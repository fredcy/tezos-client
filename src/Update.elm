module Update exposing (update, setRoute, toPage)

import Date
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Http
import InfiniteScroll
import List.Extra as List
import Process
import Task
import Table
import Time exposing (Time)
import Window
import Api
import Model exposing (Error(HttpError, OtherError), Model, PageState(Loaded), getPage)
import Data.Schema as Schema exposing (SchemaData, SchemaName, collapseTrees)
import Data.Chain as Chain exposing (BlockID, OperationID, decodeBlocks)
import Data.Request exposing (URL)
import Msg exposing (..)
import Page exposing (Page)
import Request
import Request.Block
import Request.Schema exposing (getSchema)
import Route exposing (Route)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


addError : Error -> Model -> Model
addError error model =
    { model | errors = error :: model.errors }


addErrorMaybe : Maybe Error -> Model -> Model
addErrorMaybe errorMaybe model =
    case errorMaybe of
        Just error ->
            addError error model

        Nothing ->
            model


log : Msg -> Msg
log msg =
    case msg of
        InfiniteScroll _ ->
            msg

        _ ->
            Debug.log "msg" msg


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    case ( msg, page ) of
        ( RpcResponse response, _ ) ->
            let
                ( newModel, cmd, errorMaybe ) =
                    Request.handleResponse response model
            in
                ( addErrorMaybe (Maybe.map HttpError errorMaybe) newModel, Cmd.map RpcResponse cmd )

        ( LoadSchema schemaName schemaMaybe, _ ) ->
            case schemaMaybe of
                Ok schemaData ->
                    ( { model | schemaData = Dict.insert schemaName (collapseTrees schemaData) model.schemaData }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( SchemaMsg name smsg, _ ) ->
            let
                newSchemaMaybe : Maybe SchemaData
                newSchemaMaybe =
                    Dict.get name model.schemaData |> Maybe.map (Schema.update smsg)
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

        ( LoadBlockOperations blockHash operationGroupsResult, _ ) ->
            case operationGroupsResult of
                Ok operationGroups ->
                    let
                        chain =
                            model.chain

                        newChain =
                            { chain | blockOperationGroups = Dict.insert blockHash operationGroups chain.blockOperationGroups }
                    in
                        ( { model | chain = newChain }, Cmd.none )

                Err error ->
                    ( { model | errors = HttpError error :: model.errors }, Cmd.none )

        ( LoadContractIDs contractsResult, _ ) ->
            case contractsResult of
                Ok contractIDs ->
                    let
                        newChain =
                            Chain.loadContractIDs model.chain contractIDs
                    in
                        ( { model | chain = newChain }
                        , List.filter (Chain.contractHasData model.chain >> not) contractIDs
                            |> getContractDetails model.nodeUrl
                        )

                Err error ->
                    ( { model
                        | errors = HttpError error :: model.errors
                        , chain = Chain.loadContractIDsError model.chain error
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

        ( LoadPeers peersResult, _ ) ->
            case peersResult of
                Ok peers ->
                    ( { model | chain = Chain.loadPeers model.chain peers }, Cmd.none )

                Err error ->
                    ( { model
                        | errors = HttpError error :: model.errors
                        , chain = Chain.loadPeersError model.chain error
                      }
                    , Cmd.none
                    )

        ( LoadContract contractId contractResult, _ ) ->
            case contractResult of
                Ok contract ->
                    ( { model | chain = Chain.loadContract model.chain contractId contract }, Cmd.none )

                Err error ->
                    ( { model
                        | errors = HttpError error :: model.errors
                        , chain = Chain.loadContractError model.chain contractId error
                      }
                    , Cmd.none
                    )

        ( Tick time, _ ) ->
            ( { model | now = Date.fromTime time }
            , Request.Block.getHeads model.nodeUrl |> Http.send (Result.map Request.Heads >> RpcResponse)
            )

        ( Now time, _ ) ->
            ( { model | now = Date.fromTime time }, Cmd.none )

        ( SetRoute route, _ ) ->
            setRoute route model

        ( ClearErrors, _ ) ->
            ( { model | errors = [] }, Cmd.none )

        ( Monitor _, _ ) ->
            let
                lengthToRequest =
                    List.length model.chain.blockSummaries + 1
            in
                ( model
                , Cmd.batch
                    [ Task.perform Now Time.now
                    , Request.Block.requestChainSummary2 model.nodeUrl lengthToRequest
                        |> Http.send (Result.map Request.ChainSummary >> RpcResponse)
                    ]
                )

        ( SetTableState tableState, _ ) ->
            ( { model | tableState = tableState }, Cmd.none )

        ( SetTransactionTableState tableState, _ ) ->
            ( { model | transactionTableState = tableState }, Cmd.none )

        ( SetContractTableState tableState, _ ) ->
            ( { model | contractTableState = tableState }, Cmd.none )

        ( SetPeerTableState tableState, _ ) ->
            ( { model | peerTableState = tableState }, Cmd.none )

        ( SetQuery queryString, _ ) ->
            ( { model | query = queryString }, Cmd.none )

        ( WindowResized size, _ ) ->
            ( { model | windowSize = size }, Cmd.none )

        ( InfiniteScroll scrollMsg, _ ) ->
            let
                ( infiniteScroll, cmd ) =
                    InfiniteScroll.update InfiniteScroll scrollMsg model.infiniteScroll
            in
                ( { model | infiniteScroll = infiniteScroll }, cmd )

        ( LoadMore dir, _ ) ->
            let
                lengthToRequest =
                    List.length model.chain.blockSummaries + 100

                cmd =
                    Request.Block.requestChainSummary2 model.nodeUrl lengthToRequest
                        |> Http.send (Result.map Request.ChainSummary >> RpcResponse)
            in
                ( model, cmd )


{-| Determine Page for given Route. (TODO: The distinction between Route and
Page is perhaps unnecessary. Can we combine the types somehow and eliminate
these kind of mappings?)
-}
toPage : Route -> Page
toPage route =
    case route of
        Route.Home ->
            Page.Home

        Route.Block hash ->
            Page.Block hash

        Route.Operations ->
            Page.Operations

        Route.Operation operationId ->
            Page.Operation operationId

        Route.Heads ->
            Page.Heads

        Route.ChainAt hash ->
            Page.ChainAt hash

        Route.Chain2 ->
            Page.Chain2

        Route.Contracts ->
            Page.Contracts

        Route.Accounts ->
            Page.Accounts

        Route.Account accountId ->
            Page.Account accountId

        Route.Keys ->
            Page.Keys

        Route.Peers ->
            Page.Peers

        Route.Contract contractId ->
            Page.Contract contractId

        Route.Schema ->
            Page.Schema

        Route.Errors ->
            Page.Errors

        Route.Debug ->
            Page.Debug

        Route.About ->
            Page.About


{-| Given a route, set the page state and make any requests needed to get data
for the page.
-}
setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute routeMaybe model =
    case routeMaybe of
        Nothing ->
            ( { model | pageState = Loaded Page.NotFound }, Cmd.none )

        Just Route.Home ->
            ( { model | pageState = Loaded Page.Home }
            , List.head model.chain.heads
                |> Maybe.map (Request.getBranch 24 model >> Cmd.map RpcResponse)
                |> Maybe.withDefault Cmd.none
            )

        Just (Route.Block hash) ->
            ( { model | pageState = Loaded (Page.Block hash) }
            , Cmd.batch
                [ getBlock model hash
                , Api.requestBlockOperations model.nodeUrl hash
                    |> Http.send (LoadBlockOperations hash)
                ]
            )

        Just Route.Heads ->
            ( { model | pageState = Loaded Page.Heads }
            , Request.Block.getHeads model.nodeUrl |> Http.send (Result.map Request.Heads >> RpcResponse)
            )

        Just (Route.ChainAt hash) ->
            ( { model | pageState = Loaded (Page.ChainAt hash) }
            , Request.getBranch 24 model hash |> Cmd.map RpcResponse
            )

        Just Route.Chain2 ->
            ( { model | pageState = Loaded Page.Chain2 }
            , Request.Block.requestChainSummary model.nodeUrl
                |> Http.send (Result.map Request.ChainSummary >> RpcResponse)
            )

        Just Route.Contracts ->
            ( { model
                | pageState = Loaded Page.Contracts
                , chain = Chain.loadingContractIDs model.chain
              }
            , getContractIDs model
            )

        Just Route.Accounts ->
            ( { model | pageState = Loaded Page.Accounts }
            , getAccounts model
            )

        Just (Route.Account accountId) ->
            ( { model | pageState = Loaded (Page.Account accountId) }
            , Request.Block.requestTransactions model.nodeUrl accountId
                |> Http.send (Result.map (Request.TransactionSummaries accountId) >> RpcResponse)
            )

        Just Route.Keys ->
            ( { model
                | pageState = Loaded Page.Keys
                , chain = Chain.loadingKeys model.chain
              }
            , getKeys model
            )

        Just Route.Peers ->
            ( { model
                | pageState = Loaded Page.Peers
                , chain = Chain.loadingPeers model.chain
              }
            , getPeers model
            )

        Just (Route.Contract contractId) ->
            ( { model
                | pageState = Loaded (Page.Contract contractId)
                , chain = Chain.loadingContract model.chain contractId
              }
            , Request.Block.getContract model.nodeUrl contractId |> Http.send (LoadContract contractId)
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

        Just route ->
            -- Handle those routes that require no command to get data
            ( { model | pageState = Loaded (toPage route) }, Cmd.none )


getBlock : Model -> BlockID -> Cmd Msg
getBlock model hash =
    case Dict.get hash model.chain.blocks of
        Nothing ->
            -- request block and some predecessors in anticipation of user following the chain
            Request.Block.getChainStartingAt model.nodeUrl 4 hash
                |> Http.send (Result.map Request.Blocks >> RpcResponse)

        _ ->
            Cmd.none


getContractIDs : Model -> Cmd Msg
getContractIDs model =
    Request.Block.getContractIDs model.nodeUrl |> Http.send LoadContractIDs


getContractDetails : URL -> List Chain.ContractID -> Cmd Msg
getContractDetails nodeUrl contractIDs =
    let
        get contractId =
            Request.Block.getContract nodeUrl contractId |> Http.send (LoadContract contractId)
    in
        List.map get contractIDs |> Cmd.batch


getAccounts : Model -> Cmd Msg
getAccounts model =
    Request.Block.requestAccounts model.nodeUrl
        |> Http.send (Result.map Request.AccountSummaries >> RpcResponse)


getKeys : Model -> Cmd Msg
getKeys model =
    Request.Block.getKeys model.nodeUrl |> Http.send LoadKeys


getPeers : Model -> Cmd Msg
getPeers model =
    Request.Block.getPeers model.nodeUrl |> Http.send LoadPeers


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
    in
        ( newModel, Cmd.none )


{-| delayedSend is like Http.send except that it also introduces a delay before
sending the request
-}
delayedSend : Time -> (Result Http.Error a -> msg) -> Http.Request a -> Cmd msg
delayedSend delay tagger request =
    Process.sleep delay
        |> Task.andThen (\() -> Http.toTask request)
        |> Task.attempt tagger
