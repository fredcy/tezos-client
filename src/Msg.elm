module Msg exposing (Msg(..))

import Http
import InfiniteScroll
import Json.Decode as Decode
import Table
import Time exposing (Time)
import Window
import Api
import Data.Chain as Chain
import Data.Schema as Schema
import Request
import Route


type Msg
    = LoadSchema Schema.SchemaName (Result Http.Error Schema.SchemaData)
    | LoadParsedOperation Chain.OperationID (Result Http.Error Chain.ParsedOperation)
    | SchemaMsg Schema.SchemaName Schema.Msg
    | LoadContractIDs (Result Http.Error (List Chain.ContractID))
    | LoadKeys (Result Http.Error (List Chain.Key))
    | LoadPeers (Result Http.Error (List Chain.Peer))
    | LoadContract Chain.ContractID (Result Http.Error Chain.Contract)
    | LoadBlockOperations Chain.BlockID (Result Http.Error (List Api.OperationGroup))
    | Tick Time
    | SetRoute (Maybe Route.Route)
    | ClearErrors
    | Monitor String
    | Now Time
    | RpcResponse Request.Response
    | SetTableState Table.State
    | SetTransactionTableState Table.State
    | SetContractTableState Table.State
    | SetPeerTableState Table.State
    | SetQuery String
    | WindowResized Window.Size
    | InfiniteScroll InfiniteScroll.Msg
    | LoadMore InfiniteScroll.Direction
