module Page exposing (Page(..))

import Data.Chain exposing (BlockID, ContractID, OperationID)


type Page
    = Blank
    | NotFound
    | Heads
    | Home
    | Block BlockID
    | Schema
    | Operations
    | Operation OperationID
    | ChainAt BlockID
    | Contracts
    | Keys
    | Peers
    | Contract ContractID
    | Errors
    | Debug
    | About
