module Page exposing (Page(..))

import InfiniteScroll
import Data.Chain exposing (BlockID, ContractID, OperationID, AccountID)
import Msg exposing (Msg)


type Page
    = Blank
    | NotFound
    | Heads
    | Home (InfiniteScroll.Model Msg)
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
    | Chain2
    | Accounts
    | Account AccountID
    | Delegations
