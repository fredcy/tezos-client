module Page exposing (Page(..))

import Data.Chain exposing (BlockID, OperationID)


type Page
    = Blank
    | NotFound
    | Heads
    | Home
    | Block BlockID
    | Schema
    | Operations
    | Operation OperationID
    | Errors
    | Debug
