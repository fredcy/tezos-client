module Page exposing (Page(..))

import Data.Chain exposing (BlockID)


type Page
    = Blank
    | NotFound
    | Heads
    | Home
    | Block BlockID
    | Schema
    | Operations
    | Errors
    | Debug
