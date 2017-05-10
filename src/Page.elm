module Page exposing (Page(..))

import Data.Chain exposing (BlockID)


type Page
    = Blank
    | NotFound
    | Home
    | Block BlockID
    | Schema
    | Operations
    | Debug
