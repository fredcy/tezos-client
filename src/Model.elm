module Model exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Data.Schema as Schema
import Data.Chain exposing (Block, BlockID, OperationID, ParsedOperation)


type alias SchemaName =
    String


type alias Model =
    { schemaData : Dict SchemaName Schema.SchemaData
    , errors : List Http.Error
    , nodeUrl : String
    , showBlock : Maybe BlockID
    , showOperation : Maybe OperationID
    , showBranch : Maybe BlockID
    , now : Date
    , chain : Data.Chain.Model
    }

