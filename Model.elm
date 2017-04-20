module Model exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import List.Extra as List
import Schema


type alias Base58CheckEncodedSHA256 =
    String


type alias BlockID =
    Base58CheckEncodedSHA256


type alias OperationID =
    Base58CheckEncodedSHA256


type alias NetID =
    Base58CheckEncodedSHA256


type alias SourceID =
    Base58CheckEncodedSHA256


type alias Signature =
    Base58CheckEncodedSHA256


type alias Fitness =
    String


type alias Timestamp =
    String


type alias URL =
    String


type alias Block =
    { hash : BlockID
    , predecessor : BlockID
    , fitness : List Fitness
    , timestamp :
        String
        -- TODO convert to date value
    , operations : List (List OperationID)
    , net_id : NetID
    , level : Level
    }


type alias BlocksData =
    List (List Block)


type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a


type alias Operation =
    { hash : OperationID
    , netID : NetID
    , data : String
    }


type alias Level =
    Int


type alias Nonce =
    String


type SubOperation
    = Unknown Decode.Value
    | Endorsement BlockID Int
    | SeedNonceRevelation Level Nonce


type alias ParsedOperation =
    { hash : OperationID
    , net_id : NetID
    , source : SourceID
    , operations : List SubOperation
    , signature : Signature
    }


type alias SchemaName =
    String


type alias Model =
    { blocks : List (List Block)
    , schemaData : Dict SchemaName Schema.SchemaData
    , errors : List Http.Error
    , nodeUrl : String
    , operations : Dict OperationID Operation
    , parsedOperations : Dict OperationID ParsedOperation
    , blockOperations : Dict BlockID (List ParsedOperation)
    , showBlock : Maybe BlockID
    , showOperation : Maybe OperationID
    , showBranch : Maybe Int
    }


type alias BlockOperations =
    List (List ParsedOperation)


findInChain : List Block -> BlockID -> Maybe Block
findInChain blocks hash =
    List.find (\block -> block.hash == hash) blocks


findBlock : List (List Block) -> BlockID -> Maybe Block
findBlock blockchains hash =
    case blockchains of
        [] ->
            Nothing

        hd :: tl ->
            case findInChain hd hash of
                Just block ->
                    Just block

                Nothing ->
                    findBlock tl hash


findOperation : List Operation -> OperationID -> Maybe Operation
findOperation operations operationId =
    List.find (\operation -> operation.hash == operationId) operations
