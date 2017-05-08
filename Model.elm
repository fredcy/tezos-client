module Model exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
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
    Date


type alias URL =
    String


type alias Block =
    { hash : BlockID
    , predecessor : BlockID
    , fitness : List Fitness
    , timestamp : Timestamp
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
    { heads : List BlockID
    , blocks : Dict BlockID Block
    , schemaData : Dict SchemaName Schema.SchemaData
    , errors : List Http.Error
    , nodeUrl : String
    , operations : Dict OperationID Operation
    , parsedOperations : Dict OperationID ParsedOperation
    , blockOperations : Dict BlockID (List ParsedOperation)
    , showBlock : Maybe BlockID
    , showOperation : Maybe OperationID
    , showBranch : Maybe BlockID
    , now : Date
    }


type alias BlockOperations =
    List (List ParsedOperation)


{-| Get list of saved blocks starting with the block with given hash and
following predecessor links. This only finds blocks already in the dict.
-}
getBranchList : Dict BlockID Block -> BlockID -> List Block
getBranchList blocks blockhash =
    let
        helper hash blockList =
            Dict.get hash blocks
                |> Maybe.map (\block -> helper block.predecessor (block :: blockList))
                |> Maybe.withDefault blockList
    in
        helper blockhash [] |> List.reverse
