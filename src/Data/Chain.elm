module Data.Chain exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import RemoteData exposing (RemoteData)
import Set


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


type alias TransactionID =
    Base58CheckEncodedSHA256


type alias Fitness =
    String


type alias Timestamp =
    Date


type alias Block =
    { hash : BlockID
    , predecessor : BlockID
    , fitness : List Fitness
    , timestamp : Timestamp
    , operations : Maybe (List (List OperationID))
    , net_id : NetID
    , level : Level
    }


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
    | Transaction TransactionID Int
    | Faucet TransactionID Nonce
    | Delegation Base58CheckEncodedSHA256


type alias ParsedOperation =
    { hash : OperationID
    , net_id : NetID
    , operations : List SubOperation
    , source : Maybe SourceID
    , signature : Maybe Signature
    }


type alias BlockOperations =
    List (List ParsedOperation)


type alias BlocksData =
    List (List Block)


type alias Contracts =
    List Base58CheckEncodedSHA256


type alias Model =
    { heads : List BlockID
    , blocks : Dict BlockID Block
    , operations : Dict OperationID Operation
    , parsedOperations : Dict OperationID ParsedOperation
    , blockOperations : Dict BlockID (List OperationID)
    , contracts : RemoteData Http.Error Contracts
    }


init : Model
init =
    { heads = []
    , blocks = Dict.empty
    , operations = Dict.empty
    , parsedOperations = Dict.empty
    , blockOperations = Dict.empty
    , contracts = RemoteData.NotAsked
    }


blockNeedsOperations : Model -> BlockID -> Bool
blockNeedsOperations model blockHash =
    not (Dict.member blockHash model.blockOperations)


blocksNeedingOperations : Model -> List BlockID
blocksNeedingOperations model =
    let
        blockHashSet =
            Dict.toList model.blocks |> List.map Tuple.first |> Set.fromList

        blockOperHashSet =
            Dict.toList model.blockOperations |> List.map Tuple.first |> Set.fromList
    in
        Set.diff blockHashSet blockOperHashSet |> Set.toList


addBlockOperations : Model -> BlockID -> BlockOperations -> Model
addBlockOperations model blockhash operations =
    let
        blockOperations =
            Dict.insert blockhash (List.concat operations |> List.map .hash) model.blockOperations

        addOperation operation dict =
            Dict.insert operation.hash operation dict

        parsedOperations =
            List.foldr addOperation model.parsedOperations (List.concat operations)
    in
        { model | blockOperations = blockOperations, parsedOperations = parsedOperations }


loadBlocks : Model -> BlocksData -> Model
loadBlocks model blocksData =
    let
        newBlocks =
            List.foldl addChainBlocks model.blocks blocksData
    in
        { model | blocks = newBlocks }


loadHeads : Model -> BlocksData -> Model
loadHeads model headsData =
    let
        newModel =
            loadBlocks model headsData

        heads : List BlockID
        heads =
            List.map List.head headsData
                |> List.filterMap identity
                |> List.map .hash
    in
        { newModel | heads = heads }


updateMonitor : Model -> BlocksData -> Model
updateMonitor model headsData =
    let
        newModel =
            loadBlocks model headsData

        newHeadHash : Maybe BlockID
        newHeadHash =
            List.map List.head headsData
                |> List.filterMap identity
                |> List.head
                |> Maybe.map .hash

        newHeads =
            newHeadHash
                |> Maybe.map (\hash -> hash :: (List.tail model.heads |> Maybe.withDefault []))
                |> Maybe.withDefault model.heads
    in
        { newModel | heads = newHeads }


addBlock : Block -> Dict BlockID Block -> Dict BlockID Block
addBlock block blocks =
    Dict.insert block.hash block blocks


addChainBlocks : List Block -> Dict BlockID Block -> Dict BlockID Block
addChainBlocks chain blocks =
    List.foldl addBlock blocks chain


head : Model -> Maybe BlockID
head model =
    List.head model.heads


{-| Get list of saved blocks starting with the block with given hash and
following predecessor links. This only finds blocks already in the dict.
-}
getBranchList : Model -> BlockID -> List Block
getBranchList model blockhash =
    let
        helper hash blockList =
            let
                blockMaybe =
                    Dict.get hash model.blocks
            in
                case blockMaybe of
                    Just block ->
                        if block.predecessor == hash then
                            -- this should happen only for the genesis block
                            blockList
                        else
                            helper block.predecessor (block :: blockList)

                    Nothing ->
                        blockList
    in
        helper blockhash [] |> List.reverse


loadOperation : Model -> Operation -> Model
loadOperation model operation =
    let
        newOperations =
            Dict.insert operation.hash operation model.operations
    in
        { model | operations = newOperations }


loadParsedOperation : Model -> OperationID -> ParsedOperation -> Model
loadParsedOperation model operationId operation =
    let
        newParsed =
            Dict.insert operationId operation model.parsedOperations
    in
        { model | parsedOperations = newParsed }


loadContracts : Model -> Contracts -> Model
loadContracts model contracts =
    { model | contracts = RemoteData.Success contracts }


loadContractError : Model -> Http.Error -> Model
loadContractError model error =
    { model | contracts = RemoteData.Failure error }


loadingContracts : Model -> Model
loadingContracts model =
    { model | contracts = RemoteData.Loading }



-- Decoders


decodeBlocks : Decode.Decoder BlocksData
decodeBlocks =
    Decode.field "blocks" (Decode.list (Decode.list decodeBlock))


decodeBlock : Decode.Decoder Block
decodeBlock =
    Decode.succeed Block
        |> Decode.required "hash" Decode.string
        |> Decode.required "predecessor" Decode.string
        |> Decode.required "fitness" (Decode.list Decode.string)
        |> Decode.required "timestamp" decodeTimestamp
        |> Decode.optional "operations"
            (Decode.list (Decode.list Decode.string) |> Decode.map Just)
            Nothing
        |> Decode.required "net_id" Decode.string
        |> Decode.required "level" Decode.int


decodeTimestamp : Decode.Decoder Timestamp
decodeTimestamp =
    Decode.string
        |> Decode.map Date.fromString
        |> Decode.andThen
            (\dateResult ->
                case dateResult of
                    Ok date ->
                        Decode.succeed date

                    Err error ->
                        Decode.fail error
            )


decodeLevel : Decode.Decoder Int
decodeLevel =
    Decode.at [ "ok", "level" ] Decode.int


decodeContracts : Decode.Decoder Contracts
decodeContracts =
    Decode.field "ok" (Decode.list Decode.string)
