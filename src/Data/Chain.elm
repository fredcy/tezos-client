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


type alias Key =
    { hash : Base58CheckEncodedSHA256
    , public_key : Base58CheckEncodedSHA256
    }


type alias Peer =
    { hash : Base58CheckEncodedSHA256
    , info : PeerInfo
    }


type alias PeerInfo =
    { value : Decode.Value
    , state : String
    , stat : PeerStats
    , trusted : Bool
    , score : Int
    , lastConnection : Maybe Connection
    }


type alias Connection =
    { addr : Address
    , time : Timestamp
    }

type alias Address =
    { addr: String
    , port_: Int
    }


type alias PeerStats =
    { total_sent : Int
    , total_recv : Int
    , current_inflow : Int
    , current_outflow : Int
    }


type alias Model =
    { heads : List BlockID
    , blocks : Dict BlockID Block
    , operations : Dict OperationID Operation
    , parsedOperations : Dict OperationID ParsedOperation
    , blockOperations : Dict BlockID (List OperationID)
    , contracts : RemoteData Http.Error Contracts
    , keys : RemoteData Http.Error (List Key)
    , peers : RemoteData Http.Error (List Peer)
    }


init : Model
init =
    { heads = []
    , blocks = Dict.empty
    , operations = Dict.empty
    , parsedOperations = Dict.empty
    , blockOperations = Dict.empty
    , contracts = RemoteData.NotAsked
    , keys = RemoteData.NotAsked
    , peers = RemoteData.NotAsked
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


loadingKeys : Model -> Model
loadingKeys model =
    { model | keys = RemoteData.Loading }


loadKeys : Model -> List Key -> Model
loadKeys model keys =
    { model | keys = RemoteData.Success keys }


loadKeysError : Model -> Http.Error -> Model
loadKeysError model error =
    { model | keys = RemoteData.Failure error }


loadingPeers : Model -> Model
loadingPeers model =
    { model | peers = RemoteData.Loading }


loadPeers : Model -> List Peer -> Model
loadPeers model peers =
    { model | peers = RemoteData.Success peers }


loadPeersError : Model -> Http.Error -> Model
loadPeersError model error =
    { model | peers = RemoteData.Failure error }



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


decodeKey : Decode.Decoder Key
decodeKey =
    Decode.succeed Key
        |> Decode.required "hash" Decode.string
        |> Decode.required "public_key" Decode.string


decodeKeys : Decode.Decoder (List Key)
decodeKeys =
    Decode.field "ok" (Decode.list decodeKey)


decodePeers : Decode.Decoder (List Peer)
decodePeers =
    Decode.list decodePeer


type Item
    = ItemString String
    | ItemValue PeerInfo


decodePeer : Decode.Decoder Peer
decodePeer =
    let
        decodeString =
            Decode.map ItemString Decode.string

        decodeValue =
            Decode.map ItemValue decodePeerInfo

        decodeItem =
            Decode.oneOf [ decodeString, decodeValue ]
    in
        Decode.list decodeItem
            |> Decode.andThen
                (\list ->
                    case list of
                        [ ItemString string, ItemValue value ] ->
                            Decode.succeed (Peer string value)

                        _ ->
                            Decode.fail "bad peer"
                )


decodePeerInfo : Decode.Decoder PeerInfo
decodePeerInfo =
    Decode.value
        |> Decode.andThen
            (\value ->
                Decode.succeed PeerInfo
                    |> Decode.hardcoded value
                    |> Decode.required "state" Decode.string
                    |> Decode.required "stat" decodePeerStats
                    |> Decode.required "trusted" Decode.bool
                    |> Decode.required "score" Decode.int
                    |> Decode.optional "last_established_connection" (Decode.map Just decodeConnection) Nothing
            )


decodePeerStats : Decode.Decoder PeerStats
decodePeerStats =
    Decode.succeed PeerStats
        |> Decode.required "total_sent" Decode.int
        |> Decode.required "total_recv" Decode.int
        |> Decode.required "current_inflow" Decode.int
        |> Decode.required "current_outflow" Decode.int


type AddressItem
    = AddressTime Timestamp
    | AddressAddr Address


decodeConnection : Decode.Decoder Connection
decodeConnection =
    let
        decodeAddrItem =
            Decode.oneOf
                [ Decode.map AddressTime decodeTimestamp
                , Decode.map AddressAddr decodeAddress
                ]
    in
        Decode.list decodeAddrItem
            |> Decode.andThen
                (\list ->
                    case list of
                        [ AddressAddr addr, AddressTime time ] ->
                            Decode.succeed (Connection addr time)

                        _ ->
                            Decode.fail "address decode failed"
                )


decodeAddress : Decode.Decoder Address
decodeAddress =
    Decode.succeed Address
        |> Decode.required "addr" Decode.string
        |> Decode.required "port" Decode.int
