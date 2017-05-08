module Update exposing (..)

import Date
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Decode
import Http
import List.Extra as List
import Set
import Time exposing (Time)
import Model exposing (..)
import Schema exposing (SchemaData, decodeSchema, collapseTrees)


type alias HeadsResponse =
    List BlockID


type Msg
    = LoadBlocks (Result Http.Error BlocksData)
    | LoadSchema SchemaName (Result Http.Error SchemaData)
    | LoadOperation (Result Http.Error Operation)
    | LoadBlockOperations BlockID (Result Http.Error BlockOperations)
    | LoadParsedOperation OperationID (Result Http.Error ParsedOperation)
    | SchemaMsg SchemaName Schema.Msg
    | ShowBlock BlockID
    | ShowBranch BlockID
    | LoadHeads (Result Http.Error BlocksData)
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        LoadBlocks blocksMaybe ->
            case blocksMaybe of
                Ok blockChains ->
                    loadBlocks model blockChains

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        LoadSchema schemaName schemaMaybe ->
            case schemaMaybe of
                Ok schemaData ->
                    ( { model | schemaData = Dict.insert schemaName (collapseTrees schemaData) model.schemaData }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        SchemaMsg name msg ->
            let
                newSchemaMaybe : Maybe SchemaData
                newSchemaMaybe =
                    Dict.get name model.schemaData |> Maybe.map (Schema.update msg)
            in
                case newSchemaMaybe of
                    Just newSchema ->
                        ( { model | schemaData = Dict.insert name newSchema model.schemaData }, Cmd.none )

                    Nothing ->
                        let
                            _ =
                                Debug.log "Failed to find schema" name
                        in
                            ( model, Cmd.none )

        LoadOperation operationResult ->
            case operationResult of
                Ok operation ->
                    ( { model
                        | operations = Dict.insert operation.hash operation model.operations
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        LoadParsedOperation operationId parseResult ->
            case parseResult of
                Ok parse ->
                    ( { model
                        | parsedOperations =
                            Dict.insert operationId parse model.parsedOperations
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        LoadBlockOperations blockhash result ->
            case result of
                Ok operationListList ->
                    let
                        blockOperations =
                            Dict.insert blockhash (List.concat operationListList) model.blockOperations
                    in
                        ( { model | blockOperations = blockOperations }, Cmd.none )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        LoadHeads headsResult ->
            case headsResult of
                Ok heads ->
                    loadHeads model heads

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        ShowBlock blockhash ->
            ( { model | showBlock = Just blockhash }
            , getBlockOperationDetails model blockhash
            )

        ShowBranch hash ->
            ( { model | showBranch = Just hash }
            , getBranch model hash
            )

        Tick time ->
            ( { model | now = Date.fromTime time }, getHeads model.nodeUrl )


emptyJsonBody : Http.Body
emptyJsonBody =
    Encode.object [] |> Http.jsonBody


{-| Construct an RPC request for the blockchain header data.
-}
getBlocks : String -> Http.Request BlocksData
getBlocks nodeUrl =
    let
        maxBlocksToGet =
            1

        body =
            [ ( "include_ops", Encode.bool True )
            , ( "length", Encode.int maxBlocksToGet )
            ]
                |> Encode.object
                |> Http.jsonBody

        url =
            nodeUrl ++ "/blocks"
    in
        Http.post url body decodeBlocks


getHeads : String -> Cmd Msg
getHeads nodeUrl =
    let
        body =
            [] |> Encode.object |> Http.jsonBody

        url =
            nodeUrl ++ "/blocks"
    in
        Http.post url body decodeBlocks |> Http.send LoadHeads


addBlock : Block -> Dict BlockID Block -> Dict BlockID Block
addBlock block blocks =
    Dict.insert block.hash block blocks


addChainBlocks : List Block -> Dict BlockID Block -> Dict BlockID Block
addChainBlocks chain blocks =
    List.foldl addBlock blocks chain


loadHeads : Model -> BlocksData -> ( Model, Cmd Msg )
loadHeads model headsData =
    let
        heads : List BlockID
        heads =
            List.map List.head headsData
                |> List.filterMap identity
                |> List.map .hash

        blocks : Dict BlockID Block
        blocks =
            List.foldl addChainBlocks model.blocks headsData

        showBranch : Maybe BlockID
        showBranch =
            if model.showBranch == Nothing then
                -- default to displaying first branch
                List.head heads
            else
                model.showBranch
    in
        ( { model | heads = heads, blocks = blocks, showBranch = showBranch }
        , showBranch |> Maybe.map (getBranch model) |> Maybe.withDefault Cmd.none
        )


loadBlocks : Model -> BlocksData -> ( Model, Cmd Msg )
loadBlocks model blocksData =
    let
        blocks : Dict BlockID Block
        blocks =
            List.foldl addChainBlocks model.blocks blocksData
    in
        ( { model | blocks = blocks }
        , getAllBlocksOperations model
        )


{-| Request chain starting at given block (hash) if necessary. If we already have some blocks stored, request only what is needed to get to some target length.
-}
getBranch : Model -> BlockID -> Cmd Msg
getBranch model blockhash =
    let
        branchList =
            getBranchList model.blocks blockhash

        desiredLength =
            20

        toGet =
            desiredLength - List.length branchList
    in
        if toGet > 0 then
            let
                startHash =
                    List.reverse branchList
                        |> List.head
                        |> Maybe.map .predecessor
                        |> Maybe.withDefault blockhash
            in
                getChainStartingAt model.nodeUrl toGet startHash |> Http.send LoadBlocks
        else
            Cmd.none


getChainStartingAt : URL -> Int -> BlockID -> Http.Request BlocksData
getChainStartingAt nodeUrl length blockhash =
    let
        body =
            [ ( "include_ops", Encode.bool True )
            , ( "length", Encode.int length )
            , ( "heads", Encode.list [ Encode.string blockhash ] )
            ]
                |> Encode.object
                |> Http.jsonBody

        url =
            nodeUrl ++ "/blocks"
    in
        Http.post url body decodeBlocks


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
        |> Decode.optional "operations" (Decode.list (Decode.list Decode.string)) [ [] ]
        |> Decode.required "net_id" Decode.string
        |> Decode.required "level" Decode.int


decodeTimestamp : Decode.Decoder Timestamp
decodeTimestamp =
    Decode.string
        |> Decode.map Date.fromString
        |> Decode.map (Result.withDefault (Date.fromTime 0))


decodeHeads : Decode.Decoder HeadsResponse
decodeHeads =
    Decode.field "blocks" (Decode.list (Decode.list (Decode.field "hash" Decode.string)))
        |> Decode.andThen
            (\branches ->
                -- get first element of each sublist list (should be exactly one in each)
                List.map List.head branches |> List.filterMap identity |> Decode.succeed
            )


getOperation : String -> OperationID -> Http.Request Operation
getOperation nodeUrl operationId =
    let
        body =
            [] |> Encode.object |> Http.jsonBody

        decoder : Decode.Decoder Operation
        decoder =
            decodeOperationContents operationId
    in
        Http.post (nodeUrl ++ "/operations/" ++ operationId) body decoder


decodeOperations : Decode.Decoder (List Operation)
decodeOperations =
    Decode.field "operations" (Decode.list decodeOperation)


decodeOperation : Decode.Decoder Operation
decodeOperation =
    Decode.map3 Operation
        (Decode.field "hash" Decode.string)
        (Decode.at [ "contents", "net_id" ] Decode.string)
        (Decode.at [ "contents", "data" ] Decode.string)


{-| Decode RPC response with Operation data that does not include the hash of
the operation; instead the caller must pass that hash value.
-}
decodeOperationContents : OperationID -> Decode.Decoder Operation
decodeOperationContents operationId =
    (Decode.list
        (Decode.map3 Operation
            (Decode.succeed operationId)
            (Decode.field "net_id" Decode.string)
            (Decode.field "data" Decode.string)
        )
    )
        |> Decode.andThen
            (\opList ->
                List.head opList
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "bad operation list")
            )


getSchema : String -> String -> Http.Request SchemaData
getSchema nodeUrl schemaQuery =
    let
        body =
            [ ( "recursive", Encode.bool True ) ] |> Encode.object |> Http.jsonBody

        url =
            nodeUrl ++ schemaQuery
    in
        Http.post url body decodeSchema


getHeadId : List Block -> Maybe BlockID
getHeadId blocks =
    List.head blocks |> Maybe.map .hash


decodeLevel : Decode.Decoder Int
decodeLevel =
    Decode.at [ "ok", "level" ] Decode.int


{-| Form RPC command to parse the given operation.
-}
getParseOperationCommand : String -> Operation -> Cmd Msg
getParseOperationCommand nodeUrl operation =
    let
        url =
            nodeUrl ++ "/blocks/head/proto/helpers/parse/operation"

        body =
            [ ( "data", Encode.string operation.data )
            , ( "net_id", Encode.string operation.netID )
            ]
                |> Encode.object
                |> Http.jsonBody
    in
        Http.post url body decodeParsedOperation
            |> Http.send (LoadParsedOperation operation.hash)


decodeParsedOperationResponse : Decode.Decoder ParsedOperation
decodeParsedOperationResponse =
    Decode.field "ok" decodeParsedOperation


decodeParsedOperation : Decode.Decoder ParsedOperation
decodeParsedOperation =
    Decode.succeed ParsedOperation
        |> Decode.required "hash" Decode.string
        |> Decode.required "net_id" Decode.string
        |> Decode.required "source" Decode.string
        |> Decode.required "operations" (Decode.list decodeSubOperation)
        |> Decode.required "signature" Decode.string


decodeSubOperation : Decode.Decoder SubOperation
decodeSubOperation =
    Decode.oneOf
        [ decodeEndorsement
        , Decode.map Unknown Decode.value
        ]


decodeEndorsement : Decode.Decoder SubOperation
decodeEndorsement =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "endorsement" ->
                        (Decode.map2 Endorsement
                            (Decode.field "block" Decode.string)
                            (Decode.field "slot" Decode.int)
                        )

                    "seed_nonce_revelation" ->
                        Decode.map2 SeedNonceRevelation
                            (Decode.field "level" Decode.int)
                            (Decode.field "nonce" Decode.string)

                    _ ->
                        decodeDebug "bad kind" |> Decode.map Unknown
            )


{-| This decoder is useful for debugging. It is basically the same as just
`Decode.value` except that it has the side-effect of logging the decoded value
along with a message.
-}
decodeDebug : String -> Decode.Decoder Decode.Value
decodeDebug message =
    Decode.value
        |> Decode.andThen
            (\value ->
                let
                    _ =
                        Debug.log message value
                in
                    Decode.value
            )


getBlockOperationIDs : Block -> List OperationID
getBlockOperationIDs block =
    List.concatMap identity block.operations


getBlockOperationDetails : Model -> BlockID -> Cmd Msg
getBlockOperationDetails model blockHash =
    let
        url =
            model.nodeUrl ++ "/blocks/" ++ blockHash ++ "/proto/operations"
    in
        case Dict.get blockHash model.blockOperations of
            Nothing ->
                Http.post url emptyJsonBody decodeBlockOperationDetails
                    |> Http.send (LoadBlockOperations blockHash)

            _ ->
                Cmd.none


getAllBlocksOperations : Model -> Cmd Msg
getAllBlocksOperations model =
    let
        blockHashSet =
            Dict.toList model.blocks |> List.map Tuple.first |> Set.fromList

        blockOperHashSet =
            Dict.toList model.blockOperations |> List.map Tuple.first |> Set.fromList

        blocksToGet =
            Set.diff blockHashSet blockOperHashSet |> Set.toList |> Debug.log "blocksToGet"
    in
        Cmd.batch (List.map (getBlockOperationDetails model) blocksToGet)


decodeBlockOperationDetails : Decode.Decoder BlockOperations
decodeBlockOperationDetails =
    -- I don't understand why the RPC response data has two levels of lists. Anyway...
    Decode.field "ok" (Decode.list (Decode.list decodeParsedOperation))
