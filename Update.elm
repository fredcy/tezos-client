module Update exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Decode
import Http
import Model exposing (..)
import Schema exposing (SchemaData, decodeSchema, collapseTrees)


type Msg
    = LoadBlocks (Result Http.Error BlocksData)
    | LoadSchema SchemaName (Result Http.Error SchemaData)
    | LoadOperation (Result Http.Error Operation)
    | LoadBlockOperations BlockID (Result Http.Error BlockOperations)
    | LoadParsedOperation OperationID (Result Http.Error ParsedOperation)
    | SchemaMsg SchemaName Schema.Msg
    | ShowBlock BlockID
    | ShowOperation OperationID
    | ShowBranch Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        LoadBlocks blocksMaybe ->
            case blocksMaybe of
                Ok blocks ->
                    ( { model | blocks = blocks }
                      --, getBlocksOperationsDetail model blocks
                    , Cmd.none
                    )

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
            ( model, Cmd.none )

        ShowBlock blockhash ->
            ( { model | showBlock = Just blockhash }
            , getBlockOperationDetails model.nodeUrl blockhash
            )

        ShowOperation operationhash ->
            ( { model | showOperation = Just operationhash }
            , getOperationIfNew model.nodeUrl model.operations operationhash
              -- get details of operations, anticipating user request to view those details
            )

        ShowBranch index ->
            ( { model | showBranch = Just index }, Cmd.none )


emptyJsonBody : Http.Body
emptyJsonBody =
    Encode.object [] |> Http.jsonBody


{-| Construct an RPC request for the blockchain header data.
-}
getBlocks : String -> Http.Request BlocksData
getBlocks nodeUrl =
    let
        maxBlocksToGet =
            20

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
        |> Decode.required "operations" (Decode.list (Decode.list Decode.string))
        |> Decode.required "net_id" Decode.string
        |> Decode.required "level" Decode.int


decodeTimestamp : Decode.Decoder Timestamp
decodeTimestamp =
    -- TODO
    Decode.string


getOperations : String -> BlockID -> Http.Request (List Operation)
getOperations nodeUrl blockId =
    let
        body =
            [] |> Encode.object |> Http.jsonBody

        query =
            "/blocks/" ++ blockId ++ "/proto/operations"
    in
        Http.post (nodeUrl ++ query) body decodeOperations


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


getBlockOperationInfo : Model -> BlockID -> Cmd Msg
getBlockOperationInfo model blockhash =
    findBlock model.blocks blockhash
        |> Maybe.map getBlockOperationIDs
        |> Maybe.map (List.map (getOperationIfNew model.nodeUrl model.operations))
        |> Maybe.map Cmd.batch
        |> Maybe.withDefault Cmd.none


getBlockOperationDetails : URL -> BlockID -> Cmd Msg
getBlockOperationDetails nodeUrl blockHash =
    let
        url =
            nodeUrl ++ "/blocks/" ++ blockHash ++ "/proto/operations"
    in
        Http.post url emptyJsonBody decodeBlockOperationDetails
            |> Http.send (LoadBlockOperations blockHash)


decodeBlockOperationDetails : Decode.Decoder BlockOperations
decodeBlockOperationDetails =
    -- I don't understand why the RPC response data has two levels of lists. Anyway...
    Decode.field "ok" (Decode.list (Decode.list decodeParsedOperation))


{-| Form command to get details of all operations in the (potentially
multi-headed) blockchain.
-}
getBlocksOperationsDetail : Model -> List (List Block) -> Cmd Msg
getBlocksOperationsDetail model blockChains =
    let
        doChain : List Block -> Cmd Msg
        doChain blocks =
            blocks
                |> List.map (getBlockOperationIDs >> List.map (getOperationIfNew model.nodeUrl model.operations) >> Cmd.batch)
                |> Cmd.batch
    in
        Cmd.batch (List.map doChain blockChains)


getOperationIfNew : String -> Dict OperationID Operation -> OperationID -> Cmd Msg
getOperationIfNew nodeUrl operations operationId =
    if Dict.member operationId operations then
        Cmd.none
    else
        Http.send LoadOperation (getOperation nodeUrl operationId)
