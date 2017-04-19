module Main exposing (main)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Decode.Pipeline as Decode
import Dict exposing (Dict)
import List.Extra as List
import ParseInt
import Schema exposing (..)


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


type alias Fitness =
    String


type alias Timestamp =
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
    { source : Maybe SourceID
    , operations : List SubOperation
    }


type alias SchemaName =
    String


type alias Model =
    { blocks : List (List Block)
    , schemaData : Dict SchemaName SchemaData
    , errors : List Http.Error
    , nodeUrl : String
    , operations : Dict OperationID Operation
    , parsedOperations : Dict OperationID ParsedOperation
    , showBlock : Maybe BlockID
    , showOperation : Maybe OperationID
    , showBranch : Maybe Int
    }


type Msg
    = LoadBlocks (Result Http.Error BlocksData)
    | LoadSchema SchemaName (Result Http.Error SchemaData)
    | LoadOperation (Result Http.Error Operation)
    | LoadParsedOperation OperationID (Result Http.Error ParsedOperation)
    | SchemaMsg SchemaName Schema.Msg
    | ShowBlock BlockID
    | ShowOperation OperationID
    | ShowBranch Int


main =
    H.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


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


decodeParsedOperation : Decode.Decoder ParsedOperation
decodeParsedOperation =
    Decode.field "ok"
        (Decode.map2 ParsedOperation
            (Decode.maybe (Decode.field "source" Decode.string))
            (Decode.field "operations" (Decode.list decodeSubOperation))
        )


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


type alias Flags =
    { nodeUrl : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { blocks = []
            , schemaData = Dict.empty
            , errors = []
            , nodeUrl = flags.nodeUrl
            , operations = Dict.empty
            , parsedOperations = Dict.empty
            , showBlock = Nothing
            , showOperation = Nothing
            , showBranch = Nothing
            }

        schemaQuery1 =
            "/describe"

        schemaQuery2 =
            "/describe/blocks/head/proto"
    in
        ( model
        , Cmd.batch
            [ Http.send LoadBlocks (getBlocks model.nodeUrl)
            , Http.send (LoadSchema schemaQuery1) (getSchema model.nodeUrl schemaQuery1)
            , Http.send (LoadSchema schemaQuery2) (getSchema model.nodeUrl schemaQuery2)
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        LoadBlocks blocksMaybe ->
            case blocksMaybe of
                Ok blocks ->
                    ( { model | blocks = blocks }
                    , getBlocksOperationsDetail model blocks
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

        ShowBlock blockhash ->
            ( { model | showBlock = Just blockhash }
            , getBlockOperationInfo model blockhash
            )

        ShowOperation operationhash ->
            ( { model | showOperation = Just operationhash }
            , getOperationIfNew model.nodeUrl model.operations operationhash
              -- get details of operations, anticipating user request to view those details
            )

        ShowBranch index ->
            ( { model | showBranch = Just index }, Cmd.none )


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


view : Model -> Html Msg
view model =
    H.div []
        [ viewHeader model.nodeUrl
        , viewError model.nodeUrl model.errors
        , viewHeads model.blocks
        , viewShowBranch model.blocks model.showBranch
        , viewShowBlock model.blocks model.showBlock
        , viewShowOperation model.operations model.showOperation
        , viewParse model.parsedOperations model.showOperation
        , viewSchemas model.schemaData
        ]


viewSchemas : Dict SchemaName SchemaData -> Html Msg
viewSchemas schemas =
    let
        names =
            Dict.keys schemas

        viewSchema name =
            Dict.get name schemas
                |> Maybe.map (\data -> viewSchemaDataTop name data |> H.map (SchemaMsg name))
                |> Maybe.withDefault (H.text "failed to get schema from dict")
    in
        H.div [] (List.map viewSchema names)


viewHeader : String -> Html Msg
viewHeader nodeUrl =
    H.div []
        [ H.h1 [] [ H.text "Tezos client" ]
        , H.div [] [ H.text ("Connecting to Tezos RPC server " ++ nodeUrl) ]
        ]


canonFitness : List String -> List Int
canonFitness strings =
    List.map (ParseInt.parseIntHex >> Result.withDefault 0) strings
        |> List.dropWhile ((==) 0)


viewHeads : List (List Block) -> Html Msg
viewHeads branches =
    let
        header =
            H.tr []
                [ H.th [] [ H.text "index" ]
                , H.th [] [ H.text "hash" ]
                , H.th [] [ H.text "timestamp" ]
                , H.th [] [ H.text "fitness" ]
                , H.th [] [ H.text "level" ]
                ]

        viewBlockSummary : Int -> Block -> Html Msg
        viewBlockSummary i block =
            H.tr [ HA.class "head" ]
                [ H.td [] [ H.text (toString i) ]
                , H.td
                    [ HA.class "hash"
                    , HE.onClick (ShowBranch i)
                    , HA.title block.hash
                    ]
                    [ H.text (shortHash block.hash) ]
                , H.td [] [ H.text block.timestamp ]
                , H.td [] [ H.text (toString (canonFitness block.fitness)) ]
                , H.td [] [ H.text (toString block.level) ]
                ]

        viewHead : Int -> List Block -> Html Msg
        viewHead i blocks =
            case blocks of
                head :: _ ->
                    viewBlockSummary i head

                _ ->
                    H.text ""
    in
        H.div []
            [ H.h2 [] [ H.text "Blockchain heads" ]
            , H.table [ HA.class "heads" ]
                [ H.thead [] [ header ]
                , H.tbody [] (List.indexedMap viewHead branches)
                ]
            ]


findBranchByHead : List (List Block) -> BlockID -> Maybe (List Block)
findBranchByHead branches headid =
    let
        match branch =
            case branch of
                head :: tail ->
                    head.hash == headid

                _ ->
                    False
    in
        List.find match branches


viewShowBranch : List (List Block) -> Maybe Int -> Html Msg
viewShowBranch branches indexMaybe =
    let
        index =
            Maybe.withDefault 0 indexMaybe

        branchMaybe =
            List.getAt index branches
    in
        Maybe.map (viewBranch index) branchMaybe |> Maybe.withDefault (H.text "")


viewBranch n branch =
    let
        tableHeader =
            H.tr []
                [ H.th [] [ H.text "hash" ]
                , H.th [ HA.class "timestamp" ] [ H.text "timestamp" ]
                ]
    in
        H.div []
            [ H.h3 [] [ H.text ("branch " ++ toString n) ]
            , H.div [ HA.class "branch" ]
                [ H.table [ HA.class "blockchain" ]
                    [ H.thead [] [ tableHeader ]
                    , H.tbody [] (List.indexedMap viewBlock2 branch)
                    ]
                ]
            ]


viewBlock2 : Int -> Block -> Html Msg
viewBlock2 n block =
    H.tr [ HA.class "block" ]
        [ H.td
            [ HA.class "hash"
            , HA.title block.hash
            , HE.onClick (ShowBlock block.hash)
            ]
            [ H.text (shortHash block.hash) ]
        , H.td [ HA.class "timestamp" ] [ H.text block.timestamp ]
        ]


viewBlocks : List (List Block) -> Html Msg
viewBlocks branches =
    let
        header =
            [ H.h2 [] [ H.text "Block chains" ] ]
    in
        H.div [ HA.class "branches" ] (header ++ List.indexedMap viewBranch branches)


{-| View details of a single block.
-}
viewBlock : Block -> Html Msg
viewBlock block =
    let
        viewProperty : String -> Html Msg -> Html Msg
        viewProperty label value =
            H.div [ HA.class "property" ]
                [ H.div [ HA.class "label" ] [ H.text label ]
                , H.div [ HA.class label ] [ value ]
                ]

        viewPropertyString : String -> String -> Html Msg
        viewPropertyString label value =
            viewProperty label (H.text value)

        viewPropertyList : String -> List String -> Html Msg
        viewPropertyList label values =
            viewProperty label (List.intersperse ", " values |> String.concat |> H.text)

        viewOperations : String -> List OperationID -> Html Msg
        viewOperations label values =
            let
                li value =
                    H.li
                        [ HE.onClick (ShowOperation value)
                        , HA.class "operation hash"
                        , HA.title value
                        ]
                        [ H.text (shortHash value) ]
            in
                H.ol [] (List.map li values) |> viewProperty label

        viewOperationsList : String -> List (List OperationID) -> Html Msg
        viewOperationsList label outerList =
            H.div [] (List.map (viewOperations label) outerList)
    in
        H.div [ HA.class "block" ]
            [ H.h3 []
                [ H.text "Block "
                , H.span [ HA.class "hash" ] [ H.text (shortHash block.hash) ]
                ]
            , H.div [ HA.class "property-list" ]
                [ viewPropertyString "hash" block.hash
                , viewPropertyString "predecessor" block.predecessor
                , viewPropertyString "timestamp" block.timestamp
                , viewPropertyList "fitness" block.fitness
                , viewPropertyString "net_id" block.net_id
                , viewOperationsList "operations" block.operations
                ]
            ]


viewShowBlock : List (List Block) -> Maybe BlockID -> Html Msg
viewShowBlock blocks blockhashMaybe =
    case blockhashMaybe of
        Just blockhash ->
            case findBlock blocks blockhash of
                Just block ->
                    viewBlock block

                Nothing ->
                    H.div [] [ H.text ("Cannot find block " ++ blockhash) ]

        Nothing ->
            H.text ""


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


viewShowOperation : Dict OperationID Operation -> Maybe OperationID -> Html Msg
viewShowOperation operations operationidMaybe =
    operationidMaybe
        |> Maybe.andThen
            (\oId -> Dict.get oId operations)
        |> Maybe.map viewOperation
        |> Maybe.withDefault (H.text "")


viewOperations : RemoteData Http.Error (List Operation) -> Html Msg
viewOperations operationsStatus =
    H.div []
        [ H.h2 [] [ H.text "Operations" ]
        , case operationsStatus of
            Success operations ->
                H.div [ HA.class "property-list" ] (List.map viewOperation operations)

            Failure error ->
                H.div [] [ H.text ("Error: " ++ toString operationsStatus) ]

            _ ->
                H.div [] [ H.text (toString operationsStatus) ]
        ]


shortHash : Base58CheckEncodedSHA256 -> String
shortHash hash =
    String.left 12 hash


viewOperation : Operation -> Html Msg
viewOperation operation =
    let
        id =
            "operationdata-" ++ operation.hash
    in
        H.div []
            [ H.h3 []
                [ H.text "Operation "
                , H.span [ HA.class "hash" ] [ H.text (shortHash operation.hash) ]
                ]
            , H.div [ HA.class "property" ]
                [ H.div [ HA.class "label" ] [ H.text "net_id" ]
                , H.div [] [ H.text operation.netID ]
                ]
            , H.div [ HA.class "property" ]
                [ H.div [ HA.class "label" ] [ H.text "data" ]
                , H.div [ HA.class "operation-data" ] [ H.text operation.data ]
                ]
            ]


viewError : String -> List Http.Error -> Html Msg
viewError nodeUrl errors =
    case errors of
        [] ->
            H.text ""

        errors ->
            H.div [ HA.class "error" ]
                [ H.h1 [] [ H.text "Errors" ]
                , H.div [] (List.map (viewErrorInfo nodeUrl) errors)
                ]


viewErrorInfo nodeUrl error =
    case error of
        Http.BadPayload message response ->
            H.div []
                [ H.h4 [] [ H.text "Bad Payload (JSON parsing problem)" ]
                , H.div [ HA.style [ ( "white-space", "pre" ) ] ] [ H.text message ]
                ]

        Http.BadStatus response ->
            H.div []
                [ H.h4 [] [ H.text "Bad response status from node" ]
                , H.div [] [ H.text (toString response.status) ]
                , H.div [] [ H.text response.url ]
                  --, H.div [ HA.style [ ( "white-space", "pre" ) ] ] [ H.text (toString response) ]
                ]

        Http.NetworkError ->
            H.div []
                [ H.h4 [] [ H.text "Network Error" ]
                , H.div [] [ H.text ("Unable to access Tezos node at " ++ nodeUrl) ]
                ]

        _ ->
            H.text (toString error)


viewDebug : Model -> Html Msg
viewDebug model =
    H.div [ HA.class "debug" ]
        [ H.h2 [] [ H.text "Raw model" ]
        , H.text <| toString model
        ]


viewParse : Dict OperationID ParsedOperation -> Maybe OperationID -> Html Msg
viewParse parsedOperations operationIdMaybe =
    case operationIdMaybe of
        Just operationId ->
            let
                parse =
                    Dict.get operationId parsedOperations
                        -- |> Maybe.map (Encode.encode 2)
                        |>
                            Maybe.map toString
                        |> Maybe.withDefault "cannot get parse"
            in
                H.div []
                    [ H.h4 [] [ H.text "Parsed operation" ]
                    , H.div [] [ H.text parse ]
                    ]

        Nothing ->
            H.text ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
