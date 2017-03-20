module Main exposing (main)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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
    , operations : List OperationID
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


type alias Model =
    { blocks : List (List Block)
    , schemaData : Maybe SchemaData
    , errors : List Http.Error
    , nodeUrl : String
    , operations : RemoteData Http.Error (List Operation)
    , showBlock : Maybe BlockID
    , showBranch : Maybe BlockID
    }


type Msg
    = LoadBlocks (Result Http.Error BlocksData)
    | LoadSchema (Result Http.Error SchemaData)
    | LoadOperations (Result Http.Error (List Operation))
    | SchemaMsg Schema.Msg
    | ShowBlock BlockID


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
            100

        body =
            [ ( "operations", Encode.bool True )
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
    Decode.map5 Block
        (Decode.field "hash" Decode.string)
        (Decode.field "predecessor" Decode.string)
        (Decode.field "fitness" (Decode.list Decode.string))
        (Decode.field "timestamp" decodeTimestamp)
        (Decode.field "operations" (Decode.list Decode.string))


decodeTimestamp : Decode.Decoder Timestamp
decodeTimestamp =
    -- TODO
    Decode.string


getOperations : String -> Http.Request (List Operation)
getOperations nodeUrl =
    let
        body =
            [ ( "contents", Encode.bool True ) ] |> Encode.object |> Http.jsonBody
    in
        Http.post (nodeUrl ++ "/operations") body decodeOperations


decodeOperations : Decode.Decoder (List Operation)
decodeOperations =
    Decode.field "operations" (Decode.list decodeOperation)


decodeOperation : Decode.Decoder Operation
decodeOperation =
    Decode.map3 Operation
        (Decode.field "hash" Decode.string)
        (Decode.at [ "contents", "net_id" ] Decode.string)
        (Decode.at [ "contents", "data" ] Decode.string)


getSchema : String -> Http.Request SchemaData
getSchema nodeUrl =
    let
        body =
            [ ( "recursive", Encode.bool True ) ] |> Encode.object |> Http.jsonBody

        url =
            nodeUrl ++ "/describe"
    in
        Http.post url body decodeSchema


type alias Flags =
    { nodeUrl : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { blocks = []
            , schemaData = Nothing
            , errors = []
            , nodeUrl = flags.nodeUrl
            , operations = Loading
            , showBlock = Nothing
            , showBranch = Nothing
            }
    in
        ( model
        , Cmd.batch
            [ Http.send LoadBlocks (getBlocks model.nodeUrl)
              --, Http.send LoadSchema (getSchema model.nodeUrl)
            , Http.send LoadOperations (getOperations model.nodeUrl)
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        LoadBlocks blocksMaybe ->
            case blocksMaybe of
                Ok blocks ->
                    ( { model | blocks = blocks }, Cmd.none )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        LoadSchema schemaMaybe ->
            case schemaMaybe of
                Ok schemaData ->
                    ( { model | schemaData = Just schemaData }, Cmd.none )

                Err error ->
                    ( { model | errors = error :: model.errors }, Cmd.none )

        SchemaMsg msg ->
            let
                newSchema =
                    Schema.update msg model.schemaData
            in
                ( { model | schemaData = newSchema }, Cmd.none )

        LoadOperations operationsResult ->
            case operationsResult of
                Ok operationsData ->
                    ( { model | operations = Success operationsData }, Cmd.none )

                Err error ->
                    ( { model | operations = Failure error }, Cmd.none )

        ShowBlock blockhash ->
            ( { model | showBlock = Just blockhash }, Cmd.none )


view : Model -> Html Msg
view model =
    H.div []
        [ viewHeader model.nodeUrl
        , viewError model.nodeUrl model.errors
        , viewHeads model.blocks
        , viewShowBranch model.blocks model.showBranch
        , viewShowBlock model.blocks model.showBlock
        , viewOperations model.operations
        , case model.schemaData of
            Just schemaData ->
                viewSchemaDataTop schemaData |> H.map SchemaMsg

            Nothing ->
                H.text ""
          --, viewSchemaDataRaw model.schemaData |> H.map SchemaMsg
          --     , viewDebug model
        ]


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
                [ H.th [] [ H.text "hash" ]
                , H.th [] [ H.text "timestamp" ]
                , H.th [] [ H.text "fitness" ]
                ]

        viewBlockSummary : Block -> Html Msg
        viewBlockSummary block =
            H.tr [ HA.class "head" ]
                [ H.td [] [ H.text block.hash ]
                , H.td [] [ H.text block.timestamp ]
                , H.td [] [ H.text (toString (canonFitness block.fitness)) ]
                ]

        viewHead : List Block -> Html Msg
        viewHead blocks =
            case blocks of
                head :: _ ->
                    viewBlockSummary head

                _ ->
                    H.text ""
    in
        H.div []
            [ H.h2 [] [ H.text "Blockchain heads" ]
            , H.table [ HA.class "heads" ]
                [ H.thead [] [ header ]
                , H.tbody [] (List.map viewHead branches)
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


viewShowBranch : List (List Block) -> Maybe BlockID -> Html Msg
viewShowBranch branches headidMaybe =
    let
        branchMaybe =
            case headidMaybe of
                Just headid ->
                    findBranchByHead branches headid

                Nothing ->
                    case branches of
                        head :: _ ->
                            Just head

                        _ ->
                            Nothing
    in
        Maybe.map (viewBranch 99) branchMaybe |> Maybe.withDefault (H.text "")


viewBranch n branch =
    let
        tableHeader =
            H.tr []
                [ H.th [ HA.class "hash" ] [ H.text "hash" ]
                , H.th [] [ H.text "timestamp" ]
                , H.th [] [ H.text "operations" ]
                ]
    in
        H.div [ HA.class "branch" ]
            [ H.h3 [] [ H.text ("branch " ++ toString n) ]
            , H.table [ HA.class "blockchain" ]
                [ H.thead [] [ tableHeader ]
                , H.tbody [] (List.indexedMap viewBlock2 branch)
                ]
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
viewBlock : Int -> Block -> Html Msg
viewBlock n block =
    let
        viewProperty : String -> String -> Html Msg
        viewProperty label value =
            H.div [ HA.class "property" ]
                [ H.div [ HA.class "label" ] [ H.text label ]
                , H.div [] [ H.text value ]
                ]

        viewPropertyList : String -> List String -> Html Msg
        viewPropertyList label values =
            viewProperty label (List.intersperse ", " values |> String.concat)
    in
        H.div [ HA.class "block" ]
            [ H.h3 [] [ H.text "Block" ]
            , H.div [ HA.class "property-list" ]
                [ viewProperty "hash" block.hash
                , viewProperty "predecessor" block.predecessor
                , viewProperty "timestamp" block.timestamp
                , viewPropertyList "fitness" block.fitness
                , viewPropertyList "operations" block.operations
                ]
            ]


viewBlock2 : Int -> Block -> Html Msg
viewBlock2 n block =
    H.tr [ HA.class "block" ]
        [ H.td
            [ HA.class "hash"
            , HA.title "click to view block details"
            , HE.onClick (ShowBlock block.hash)
            ]
            [ H.text block.hash ]
        , H.td [] [ H.text block.timestamp ]
        , H.td [] [ H.text <| toString <| List.length block.operations ]
        ]


viewShowBlock : List (List Block) -> Maybe BlockID -> Html Msg
viewShowBlock blocks blockhashMaybe =
    case blockhashMaybe of
        Just blockhash ->
            case findBlock blocks blockhash of
                Just block ->
                    viewBlock 99 block

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


shortHash hash =
    String.left 8 hash


viewOperation : Operation -> Html Msg
viewOperation operation =
    let
        id =
            "operationdata-" ++ operation.hash
    in
        H.div []
            [ H.h4 [] [ H.text operation.hash ]
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
                [ H.h1 [] [ H.text "Error" ]
                , H.div [] (List.map (viewErrorInfo nodeUrl) errors)
                ]


viewErrorInfo nodeUrl error =
    case error of
        Http.BadPayload message response ->
            H.div []
                [ H.h2 [] [ H.text "Bad Payload (JSON parsing problem)" ]
                , H.div [ HA.style [ ( "white-space", "pre" ) ] ] [ H.text message ]
                ]

        Http.BadStatus response ->
            H.div []
                [ H.h2 [] [ H.text "Bad response status from node" ]
                , H.div [] [ H.text (toString response.status) ]
                , H.div [] [ H.text response.url ]
                  --, H.div [ HA.style [ ( "white-space", "pre" ) ] ] [ H.text (toString response) ]
                ]

        Http.NetworkError ->
            H.div []
                [ H.h2 [] [ H.text "Network Error" ]
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
