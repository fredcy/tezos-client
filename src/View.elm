module View exposing (view)

import Date exposing (Date)
import Date.Distance
import Date.Extra.Format
import Date.Extra.Config.Config_en_us
import FormatNumber
import FormatNumber.Locales
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Dict exposing (Dict)
import List.Extra as List
import ParseInt
import RemoteData exposing (RemoteData)
import Data.Chain as Chain exposing (BlockID, Block, OperationID, ParsedOperation, Base58CheckEncodedSHA256, SubOperation(..), getBranchList)
import Data.Schema as Schema
import Model exposing (..)
import Page
import Update exposing (Msg(..))
import View.Page


view : Model -> Html Msg
view model =
    let
        context =
            { pageState = model.pageState
            , now = model.now
            , errorCount = List.length model.errors
            }

        content =
            case model.pageState of
                Loaded Page.Home ->
                    viewHome model

                Loaded Page.Blank ->
                    H.text ""

                Loaded Page.Operations ->
                    viewAllOperations model

                Loaded (Page.Operation operationId) ->
                    viewOperation model operationId

                Loaded Page.Schema ->
                    viewSchemas model.schemaData

                Loaded Page.Debug ->
                    viewDebug model

                Loaded Page.Heads ->
                    viewHeads model

                Loaded (Page.Block hash) ->
                    case Dict.get hash model.chain.blocks of
                        Just block ->
                            viewBlock model block

                        Nothing ->
                            H.text "loading block ..."

                Loaded (Page.ChainAt hash) ->
                    viewChainAt model hash

                Loaded Page.Contracts ->
                    viewContracts model

                Loaded Page.Keys ->
                    viewKeys model.chain.keys

                Loaded Page.Peers ->
                    viewPeers model.chain.peers

                Loaded (Page.Contract contractId) ->
                    viewContract contractId model.chain.contract

                Loaded Page.Errors ->
                    viewError model.nodeUrl model.errors

                Loaded Page.NotFound ->
                    H.text "page not found"
    in
        View.Page.frame context content


viewHome : Model -> Html Msg
viewHome model =
    let
        headMaybe =
            List.head model.chain.heads
    in
        case headMaybe of
            Just head ->
                H.div []
                    [ H.h2 [] [ H.text "Newest blocks" ]

                    -- TODO: fix this mess
                    , getBranchList model.chain head
                        |> viewBranch 24 model (Just head)
                    ]

            Nothing ->
                H.text "no head found"


viewSchemas : Dict Schema.SchemaName Schema.SchemaData -> Html Msg
viewSchemas schemas =
    let
        names =
            Dict.keys schemas

        viewSchema name =
            Dict.get name schemas
                |> Maybe.map (\data -> Schema.viewSchemaDataTop name data |> H.map (SchemaMsg name))
                |> Maybe.withDefault (H.text "failed to get schema from dict")
    in
        H.div [] (List.map viewSchema names)


viewHeader : String -> Html Msg
viewHeader nodeUrl =
    H.div []
        [ H.h1 [] [ H.text "Tezos Explorer" ]
        , H.div [] [ H.text ("Connecting to Tezos RPC server " ++ nodeUrl) ]
        ]


canonFitness : Chain.Fitness -> Chain.Fitness
canonFitness fitness =
    List.dropWhile ((==) 0) fitness


type BlockStatus
    = BlockFound Block
    | BlockNotFound BlockID


findBlockStatus : Dict BlockID Block -> BlockID -> BlockStatus
findBlockStatus blocks blockhash =
    case Dict.get blockhash blocks of
        Just block ->
            BlockFound block

        Nothing ->
            BlockNotFound blockhash


viewHeads : Model -> Html Msg
viewHeads model =
    let
        heads : List BlockStatus
        heads =
            List.map (findBlockStatus model.chain.blocks) model.chain.heads

        header =
            H.tr []
                [ H.th [ HA.class "index" ] [ H.text "index" ]
                , H.th [ HA.class "hash" ] [ H.text "hash" ]
                , H.th [ HA.class "timestamp" ] [ H.text "timestamp" ]
                , H.th [ HA.class "age" ] [ H.text "age" ]
                , H.th [ HA.class "fitness" ] [ H.text "fitness" ]
                , H.th [ HA.class "level" ] [ H.text "level" ]
                ]

        viewBlockSummary : Int -> Block -> Html Msg
        viewBlockSummary i block =
            H.tr []
                [ H.td [ HA.class "index" ] [ H.text (toString i) ]
                , H.td
                    [ HA.class "hash"
                    , HE.onClick (ShowBranch block.hash)
                    , HA.title block.hash
                    ]
                    [ H.text (shortHash block.hash) ]
                , H.td [ HA.class "timestamp" ] [ H.text (formatDate block.timestamp) ]
                , H.td [ HA.class "age" ] [ H.text (Date.Distance.inWords model.now block.timestamp) ]
                , H.td [ HA.class "fitness" ] [ H.text (toString (canonFitness block.fitness)) ]
                , H.td [ HA.class "level" ] [ H.text (toString block.level) ]
                ]

        viewHead : Int -> BlockStatus -> Html Msg
        viewHead i blockStatus =
            case blockStatus of
                BlockFound block ->
                    viewBlockSummary i block

                BlockNotFound hash ->
                    H.text ("block " ++ hash ++ " not found")
    in
        H.div []
            [ H.h2 [] [ H.text "Blockchain heads" ]
            , H.div [ HA.class "heads" ]
                [ H.table [ HA.class "heads" ]
                    [ H.thead [] [ header ]
                    , H.tbody [] (List.indexedMap viewHead heads)
                    ]
                ]
            ]


viewBranch : Int -> Model -> Maybe BlockID -> List Block -> Html Msg
viewBranch howMany model blockhashMaybe branch =
    let
        tableHeader =
            H.tr []
                [ H.th [] [ H.text "level" ]
                , H.th [] [ H.text "hash" ]
                , H.th [] [ H.text "timestamp" ]
                , H.th [ HA.class "timestamp" ] [ H.text "age" ]
                , H.th [] [ H.text "operations" ]
                ]

        branchToShow =
            List.take howMany branch
    in
        H.div []
            [ H.div [ HA.class "branch" ]
                [ H.table [ HA.class "blockchain" ]
                    [ H.thead [] [ tableHeader ]
                    , H.tbody [] (List.indexedMap (viewBlock2 model blockhashMaybe) branchToShow)
                    ]
                ]
            ]


blockOperationCount : Model -> Block -> String
blockOperationCount model block =
    let
        blockOperationsMaybe =
            Dict.get block.hash model.chain.blockOperations
    in
        blockOperationsMaybe
            |> Maybe.map (List.length >> toString)
            |> Maybe.withDefault ""


viewBlock2 : Model -> Maybe BlockID -> Int -> Block -> Html Msg
viewBlock2 model blockhashMaybe n block =
    H.tr
        [ HA.classList
            [ ( "block", True ) ]
        ]
        [ H.td [] [ H.text (toString block.level) ]
        , H.td
            [ HA.class "hash"
            , HA.title block.hash
            , HE.onClick (ShowBlock block.hash)
            ]
            [ H.text (shortHash block.hash) ]
        , H.td [ HA.class "timestamp" ]
            [ H.text (formatDate block.timestamp) ]
        , H.td [ HA.class "age" ]
            [ H.text (Date.Distance.inWords model.now block.timestamp) ]
        , H.td [ HA.class "operation-count" ]
            [ H.text (blockOperationCount model block) ]
        ]


blockFullLink : BlockID -> Html Msg
blockFullLink hash =
    H.span [ HA.class "hash link", HE.onClick (ShowBlock hash) ] [ H.text hash ]


viewProperty : String -> Html Msg -> Html Msg
viewProperty label value =
    H.div [ HA.class "property" ]
        [ H.div [ HA.class "label" ] [ H.text label ]
        , H.div [ HA.class label ] [ value ]
        ]


viewPropertyMaybe : String -> Maybe String -> Html Msg
viewPropertyMaybe label valueMaybe =
    case valueMaybe of
        Just value ->
            viewProperty label (H.text value)

        Nothing ->
            H.text ""


{-| View details of a single block.
-}
viewBlock : Model -> Block -> Html Msg
viewBlock model block =
    let
        viewPropertyString : String -> String -> Html Msg
        viewPropertyString label value =
            viewProperty label (H.text value)

        viewPropertyList : String -> List String -> Html Msg
        viewPropertyList label values =
            viewProperty label (List.intersperse ", " values |> String.concat |> H.text)

        viewOperations : Block -> Html Msg
        viewOperations block =
            case block.operations of
                Just operations ->
                    viewPropertyList "operations" (List.concat operations |> List.map shortHash)

                Nothing ->
                    viewPropertyString "operations" "[unknown]"
    in
        H.div [ HA.class "block" ]
            [ H.h3 []
                [ H.text "Block "
                , H.span [ HA.class "hash" ] [ H.text (shortHash block.hash) ]
                ]
            , H.div [ HA.class "property-list" ]
                [ viewPropertyString "hash" block.hash
                , viewProperty "predecessor" (blockFullLink block.predecessor)
                , viewPropertyString "timestamp" (formatDate block.timestamp)
                , viewPropertyString "level" (toString block.level)
                , viewPropertyList "fitness" (List.map toString block.fitness)
                , viewPropertyString "net_id" block.net_id
                ]
            , H.h4 [] [ H.text "Operations" ]
            , viewParsedOperations model block.hash
            ]


viewParsedOperations : Model -> BlockID -> Html Msg
viewParsedOperations model blockId =
    let
        operationIDs : List OperationID
        operationIDs =
            Dict.get blockId model.chain.blockOperations
                |> Maybe.withDefault []

        operations : List ParsedOperation
        operations =
            operationIDs
                |> List.map (\oid -> Dict.get oid model.chain.parsedOperations)
                |> List.filterMap identity
    in
        viewOperationsTable operations


formatDate : Date -> String
formatDate date =
    Date.Extra.Format.formatUtc Date.Extra.Config.Config_en_us.config "%Y-%m-%dT%H:%M:%SZ" date


viewShowBlockOperations : Dict BlockID (List ParsedOperation) -> Maybe BlockID -> Html Msg
viewShowBlockOperations blockOperations hashMaybe =
    case hashMaybe of
        Just hash ->
            Dict.get hash blockOperations |> Maybe.map viewOperations |> Maybe.withDefault (H.text "")

        Nothing ->
            H.text ""


viewOperation : Model -> OperationID -> Html Msg
viewOperation model operationId =
    let
        operationMaybe =
            Dict.get operationId model.chain.parsedOperations

        viewOperationFields : ParsedOperation -> Html Msg
        viewOperationFields operation =
            H.div []
                [ viewProperty "hash" (H.text operation.hash)
                , viewProperty "net_id" (H.text operation.net_id)
                , viewPropertyMaybe "source" operation.source
                , viewPropertyMaybe "signature" operation.signature
                , H.h4 [] [ H.text "operations" ]
                , H.ul [] (List.map (\so -> H.li [] [ viewSuboperation so ]) operation.operations)
                ]
    in
        H.div []
            [ H.h3 [] [ H.text ("Operation " ++ operationId) ]
            , case operationMaybe of
                Just operation ->
                    viewOperationFields operation

                Nothing ->
                    H.text "operation not found"
            ]


{-| Format Int value with a thousands separator.
-}
formatInt : Int -> String
formatInt number =
    let
        usLocale =
            FormatNumber.Locales.usLocale
    in
        toFloat number
            |> FormatNumber.format { usLocale | decimals = 0 }


viewSuboperation : SubOperation -> Html Msg
viewSuboperation suboperation =
    case suboperation of
        Endorsement blockid int ->
            H.span []
                [ H.text "Endorsement of "
                , H.span [ HA.class "hash" ] [ H.text (shortHash blockid) ]
                , H.text (", slot " ++ (toString int))
                ]

        Transaction address amount ->
            H.span []
                [ H.text "Transaction of "
                , H.text (formatInt amount)
                , H.text " to "
                , H.span [ HA.title address, HA.class "hash" ] [ H.text (shortHash address) ]
                ]

        _ ->
            H.text (toString suboperation)


viewOperations : List ParsedOperation -> Html Msg
viewOperations operations =
    H.div []
        [ H.h3 [] [ H.text "Operations" ]
        , viewOperationsTable operations
        ]


viewOperationsTable : List ParsedOperation -> Html Msg
viewOperationsTable operations =
    let
        tableHead =
            H.thead []
                [ H.tr []
                    [ H.th [] [ H.text "hash" ]
                    , H.th [] [ H.text "source" ]
                    , H.th [] [ H.text "sub-operations" ]
                    ]
                ]

        viewSourceMaybe : Maybe Chain.SourceID -> String
        viewSourceMaybe sourceMaybe =
            Maybe.map shortHash sourceMaybe |> Maybe.withDefault "[no source]"

        sourceTitle sourceMaybe =
            Maybe.map identity sourceMaybe |> Maybe.withDefault ""

        tableRow operation =
            H.tr []
                [ H.td [ HA.class "hash link", HA.title operation.hash, HE.onClick (ShowOperation operation.hash) ]
                    [ H.text (shortHash operation.hash) ]
                , H.td [ HA.class "hash", HA.title (sourceTitle operation.source) ] [ H.text (viewSourceMaybe operation.source) ]
                , H.td [] [ H.ul [] (List.map (\so -> H.li [] [ viewSuboperation so ]) operation.operations) ]
                ]
    in
        H.table [ HA.class "operations" ]
            [ tableHead
            , H.tbody [] (List.map tableRow operations)
            ]


viewAllOperations : Model -> Html Msg
viewAllOperations model =
    H.div []
        [ H.h3 [] [ H.text "All Operations" ]
        , Dict.toList model.chain.parsedOperations
            |> List.map Tuple.second
            |> List.sortBy .hash
            |> viewOperationsTable
        ]


shortHash : Base58CheckEncodedSHA256 -> String
shortHash hash =
    String.left 14 hash


viewChainAt : Model -> BlockID -> Html Msg
viewChainAt model hash =
    H.div []
        [ H.h3 [] [ H.text ("Chain at " ++ (shortHash hash)) ]
        , getBranchList model.chain hash
            |> viewBranch 24 model (Just hash)
        ]


viewContracts : Model -> Html Msg
viewContracts model =
    H.div []
        [ H.h3 [] [ H.text "Contracts" ]
        , case model.chain.contracts of
            RemoteData.Success contracts ->
                viewContractList contracts

            RemoteData.Loading ->
                H.text "loading ..."

            _ ->
                H.text (toString model.chain.contracts)
        ]


viewContractList : Chain.Contracts -> Html Msg
viewContractList contracts =
    let
        viewContract contract =
            H.li []
                [ H.span
                    [ HA.class "hash link"
                    , HE.onClick (ShowContract contract)
                    ]
                    [ H.text contract ]
                ]
    in
        H.ul [] (List.map viewContract (List.sort contracts))


viewKeys : RemoteData Http.Error (List Chain.Key) -> Html Msg
viewKeys keysData =
    H.div []
        [ H.h3 [] [ H.text "Keys " ]
        , case keysData of
            RemoteData.Success keys ->
                viewKeyList keys

            RemoteData.Loading ->
                H.text "loading ..."

            _ ->
                H.text (toString keysData)
        ]


viewKeyList : List Chain.Key -> Html Msg
viewKeyList keys =
    let
        tableHead =
            H.thead []
                [ H.tr []
                    [ H.th [] [ H.text "hash" ]
                    , H.th [] [ H.text "public_key" ]
                    ]
                ]

        tableBody keys =
            H.tbody [] (List.map tableRow keys)

        tableRow key =
            H.tr []
                [ H.td [] [ H.span [ HA.class "hash" ] [ H.text key.hash ] ]
                , H.td [] [ H.span [ HA.class "hash" ] [ H.text key.public_key ] ]
                ]
    in
        H.table [ HA.class "keys" ] [ tableHead, tableBody keys ]


viewPeers : RemoteData Http.Error (List Chain.Peer) -> Html Msg
viewPeers peersData =
    H.div []
        [ H.h3 [] [ H.text "Peers" ]
        , case peersData of
            RemoteData.Success peers ->
                viewPeersList peers

            _ ->
                H.text (toString peersData)
        ]


simplifyHost : String -> String
simplifyHost host =
    let
        prefix =
            "::ffff:"
    in
        if String.startsWith prefix host then
            String.dropLeft (String.length prefix) host
        else
            host


viewAddress : Chain.Address -> Html Msg
viewAddress address =
    H.span []
        [ H.text (simplifyHost address.addr)
        , H.text ":"
        , H.text (toString address.port_)
        ]


viewConnection : Maybe Chain.Connection -> Html Msg
viewConnection connectionM =
    case connectionM of
        Just connection ->
            H.div [ HA.class "connection" ]
                [ viewAddress connection.addr
                , H.text " "
                , H.span [ HA.class "timestamp" ] [ H.text (formatDate connection.time) ]
                ]

        Nothing ->
            H.text ""


viewPeersList : List Chain.Peer -> Html Msg
viewPeersList peers =
    let
        row peer =
            H.tr []
                [ H.td [ HA.class "hash" ] [ H.text (shortHash peer.hash) ]
                , H.td [] [ H.text peer.info.state ]
                , H.td [] [ H.text (toString peer.info.trusted) ]
                , H.td [] [ H.text (toString peer.info.score) ]
                , H.td [ HA.class "number" ] [ H.text (toString peer.info.stat.total_sent) ]
                , H.td [ HA.class "number" ] [ H.text (toString peer.info.stat.total_recv) ]
                , H.td [] [ viewConnection peer.info.lastConnection ]

                --, H.td [] [ H.text (toString peer.info.value) ]
                ]

        colHeader label =
            H.th [] [ H.text label ]

        thead =
            H.thead [] (List.map colHeader [ "id", "state", "trusted", "score", "sent", "recv", "last connection" ])

        tbody =
            H.tbody [] (List.map row peers)
    in
        H.table [ HA.class "peers" ] [ thead, tbody ]


viewContract : Chain.ContractID -> RemoteData Http.Error Chain.Contract -> Html Msg
viewContract contractId contract =
    H.div []
        [ H.h3 []
            [ H.text "Contract "
            , H.span [ HA.class "hash" ] [ H.text contractId ]
            ]
        , H.text (toString contract)
        ]


viewError : String -> List Error -> Html Msg
viewError nodeUrl errors =
    H.div [ HA.class "error" ]
        [ H.h1 [] [ H.text "Errors" ]
        , H.button [ HE.onClick ClearErrors ] [ H.text "Clear all errors" ]
        , H.div [] (List.map (viewErrorInfo nodeUrl) errors)
        ]


viewErrorInfo nodeUrl error =
    case error of
        HttpError (Http.BadPayload message response) ->
            H.div []
                [ H.h4 [] [ H.text "Bad Payload (JSON parsing problem)" ]
                , H.div [] [ H.text message ]
                ]

        HttpError (Http.BadStatus response) ->
            H.div []
                [ H.h4 [] [ H.text "Bad response status from node" ]
                , H.div [] [ H.text (toString response.status) ]
                , H.div [] [ H.text response.url ]

                --, H.div [ HA.style [ ( "white-space", "pre" ) ] ] [ H.text (toString response) ]
                ]

        HttpError Http.NetworkError ->
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
