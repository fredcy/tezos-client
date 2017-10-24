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
import Regex
import RemoteData exposing (RemoteData)
import Table
import Data.Chain as Chain exposing (BlockID, Block, Contract, ContractID, OperationID, ParsedOperation, Base58CheckEncodedSHA256, SubOperation(..), getBranchList)
import Data.Schema as Schema
import Data.Michelson as Michelson
import Model exposing (..)
import Page
import Route
import Update exposing (Msg(..))
import View.Page
import View.Accounts
import View.Field as VF


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

                Loaded Page.Chain2 ->
                    viewChain2 model

                Loaded Page.Contracts ->
                    viewContracts model

                Loaded Page.Accounts ->
                    viewAccounts model

                Loaded (Page.Account hash) ->
                    viewAccount model hash

                Loaded Page.Keys ->
                    viewKeys model.chain.keys

                Loaded Page.Peers ->
                    viewPeers model.chain.peers

                Loaded (Page.Contract contractId) ->
                    viewContract contractId model.chain.contracts

                Loaded Page.Errors ->
                    viewError model.nodeUrl model.errors

                Loaded Page.NotFound ->
                    H.text "page not found"

                Loaded Page.About ->
                    viewAbout model
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
                H.text "no head found yet ..."


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
                    , HA.title block.hash
                    ]
                    [ H.a [ Route.href (Route.ChainAt block.hash) ]
                        [ H.text (shortHash block.hash) ]
                    ]
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
    H.tr [ HA.class "block" ]
        [ H.td [] [ H.text (toString block.level) ]
        , H.td
            [ HA.class "hash"
            , HA.title block.hash
            ]
            [ H.a [ Route.href (Route.Block block.hash) ] [ H.text (shortHash block.hash) ] ]
        , H.td [ HA.class "timestamp" ]
            [ H.text (formatDate block.timestamp) ]
        , H.td [ HA.class "age" ]
            [ H.text (Date.Distance.inWords model.now block.timestamp) ]
        , H.td [ HA.class "operation-count" ]
            [ H.text (blockOperationCount model block) ]
        ]


blockFullLink : BlockID -> Html Msg
blockFullLink hash =
    H.span [ HA.class "hash" ]
        [ H.a [ Route.href (Route.Block hash) ] [ H.text hash ] ]


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
                , viewProperty "branch" (H.text operation.branch)
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
                    H.text "(loading operation data...)"
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


formatCentiles : Int -> String
formatCentiles number =
    let
        usLocale =
            FormatNumber.Locales.usLocale
    in
        (toFloat number / 100)
            |> FormatNumber.format { usLocale | decimals = 2 }


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
                , H.text (formatCentiles amount)
                , H.text " ꜩ to "
                , H.a [ Route.href (Route.Account address), HA.title address, HA.class "hash" ] [ H.text (shortHash address) ]
                ]

        Delegation delegatee ->
            H.span []
                [ H.text "Delegation to "
                , H.span [ HA.class "hash" ]
                    [ H.a [ Route.href (Route.Contract delegatee) ] [ H.text (shortHash delegatee) ] ]
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
                [ H.td [ HA.class "hash" ]
                    [ H.a
                        [ Route.href (Route.Operation operation.hash)
                        , HA.title operation.hash
                        ]
                        [ H.text (shortHash operation.hash) ]
                    ]
                , H.td [ HA.class "hash", HA.title (sourceTitle operation.source) ]
                    [ H.text (viewSourceMaybe operation.source) ]
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
        , H.p [] [ H.text """(This displays only those operations that we have seen in the most
                           recent blocks. This will be improved soon.)""" ]
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


viewChain2 : Model -> Html Msg
viewChain2 model =
    H.div []
        [ H.h3 [] [ H.text "Chain2" ]
        ]


viewAccounts : Model -> Html Msg
viewAccounts model =
    H.div []
        [ H.h3 [] [ H.text "Accounts" ]
        , case model.chain.accounts of
            RemoteData.Success accounts ->
                --viewAccountTable accounts
                View.Accounts.view model.tableState model.query accounts

            RemoteData.NotAsked ->
                H.div [] [ H.text "..." ]

            _ ->
                H.div [] [ H.text (toString model.chain.accounts) ]
        ]


viewAccountTable : List Chain.AccountSummary -> Html Msg
viewAccountTable accounts =
    let
        thead =
            H.thead []
                [ H.tr []
                    [ H.th [] [ H.text "" ]
                    , H.th [ HA.colspan 2 ] [ H.text "source (debit)" ]
                    , H.th [ HA.colspan 2 ] [ H.text "destination (credit)" ]
                    ]
                , H.tr []
                    [ H.th [] [ H.text "account hash" ]
                    , H.th [] [ H.text "count" ]
                    , H.th [] [ H.text "sum (ꜩ)" ]
                    , H.th [] [ H.text "count" ]
                    , H.th [] [ H.text "sum (ꜩ)" ]
                    ]
                ]

        row a =
            H.tr []
                [ H.td [ HA.class "hash" ]
                    [ H.a
                        [ Route.href (Route.Account a.hash) ]
                        [ H.text a.hash ]
                    ]
                , H.td [ HA.class "number" ] [ H.text (toString a.sourceCount) ]
                , H.td [ HA.class "number" ] [ H.text (formatCentiles a.sourceSum) ]
                , H.td [ HA.class "number" ] [ H.text (toString a.destCount) ]
                , H.td [ HA.class "number" ] [ H.text (formatCentiles a.destSum) ]
                ]
    in
        H.div []
            [ H.p [] [ H.text "This summarizes all the accounts that have participated in transactions." ]
            , H.table [ HA.class "accounts" ]
                [ thead
                , H.tbody [] (List.map row accounts)
                ]
            ]


viewAccount : Model -> Chain.AccountID -> Html Msg
viewAccount model accountHash =
    H.div []
        [ H.h3 [] [ H.text ("Account " ++ accountHash) ]
        , case model.chain.account of
            Just { hash, transactions } ->
                viewTransactions hash transactions

            _ ->
                H.div [] [ H.text "..." ]
        ]


viewTransactions : Chain.AccountID -> List Chain.TransactionSummary -> Html Msg
viewTransactions accountHash transactions =
    let
        class : Chain.TransactionSummary -> String
        class t =
            if t.source == accountHash then
                "source"
            else if t.destination == accountHash then
                "destination"
            else
                ""

        thead =
            H.tr []
                [ H.th [] [ H.text "time" ]
                , H.th [] [ H.text "source" ]
                , H.th [] [ H.text "destination" ]
                , H.th [] [ H.text "amount (ꜩ)" ]
                ]

        row t =
            H.tr [ HA.class (class t) ]
                [ H.td [] [ H.a [ Route.href (Route.Block t.block) ] [ H.text (formatDate t.timestamp) ] ]
                , H.td [ HA.class "hash" ]
                    [ H.a [ Route.href (Route.Account t.source) ] [ H.text (shortHash t.source) ]
                    ]
                , H.td [ HA.class "hash" ]
                    [ H.a [ Route.href (Route.Account t.destination) ] [ H.text (shortHash t.destination) ] ]
                , H.td [ HA.class "number amount" ] [ H.text (formatCentiles t.amount) ]
                ]
    in
        H.div []
            [ H.h4 [] [ H.text "Transactions" ]
            , H.table [ HA.class "transactions" ]
                [ thead
                , H.tbody [] (List.map row transactions)
                ]
            ]


viewContracts : Model -> Html Msg
viewContracts model =
    H.div []
        [ H.h3 [] [ H.text "Contracts" ]
        , case model.chain.contractIDs of
            RemoteData.Success contractIDs ->
                viewContractTable contractIDs model.chain.contracts

            RemoteData.Loading ->
                H.text "loading ..."

            _ ->
                H.text (toString model.chain.contractIDs)
        ]


viewContractTable : List ContractID -> Dict ContractID (RemoteData Http.Error Contract) -> Html Msg
viewContractTable contractIDs contracts =
    let
        thead =
            H.tr []
                [ H.th [] [ H.text "contractID" ]
                , H.th [] [ H.text "balance (ꜩ)" ]
                , H.th [] [ H.text "manager" ]
                , H.th [] [ H.text "counter" ]
                , H.th [] [ H.text "script size" ]
                ]

        trow contractId =
            let
                contractData =
                    Dict.get contractId contracts
                        |> Maybe.withDefault RemoteData.NotAsked
            in
                case contractData of
                    RemoteData.Success contract ->
                        H.tr []
                            [ H.td [ HA.class "hash" ]
                                [ H.a [ Route.href (Route.Contract contractId) ]
                                    [ H.text (shortHash contractId) ]
                                ]
                            , H.td [ HA.class "balance" ] [ H.text (formatCentiles contract.balance) ]
                            , H.td [ HA.class "hash" ]
                                [ H.a [ Route.href (Route.Contract contract.manager) ]
                                    [ H.text (shortHash contract.manager) ]
                                ]
                            , H.td [ HA.class "number" ] [ H.text (toString contract.counter) ]
                            , H.td [ HA.class "number" ]
                                [ contract.script
                                    |> Maybe.map (toString >> String.length >> toString)
                                    |> Maybe.withDefault ""
                                    |> H.text
                                ]
                            ]

                    RemoteData.NotAsked ->
                        H.tr []
                            [ H.td [ HA.class "hash link" ] [ H.text (shortHash contractId) ]
                            , H.td [] [ H.text "..." ]
                            ]

                    RemoteData.Loading ->
                        H.tr []
                            [ H.td [ HA.class "hash link" ] [ H.text (shortHash contractId) ]
                            , H.td [] [ H.text "......" ]
                            ]

                    RemoteData.Failure error ->
                        H.tr []
                            [ H.td [ HA.class "hash link" ] [ H.text (shortHash contractId) ]
                            , H.td [] [ H.text (toString error) ]
                            ]

        tbody =
            H.tbody [] (List.map trow (List.sort contractIDs))
    in
        H.table [ HA.class "contracts" ] [ thead, tbody ]


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


viewContract : ContractID -> Dict ContractID (RemoteData Http.Error Contract) -> Html Msg
viewContract contractId contracts =
    let
        contractData =
            Dict.get contractId contracts
                |> Maybe.withDefault RemoteData.NotAsked

        viewDelegate delegate =
            H.span []
                [ H.span [ HA.class "hash" ]
                    [ H.text (Maybe.withDefault "" delegate.value) ]
                , H.text
                    (" ("
                        ++ (if delegate.setable then
                                "setable"
                            else
                                "not setable"
                           )
                        ++ ")"
                    )
                ]

        viewScript scriptMaybe =
            scriptMaybe
                |> Maybe.map
                    (\script ->
                        viewProperty "script" (H.text (toString script))
                    )
                |> Maybe.withDefault (H.text "")

        viewProg scriptMaybe =
            scriptMaybe
                |> Maybe.map
                    (\script ->
                        viewProperty "program" (simplifyProgram1 script.code.code |> viewProgram)
                    )
                |> Maybe.withDefault (H.text "")
    in
        H.div []
            [ H.h3 []
                [ H.text "Contract "
                , H.span [ HA.class "hash" ] [ H.text contractId ]
                ]
            , case contractData of
                RemoteData.Success contract ->
                    H.div []
                        [ viewProperty "manager" (H.span [ HA.class "hash" ] [ H.text contract.manager ])
                        , viewProperty "balance (ꜩ)" (H.text (formatCentiles contract.balance))
                        , viewProperty "spendable" (H.text (toString contract.spendable))
                        , viewProperty "counter" (H.text (toString contract.counter))
                        , viewProperty "delegate" (viewDelegate contract.delegate)

                        --, viewScript contract.script
                        , viewProg contract.script

                        --, viewProperty "raw data" (H.div [] [ H.text (toString contract.raw) ])
                        -- , H.h5 [] [ H.text "Raw response" ]
                        -- , H.pre [] [ H.text contract.rawBody ]
                        ]

                RemoteData.Loading ->
                    H.text "loading ..."

                RemoteData.Failure error ->
                    viewFailure error

                RemoteData.NotAsked ->
                    H.text "not asked"
            ]


{-| Convert Michelson primitive name to valid CSS class name.
-}
primCss : String -> String
primCss prim =
    String.map
        (\c ->
            if c == '_' then
                '-'
            else
                c
        )
        prim


viewProgram : Michelson.Program -> Html Msg
viewProgram program =
    case program of
        Michelson.IntT i ->
            H.span [ HA.class "IntT" ] [ H.text (toString i) ]

        Michelson.StringT s ->
            H.span [ HA.class "StringT" ] [ H.text ("\"" ++ s ++ "\"") ]

        Michelson.PrimT p ->
            H.span [ HA.class ("PrimT prim-" ++ primCss p) ] [ H.text p ]

        Michelson.SeqT seq ->
            H.div [ HA.class "SeqT" ]
                [ H.text " { "
                , H.div [ HA.class "sequence" ] (List.map viewProgram seq)
                , H.text " } "
                ]

        Michelson.PrimArgT p arg ->
            H.span [ HA.class ("PrimArgT primarg primarg-" ++ primCss p) ]
                [ H.span [ HA.class ("PrimT prim-" ++ primCss p) ] [ H.text p ]
                , H.span [ HA.class ("primargarg primargarg-" ++ primCss p) ]
                    [ viewProgram arg ]
                ]

        Michelson.EmptyT ->
            H.span [ HA.class "EmptyT" ] [ H.text "{}" ]


simplifyProgram1 program =
    combinePrimitives program
        |> hoistSingletonLists


combinePrimitives : Michelson.Program -> Michelson.Program
combinePrimitives program =
    let
        simplifySeq item sofar =
            case ( item, sofar ) of
                ( Michelson.PrimT prim1, (Michelson.PrimT prim2) :: rest ) ->
                    combinePrims prim1 prim2
                        |> Maybe.map (\newprim -> Michelson.PrimT newprim :: rest)
                        |> Maybe.withDefault (item :: sofar)

                _ ->
                    combinePrimitives item :: sofar

        cxrLetters : String -> Maybe String
        cxrLetters prim =
            -- Extract letter(s) from middle of CxR primitive name. E.g.:
            -- cxrLetters "CDAAR" == Just "DAA"
            -- cxrLetters "DUP" == Nothing
            Regex.find (Regex.AtMost 1) (Regex.regex "^C([AD])+R$") prim
                |> List.head
                |> Maybe.map .submatches
                -- should be exactly one submatch
                |> Maybe.andThen List.head
                |> Maybe.withDefault Nothing

        -- Try to combine two primitives into one.
        combinePrims : String -> String -> Maybe String
        combinePrims prim1 prim2 =
            cxrLetters prim1
                |> Maybe.andThen
                    (\letters1 ->
                        cxrLetters prim2
                            |> Maybe.andThen
                                (\letters2 ->
                                    Just ("C" ++ letters1 ++ letters2 ++ "R")
                                )
                    )
    in
        case program of
            Michelson.SeqT seq ->
                Michelson.SeqT (List.foldr simplifySeq [] seq)

            Michelson.PrimArgT prim arg ->
                Michelson.PrimArgT prim (combinePrimitives arg)

            _ ->
                program


hoistSingletonLists : Michelson.Program -> Michelson.Program
hoistSingletonLists program =
    case program of
        Michelson.SeqT [ singleton ] ->
            hoistSingletonLists singleton

        Michelson.SeqT seq ->
            Michelson.SeqT (List.map hoistSingletonLists seq)

        Michelson.PrimArgT prim arg ->
            Michelson.PrimArgT prim (hoistSingletonLists arg)

        _ ->
            program


viewFailure : Http.Error -> Html Msg
viewFailure error =
    case error of
        Http.BadPayload message response ->
            H.div [ HA.class "badpayload" ]
                [ H.h4 [] [ H.text "Error: Bad Payload" ]
                , H.pre [ HA.class "payload-message" ] [ H.text message ]
                , H.h5 [] [ H.text "Response" ]
                , H.div [] [ H.text (toString response) ]
                , H.h5 [] [ H.text "Body" ]
                , viewResponseBody response.body
                ]

        _ ->
            H.text (toString error)


viewResponseBody : String -> Html Msg
viewResponseBody body =
    H.div [ HA.style [ ( "white-space", "pre" ) ] ] [ H.text body ]


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
                , H.h5 [] [ H.text "Response body" ]
                , H.pre [] [ H.text response.body ]
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


viewAbout : Model -> Html Msg
viewAbout model =
    H.div []
        [ H.h3 [] [ H.text "About" ]
        , H.p []
            [ H.text "This Tezos blockchain explorer displays data from the Tezos "
            , H.i [] [ H.text "alphanet" ]
            , H.text ", the testnet for Tezos."
            ]
        , H.p []
            [ H.text "The data comes via the Tezos RPC over HTTPS from a single node on the network ("
            , H.text model.nodeUrl
            , H.text ") and therefore depends on the status of that node. Being a test network, sometimes that node may be out of service or out of sync with the current alphanet chain. "
            ]
        , H.p []
            [ H.text """This Tezos Explorer client was developed by Fred Yankowski (@fredcy) and is under """
            , H.a [ HA.href "https://github.com/fredcy/tezos-client" ] [ H.text "active development. " ]
            , H.text "Suggestions for additions or changes to this client are welcome. "
            ]
        ]
