module View exposing (view)

import Date exposing (Date)
import Date.Distance
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import InfiniteScroll
import Dict exposing (Dict)
import List.Extra as List
import Regex
import RemoteData exposing (RemoteData)
import Data.Chain as Chain exposing (BlockID, Block, Contract, ContractID, OperationID, ParsedOperation, SubOperation(..), getBranchList)
import Data.Schema as Schema
import Data.Michelson as Michelson
import Model exposing (Error(HttpError), Model, PageState(Loaded))
import Page
import Route
import Msg exposing (Msg(ClearErrors, SchemaMsg))
import View.Page
import View.Block
import View.Accounts
import View.AccountTransactions
import View.Chain
import View.Contracts
import View.Field exposing (formatCentiles, formatDate, shortHash)
import View.Peers


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
                Loaded (Page.Home scrollState) ->
                    viewHome scrollState model

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
                    H.text "page not implemented"

                Loaded Page.Contracts ->
                    viewContracts model

                Loaded Page.Accounts ->
                    viewAccounts model

                Loaded (Page.Account hash) ->
                    viewAccount model hash

                Loaded Page.Keys ->
                    viewKeys model.chain.keys

                Loaded Page.Peers ->
                    View.Peers.view model.chain.peers model.peerTableState

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


viewHome : InfiniteScroll.Model Msg -> Model -> Html Msg
viewHome scrollState model =
    H.div []
        [ H.h2 [] [ H.text "Newest blocks" ]
        , View.Chain.view model.now model.windowSize scrollState model.chain
        ]


oldViewHome : Model -> BlockID -> Html Msg
oldViewHome model head =
    getBranchList model.chain head
        |> viewBranch 24 model (Just head)


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
viewBlock2 model _ _ block =
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
                ]
            , View.Block.viewOperationGroups model block.hash
            ]


viewOperation : Model -> OperationID -> Html Msg
viewOperation model operationId =
    let
        operationMaybe =
            Dict.get operationId model.chain.parsedOperations

        viewOperationFields : ParsedOperation -> Html Msg
        viewOperationFields operation =
            H.div []
                [ viewProperty "hash" (H.text operation.hash)
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


viewSuboperation : SubOperation -> Html Msg
viewSuboperation suboperation =
    case suboperation of
        Endorsement blockid int ->
            H.span []
                [ H.text "Endorsement of "
                , H.span [ HA.class "hash" ] [ H.text (shortHash blockid) ]
                , H.text (", slot " ++ toString int)
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


viewChainAt : Model -> BlockID -> Html Msg
viewChainAt model hash =
    H.div []
        [ H.h3 [] [ H.text ("Chain at " ++ shortHash hash) ]
        , getBranchList model.chain hash
            |> viewBranch 24 model (Just hash)
        ]


viewAccounts : Model -> Html Msg
viewAccounts model =
    H.div []
        [ H.h3 [] [ H.text "Accounts" ]
        , case model.chain.accounts of
            RemoteData.Success accounts ->
                View.Accounts.view model.tableState model.query accounts

            RemoteData.NotAsked ->
                H.div [] [ H.text "..." ]

            _ ->
                H.div [] [ H.text (toString model.chain.accounts) ]
        ]


viewAccount : Model -> Chain.AccountID -> Html Msg
viewAccount model accountHash =
    H.div []
        [ H.h3 [] [ H.text ("Account " ++ accountHash) ]
        , case model.chain.account of
            Just { hash, transactions } ->
                let
                    _ =
                        if hash == accountHash then
                            []
                        else
                            Debug.log "error: account hash mismatch" [ accountHash, hash ]
                in
                    H.div []
                        [ H.h4 [] [ H.text "Transactions" ]
                        , View.AccountTransactions.view accountHash model.transactionTableState transactions
                        ]

            _ ->
                H.div [] [ H.text "loading account data ..." ]
        , viewAccountHelp
        ]


viewAccountHelp : Html Msg
viewAccountHelp =
    H.div []
        [ H.hr [] []
        , H.ul []
            [ H.li [] [ H.text "This displays all transaction operations that have the account as source or destination." ]
            , H.li [] [ H.text "Click the column headers to sort by that column." ]
            , H.li [] [ H.text "The timestamp column values link to the block that comprises the transaction." ]
            , H.li [] [ H.text "The source and destination column values link to a detail page about the account." ]
            ]
        ]


viewContracts : Model -> Html Msg
viewContracts model =
    H.div []
        [ H.h3 [] [ H.text "Contracts" ]
        , case model.chain.contractIDs of
            RemoteData.Success contractIDs ->
                --View.Contracts.viewContractTable contractIDs model.chain.contracts
                View.Contracts.view contractIDs model.chain.contracts model.contractTableState

            RemoteData.Loading ->
                H.text "loading ..."

            _ ->
                H.text (toString model.chain.contractIDs)
        ]


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

        tableBody =
            H.tbody [] (List.map tableRow keys)

        tableRow key =
            H.tr []
                [ H.td [] [ H.span [ HA.class "hash" ] [ H.text key.hash ] ]
                , H.td [] [ H.span [ HA.class "hash" ] [ H.text key.public_key ] ]
                ]
    in
        H.table [ HA.class "keys" ] [ tableHead, tableBody ]


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

        viewScript script =
            H.div [ HA.class "program-as-value" ] [ H.text (toString script) ]

        viewProg scriptMaybe =
            scriptMaybe
                |> Maybe.map
                    (\script ->
                        --viewProperty "program" (simplifyProgram1 script.code |> viewProgram)
                        viewProperty "program" (viewScript script)
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



{- TODO
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


   simplifyProgram1 : Michelson.Program -> Michelson.Program
   simplifyProgram1 program =
       combinePrimitives program
           |> hoistSingletonLists


   matchesCxR : Regex.Regex
   matchesCxR =
       Regex.regex "^C([AD])+R$"


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
               Regex.find (Regex.AtMost 1) matchesCxR prim
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
-}


viewFailure : Http.Error -> Html Msg
viewFailure error =
    case error of
        Http.BadPayload message response ->
            H.div [ HA.class "badpayload" ]
                [ H.h4 [] [ H.text "Error: Bad Payload" ]
                , H.div [ HA.class "payload-message" ] [ H.text message ]
                , H.h5 [] [ H.text "Response" ]
                , H.div [ HA.class "response" ] [ H.text (toString response) ]
                , H.h5 [] [ H.text "Body" ]
                , viewResponseBody response.body
                ]

        _ ->
            H.text (toString error)


viewResponseBody : String -> Html Msg
viewResponseBody body =
    H.div [ HA.class "response-body" ] [ H.text body ]


viewError : String -> List Error -> Html Msg
viewError nodeUrl errors =
    H.div [ HA.class "error" ]
        [ H.h1 [] [ H.text "Errors" ]
        , H.button [ HE.onClick ClearErrors ] [ H.text "Clear all errors" ]
        , H.div [] (List.map (viewErrorInfo nodeUrl) errors)
        ]


viewErrorInfo : String -> Error -> Html Msg
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
viewAbout _ =
    H.div []
        [ H.h3 [] [ H.text "About" ]
        , H.p []
            [ H.text "This Tezos blockchain explorer displays data from the Tezos "
            , H.i [] [ H.text "alphanet" ]
            , H.text ", the testnet for Tezos."
            ]
        , H.p []
            [ H.text "The data comes via the Tezos RPC from a single node on the network "
            , H.text """ and therefore depends on the status of that node. Being
            a test network, sometimes that node may be out of service or out of
            sync with the current alphanet chain. """
            ]
        , H.p []
            [ H.text "Tezos Explorer is developed by Fred Yankowski ("
            , H.a [ HA.href "https://github.com/fredcy" ] [ H.text "@fredcy" ]
            , H.text ") and is under "
            , H.a [ HA.href "https://github.com/fredcy/tezos-client" ] [ H.text "active development" ]
            , H.text ". "
            , H.a [ HA.href "https://github.com/fredcy/tezos-client/issues" ] [ H.text "Issue reports and enhancement suggestions" ]
            , H.text " are welcome. "
            ]
        , H.p []
            [ H.text """Tezos Explorer has been online and available to the world
            continuously since December 2016. It is the first Tezos blockchain
            explorer by far.""" ]

        {- , H.p []
           [ H.text "Donations are welcome: "
           , H.span [ HA.class "hash" ] [ H.text "1FydzLatubB6Sp7D4hEE7YddbryTvWNs3j" ]
           , H.text " (BTC). (Tezos donation address soon to come!)"

           ]
        -}
        ]
