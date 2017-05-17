module View exposing (view)

import Date exposing (Date)
import Date.Distance
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Dict exposing (Dict)
import List.Extra as List
import ParseInt
import Date.Format
import Data.Chain exposing (BlockID, Block, OperationID, ParsedOperation, Base58CheckEncodedSHA256, SubOperation(..), getBranchList)
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
                            viewBlock block

                        Nothing ->
                            H.text "loading block ..."

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
                    , getBranchList model.chain head
                        |> viewBranch 8 model.now (Just head)
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
        [ H.h1 [] [ H.text "Tezos client" ]
        , H.div [] [ H.text ("Connecting to Tezos RPC server " ++ nodeUrl) ]
        ]


canonFitness : List String -> List Int
canonFitness strings =
    List.map (ParseInt.parseIntHex >> Result.withDefault 0) strings
        |> List.dropWhile ((==) 0)


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
                , H.th [ HA.class "fitness" ] [ H.text "fitness" ]
                , H.th [ HA.class "level" ] [ H.text "level" ]
                ]

        viewBlockSummary : Int -> Block -> Bool -> Html Msg
        viewBlockSummary i block beingShown =
            H.tr []
                [ H.td [ HA.class "index" ] [ H.text (toString i) ]
                , H.td
                    [ HA.class "hash"
                    , HE.onClick (ShowBlock block.hash)
                    , HA.title block.hash
                    ]
                    [ H.text (shortHash block.hash) ]
                , H.td [ HA.class "timestamp" ] [ H.text (formatTimestamp model.now block.timestamp) ]
                , H.td [ HA.class "fitness" ] [ H.text (toString (canonFitness block.fitness)) ]
                , H.td [ HA.class "level" ] [ H.text (toString block.level) ]
                ]

        isBeingShown : Maybe BlockID -> BlockID -> Bool
        isBeingShown showBranch id =
            Maybe.map ((==) id) showBranch |> Maybe.withDefault False

        viewHead : Int -> BlockStatus -> Html Msg
        viewHead i blockStatus =
            case blockStatus of
                BlockFound block ->
                    viewBlockSummary i block (isBeingShown model.showBranch block.hash)

                _ ->
                    H.text ""
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


viewShowBranch : Model -> Html Msg
viewShowBranch model =
    case model.showBranch of
        Just hash ->
            getBranchList model.chain hash
                |> viewBranch 4 model.now model.showBlock

        Nothing ->
            H.h4 [] [ H.text "no branch selected" ]


viewBranch : Int -> Date -> Maybe BlockID -> List Block -> Html Msg
viewBranch howMany now blockhashMaybe branch =
    let
        tableHeader =
            H.tr []
                [ H.th [] [ H.text "level" ]
                , H.th [] [ H.text "hash" ]
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
                    , H.tbody [] (List.indexedMap (viewBlock2 now blockhashMaybe) branchToShow)
                    ]
                ]
            ]


formatTimestamp : Date -> Date -> String
formatTimestamp now date =
    let
        distance =
            Date.Distance.inWords now date
    in
        formatDate date ++ " (" ++ distance ++ ")"


blockOperationCount : Block -> String
blockOperationCount block =
    case block.operations of
        Just listList ->
            List.concat listList |> List.length |> toString

        Nothing ->
            "unknown"


viewBlock2 : Date -> Maybe BlockID -> Int -> Block -> Html Msg
viewBlock2 now blockhashMaybe n block =
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
            [ H.text (formatTimestamp now block.timestamp) ]
        , H.td [ HA.class "operation-count" ]
            [ H.text (blockOperationCount block) ]
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
viewBlock : Block -> Html Msg
viewBlock block =
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
                , viewPropertyList "fitness" block.fitness
                , viewPropertyString "net_id" block.net_id
                , viewOperations block
                ]
            ]


formatDate : Date -> String
formatDate date =
    Date.Format.format "%Y-%m-%d %H:%M:%S" date


viewShowBlock : Dict BlockID Block -> Maybe BlockID -> Html Msg
viewShowBlock blocks blockhashMaybe =
    blockhashMaybe
        |> Maybe.andThen (\hash -> Dict.get hash blocks)
        |> Maybe.map viewBlock
        |> Maybe.withDefault (H.text "")


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


viewSuboperation : SubOperation -> Html Msg
viewSuboperation suboperation =
    case suboperation of
        Endorsement blockid int ->
            H.span []
                [ H.text "Endorsement of "
                , H.span [ HA.class "hash" ] [ H.text (shortHash blockid) ]
                , H.text (", " ++ (toString int))
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

        viewSourceMaybe : Maybe Data.Chain.SourceID -> String
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
            --            |> List.concat
            |> List.sortBy .hash
            |> viewOperationsTable
        ]


shortHash : Base58CheckEncodedSHA256 -> String
shortHash hash =
    String.left 14 hash


viewError : String -> List Http.Error -> Html Msg
viewError nodeUrl errors =
    H.div [ HA.class "error" ]
        [ H.h1 [] [ H.text "Errors" ]
        , H.button [ HE.onClick ClearErrors ] [ H.text "Clear all errors" ]
        , H.div [] (List.map (viewErrorInfo nodeUrl) errors)
        ]


viewErrorInfo nodeUrl error =
    case error of
        Http.BadPayload message response ->
            H.div []
                [ H.h4 [] [ H.text "Bad Payload (JSON parsing problem)" ]
                , H.div [] [ H.text message ]
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
