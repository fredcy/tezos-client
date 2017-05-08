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
import Schema
import Model exposing (..)
import Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    H.div []
        [ viewHeader model.nodeUrl
        , viewError model.nodeUrl model.errors
        , viewHeads model
        , viewShowBranch model
        , viewShowBlock model.blocks model.showBlock
        , viewShowBlockOperations model.blockOperations model.showBlock
        , viewAllOperations model
        , viewSchemas model.schemaData
          --, viewDebug model
        ]


viewSchemas : Dict SchemaName Schema.SchemaData -> Html Msg
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
        [ H.h1 [] [ H.text "Tezos client 3" ]
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
            List.map (findBlockStatus model.blocks) model.heads

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
            H.tr [ HA.classList [ ( "selected", beingShown ) ] ]
                [ H.td [ HA.class "index" ] [ H.text (toString i) ]
                , H.td
                    [ HA.class "hash"
                    , HE.onClick (ShowBranch block.hash)
                    , HA.title block.hash
                    ]
                    [ H.text (shortHash block.hash) ]
                , H.td [ HA.class "timestamp" ] [ H.text (formatDate block.timestamp) ]
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
            getBranchList model.blocks hash
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
            [ H.h3 [] [ H.text ("branch") ]
            , H.div [ HA.class "branch" ]
                [ H.table [ HA.class "blockchain" ]
                    [ H.thead [] [ tableHeader ]
                    , H.tbody [] (List.indexedMap (viewBlock2 now blockhashMaybe) branchToShow)
                    ]
                ]
            ]


viewBlock2 : Date -> Maybe BlockID -> Int -> Block -> Html Msg
viewBlock2 now blockhashMaybe n block =
    H.tr
        [ HA.classList
            [ ( "block", True )
            , ( "selected", Maybe.map ((==) block.hash) blockhashMaybe |> Maybe.withDefault False )
            ]
        ]
        [ H.td [] [ H.text (toString block.level) ]
        , H.td
            [ HA.class "hash"
            , HA.title block.hash
            , HE.onClick (ShowBlock block.hash)
            ]
            [ H.text (shortHash block.hash) ]
        , H.td [ HA.class "timestamp" ] [ H.text (Date.Distance.inWords now block.timestamp) ]
        , H.td [ HA.class "operation-count" ]
            [ List.concat block.operations |> List.length |> toString |> H.text ]
        ]


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
    in
        H.div [ HA.class "block" ]
            [ H.h3 []
                [ H.text "Block "
                , H.span [ HA.class "hash" ] [ H.text (shortHash block.hash) ]
                ]
            , H.div [ HA.class "property-list" ]
                [ viewPropertyString "hash" block.hash
                , viewPropertyString "predecessor" block.predecessor
                , viewPropertyString "timestamp" (formatDate block.timestamp)
                , viewPropertyList "fitness" block.fitness
                , viewPropertyString "net_id" block.net_id
                , viewPropertyList "operations" (List.concat block.operations |> List.map shortHash)
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


viewOperation : ParsedOperation -> Html Msg
viewOperation operation =
    H.div [] [ H.text (toString operation) ]


viewSuboperation : SubOperation -> Html Msg
viewSuboperation suboperation =
    case suboperation of
        Endorsement blockid int ->
            H.text ("Endorsement of " ++ shortHash blockid ++ ", " ++ (toString int))

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
                    , H.th [] [ H.text "net_id" ]
                    , H.th [] [ H.text "source" ]
                    , H.th [] [ H.text "sub-operations" ]
                    ]
                ]

        tableRow operation =
            H.tr []
                [ H.td [ HA.class "hash" ] [ H.text (shortHash operation.hash) ]
                , H.td [ HA.class "hash" ] [ H.text operation.net_id ]
                , H.td [ HA.class "hash" ] [ H.text (shortHash operation.source) ]
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
        , Dict.toList model.blockOperations |> List.map Tuple.second |> List.concat |> List.sortBy .hash |> viewOperationsTable
        ]


shortHash : Base58CheckEncodedSHA256 -> String
shortHash hash =
    String.left 12 hash


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
