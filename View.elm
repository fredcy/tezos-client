module View exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Dict exposing (Dict)
import List.Extra as List
import ParseInt
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
        , viewShowBlock model.blockChains model.showBlock
        , viewShowOperation model.operations model.showOperation
        , viewParse model.parsedOperations model.showOperation
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
                , H.td [ HA.class "timestamp" ] [ H.text block.timestamp ]
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
            , H.div [ HA.class "scrollable2 heads" ]
                [ H.table [ HA.class "heads" ]
                    [ H.thead [] [ header ]
                    , H.tbody [] (List.indexedMap viewHead heads)
                    ]
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


getBranchList : Dict BlockID Block -> BlockID -> List Block
getBranchList blocks blockhash =
    let
        helper hash blockList =
            Dict.get hash blocks
                |> Maybe.map (\block -> helper block.predecessor (block :: blockList))
                |> Maybe.withDefault blockList
    in
        helper blockhash [] |> List.reverse


viewShowBranch : Model -> Html Msg
viewShowBranch model =
    case model.showBranch of
        Just hash ->
            getBranchList model.blocks hash
                 |> viewBranch Nothing

        Nothing ->
            H.h4 [] [ H.text "no branch selected" ]


viewBranch : Maybe BlockID -> List Block -> Html Msg
viewBranch blockhashMaybe branch =
    let
        tableHeader =
            H.tr []
                [ H.th [] [ H.text "hash" ]
                , H.th [ HA.class "timestamp" ] [ H.text "timestamp" ]
                ]
    in
        H.div []
            [ H.h3 [] [ H.text ("branch") ]
            , H.div [ HA.class "branch scrollable" ]
                [ H.table [ HA.class "blockchain" ]
                    [ H.thead [] [ tableHeader ]
                    , H.tbody [] (List.indexedMap (viewBlock2 blockhashMaybe) branch)
                    ]
                ]
            ]


viewBlock2 : Maybe BlockID -> Int -> Block -> Html Msg
viewBlock2 blockhashMaybe n block =
    H.tr
        [ HA.classList
            [ ( "block", True )
            , ( "selected", Maybe.map ((==) block.hash) blockhashMaybe |> Maybe.withDefault False )
            ]
        ]
        [ H.td
            [ HA.class "hash"
            , HA.title block.hash
            , HE.onClick (ShowBlock block.hash)
            ]
            [ H.text (shortHash block.hash) ]
        , H.td [ HA.class "timestamp" ] [ H.text block.timestamp ]
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
viewShowBlock blockChains blockhashMaybe =
    case blockhashMaybe of
        Just blockhash ->
            case findBlock blockChains blockhash of
                Just block ->
                    viewBlock block

                Nothing ->
                    H.div [] [ H.text ("Cannot find block " ++ blockhash) ]

        Nothing ->
            H.text ""


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
