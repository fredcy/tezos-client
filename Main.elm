module Main exposing (main)

import Html as H exposing (Html)
import Html.Attributes as HA
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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


type alias Model =
    { blocks : List (List Block)
    , schemaData : Maybe SchemaData
    , errors : List Http.Error
    , nodeUrl : String
    , operations : RemoteData Http.Error (List Operation)
    }


type alias Operation =
    { hash : OperationID
    , netID : NetID
    , data : String
    }


type Msg
    = NoOp
    | LoadBlocks (Result Http.Error BlocksData)
    | LoadSchema (Result Http.Error SchemaData)
    | LoadOperations (Result Http.Error (List Operation))
    | SchemaMsg Schema.Msg


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
            5

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

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    H.div []
        [ viewHeader model.nodeUrl
        , viewError model.nodeUrl model.errors
        , viewBlocks model.blocks
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
        , H.div [] [ H.text ("Connecting to server " ++ nodeUrl) ]
        ]


viewBlocks : List (List Block) -> Html Msg
viewBlocks branches =
    let
        viewBranch n branch =
            H.div [ HA.class "branch" ]
                [ H.h3 [] [ H.text ("branch " ++ toString n) ]
                , H.div [] (List.indexedMap viewBlock branch)
                ]

        header =
            [ H.h2 [] [ H.text "Block chains" ] ]
    in
        H.div [ HA.class "branches" ] (header ++ List.indexedMap viewBranch branches)


viewBlock : Int -> Block -> Html Msg
viewBlock n block =
    let
        viewProperty : String -> String -> Html Msg
        viewProperty label value =
            let
                id =
                    label ++ toString n
            in
                H.div [ HA.class "property" ]
                    [ H.label [ HA.for id ] [ H.text label ]
                    , H.span [ HA.id id ] [ H.text value ]
                    ]

        viewPropertyList : String -> List String -> Html Msg
        viewPropertyList label values =
            let
                id =
                    label ++ toString n
            in
                H.div [ HA.class "property" ]
                    [ H.label [ HA.for id ] [ H.text label ]
                    , H.span [ HA.id id ]
                        [ (List.intersperse ", " values |> String.concat |> H.text) ]
                    ]
    in
        H.div [ HA.class "block" ]
            [ H.h4 [] [ H.text ("block " ++ toString n) ]
            , viewProperty "hash" block.hash
            , viewProperty "predecessor" block.predecessor
            , viewProperty "timestamp" block.timestamp
            , viewPropertyList "fitness" block.fitness
            , viewPropertyList "operations" block.operations
            ]


viewOperations : RemoteData Http.Error (List Operation) -> Html Msg
viewOperations operationsStatus =
    H.div []
        [ H.h2 [] [ H.text "Operations" ]
        , case operationsStatus of
            Success operations ->
                H.div [] (List.map viewOperation operations)

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
                [ H.label [ HA.for id ] [ H.text "data" ]
                , H.span [ HA.id id, HA.class "operation-data" ] [ H.text operation.data ]
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
