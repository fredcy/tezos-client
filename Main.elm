module Main exposing (main)

import Html as H exposing (Html)
import Html.Attributes as HA
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Schema exposing (..)


type alias Model =
    { blocks : List (List Block)
    , schemaData : Maybe SchemaData
    , error : Maybe Http.Error
    , nodeUrl : String
    }


type Msg
    = NoOp
    | LoadBlocks (Result Http.Error BlocksData)
    | LoadSchema (Result Http.Error SchemaData)
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
        body =
            [ ( "operations", Encode.bool True ) ]
                |> Encode.object
                |> Http.jsonBody

        url =
            nodeUrl ++ "/blocks"
    in
        Http.post url body decodeBlocks


type alias Fitness =
    String


type alias Timestamp =
    String


type alias Block =
    { hash : String
    , predecessor : String
    , fitness : List Fitness
    , timestamp :
        String
        -- TODO convert to date value
    }


type alias BlocksData =
    List (List Block)


decodeBlocks : Decode.Decoder BlocksData
decodeBlocks =
    Decode.field "blocks" (Decode.list (Decode.list decodeBlock))


decodeBlock : Decode.Decoder Block
decodeBlock =
    Decode.map4 Block
        (Decode.field "hash" Decode.string)
        (Decode.field "predecessor" Decode.string)
        (Decode.field "fitness" (Decode.list Decode.string))
        (Decode.field "timestamp" decodeTimestamp)


decodeTimestamp : Decode.Decoder Timestamp
decodeTimestamp =
    -- TODO
    Decode.string


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
            { blocks = [], schemaData = Nothing, error = Nothing, nodeUrl = flags.nodeUrl }
    in
        ( model
        , Cmd.batch
            [ Http.send LoadBlocks (getBlocks model.nodeUrl)
            , Http.send LoadSchema (getSchema model.nodeUrl)
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadBlocks blocksMaybe ->
            case blocksMaybe of
                Ok blocks ->
                    ( { model | blocks = blocks, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        LoadSchema schemaMaybe ->
            case schemaMaybe of
                Ok schemaData ->
                    ( { model | schemaData = Just schemaData, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        SchemaMsg msg ->
            let
                newSchema =
                    Schema.update msg model.schemaData
            in
                ( { model | schemaData = newSchema }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    H.div []
        [ viewBlocks model.blocks
        , viewError model.nodeUrl model.error
        , case model.schemaData of
            Just schemaData ->
                viewSchemaDataTop schemaData |> H.map SchemaMsg

            Nothing ->
                H.text ""
          --, viewSchemaDataRaw model.schemaData |> H.map SchemaMsg
          --, viewDebug model
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
            [ H.h2 [] [ H.text "Block chain" ] ]
    in
        H.div [ HA.class "branches" ] (header ++ List.indexedMap viewBranch branches)


viewBlock : Int -> Block -> Html Msg
viewBlock n block =
    let
        viewProperty label value =
            H.div [ HA.class "property" ]
                [ H.label [ HA.for label ] [ H.text label ]
                , H.span [ HA.id label ] [ H.text value ]
                ]
    in
        H.div [ HA.class "block" ]
            [ H.h4 [] [ H.text ("block " ++ toString n) ]
            , viewProperty "hash" block.hash
            , viewProperty "predecessor" block.predecessor
            , viewProperty "timestamp" block.timestamp
            ]


viewError : String -> Maybe Http.Error -> Html Msg
viewError nodeUrl errorMaybe =
    case errorMaybe of
        Just error ->
            H.div [ HA.class "error" ]
                [ H.h1 [] [ H.text "Error" ]
                , viewErrorInfo nodeUrl error
                ]

        Nothing ->
            H.text ""


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
