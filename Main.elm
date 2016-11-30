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
    }


type Msg
    = NoOp
    | LoadBlocks (Result Http.Error BlocksData)
    | LoadSchema (Result Http.Error SchemaData)
    | SchemaMsg Schema.Msg


main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


{-| Construct an RPC request. This should use Http.jsonBody but the
"Content-type: application/json" header resulting from that seems to cause CORS
problems for the Tezos server.
-}
getBlocks : Http.Request BlocksData
getBlocks =
    let
        constructBody value =
            Encode.encode 0 value |> Http.stringBody "multipart/form-data"

        body =
            [ ( "operations", Encode.bool True ) ]
                |> Encode.object
                |> Http.jsonBody
    in
        Http.post "http://localhost:8732/blocks" body decodeBlocks


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


getSchema : Http.Request SchemaData
getSchema =
    let
        body =
            [ ( "recursive", Encode.bool True ) ] |> Encode.object |> Http.jsonBody
    in
        Http.post "http://localhost:8732/describe" body decodeSchema


init : ( Model, Cmd Msg )
init =
    ( { blocks = [], schemaData = Nothing, error = Nothing }
    , Cmd.batch
        [ Http.send LoadBlocks getBlocks
        , Http.send LoadSchema getSchema
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
        , viewError model.error
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
                [ H.h2 [] [ H.text ("branch " ++ toString n) ]
                , H.div [] (List.indexedMap viewBlock branch)
                ]
    in
        H.div [ HA.class "branches" ] (List.indexedMap viewBranch branches)


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
            [ H.h3 [] [ H.text ("block " ++ toString n) ]
            , viewProperty "hash" block.hash
            , viewProperty "predecessor" block.predecessor
            , viewProperty "timestamp" block.timestamp
            ]


viewError : Maybe Http.Error -> Html Msg
viewError errorMaybe =
    case errorMaybe of
        Just error ->
            case error of
                Http.BadPayload message response ->
                    H.div [ HA.class "error" ]
                        [ H.h1 [] [ H.text "Error" ]
                        , H.h2 [] [ H.text "Bad Payload (JSON parsing problem)" ]
                        , H.div [ HA.style [ ( "white-space", "pre" ) ] ] [ H.text message ]
                        ]

                _ ->
                    H.div [ HA.class "error" ] [ H.text (toString error) ]

        Nothing ->
            H.text ""


viewDebug : Model -> Html Msg
viewDebug model =
    H.div [ HA.class "debug" ]
        [ H.h2 [] [ H.text "Raw model" ]
        , H.text <| toString model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
