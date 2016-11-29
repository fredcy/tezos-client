module Main exposing (main)

import Html
import Html.Attributes as HA
import Http
import Json.Decode as Decode


type alias Model =
    { blocks : List (List Block)
    , error : Maybe Http.Error
    }


type Msg
    = NoOp
    | LoadBlocks (Result Http.Error BlocksData)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


getBlocks : Http.Request BlocksData
getBlocks =
    Http.get "http://localhost:8732/blocks" decodeBlocks


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


init : ( Model, Cmd Msg )
init =
    ( { blocks = [], error = Nothing }, Http.send LoadBlocks getBlocks )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        LoadBlocks blocksMaybe ->
            case blocksMaybe of
                Ok blocks ->
                    ( { model | blocks = blocks, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewBlocks model.blocks
        , viewError model.error
        , viewDebug model
        ]


viewBlocks : List (List Block) -> Html.Html Msg
viewBlocks branches =
    let
        viewBranch n branch =
            Html.div [ HA.class "branch" ]
                [ Html.h2 [] [ Html.text ("branch " ++ toString n) ]
                , Html.div [] (List.indexedMap viewBlock branch)
                ]
    in
        Html.div [ HA.class "branches" ] (List.indexedMap viewBranch branches)


viewBlock : Int -> Block -> Html.Html Msg
viewBlock n block =
    let
        viewProperty label value =
            Html.div [ HA.class "property" ]
                [ Html.label [ HA.for label ] [ Html.text label ]
                , Html.span [ HA.id label ] [ Html.text value ]
                ]
    in
        Html.div [ HA.class "block" ]
            [ Html.h3 [] [ Html.text ("block " ++ toString n) ]
            , viewProperty "hash" block.hash
            , viewProperty "predecessor" block.predecessor
            , viewProperty "timestamp" block.timestamp
            ]


viewError : Maybe Http.Error -> Html.Html Msg
viewError errorMaybe =
    case errorMaybe of
        Just error ->
            Html.div [ HA.class "error" ] [ Html.text (toString error) ]

        Nothing ->
            Html.text ""


viewDebug : Model -> Html.Html Msg
viewDebug model =
    Html.div [ HA.class "debug" ]
        [ Html.h2 [] [ Html.text "Raw model" ]
        , Html.text <| toString model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
