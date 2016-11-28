module Main exposing (main)

import Html
import Html.Attributes as HA
import Http
import Json.Decode as Decode


type alias Model =
    { blocks : List (List Block)
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
    ( { blocks = [] }, Http.send LoadBlocks getBlocks )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        LoadBlocks (Ok data) ->
            let
                _ =
                    Debug.log "blocks data" data
            in
                ( { model | blocks = data }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ viewDebug model
        , viewBlocks model.blocks
        ]


viewBlocks : List (List Block) -> Html.Html Msg
viewBlocks branches =
    let
        viewBranch branch =
            Html.div [ HA.class "branch" ]
                [ Html.h2 [] [ Html.text "branch" ]
                , Html.div [] (List.map viewBlock branch)
                ]
    in
        Html.div [ HA.class "branches" ] (List.map viewBranch branches)


viewBlock : Block -> Html.Html Msg
viewBlock block =
    Html.div [ HA.class "block" ]
        [ Html.h3 [] [ Html.text "block" ]
        , Html.div [ HA.class "hash" ] [ Html.text block.hash ]
        ]


viewDebug : Model -> Html.Html Msg
viewDebug model =
    Html.text <| toString model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
