module Main exposing (main)

import Html
import Http
import Dict
import Model exposing (..)
import Update exposing (update, Msg(..), getBlocks, getSchema, getHeads)
import View exposing (view)


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    { nodeUrl : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { heads = []
            , blocks = Dict.empty
            , schemaData = Dict.empty
            , errors = []
            , nodeUrl = flags.nodeUrl
            , operations = Dict.empty
            , parsedOperations = Dict.empty
            , showBlock = Nothing
            , showOperation = Nothing
            , showBranch = Nothing
            , blockOperations = Dict.empty
            }

        schemaQuery1 =
            "/describe"

        schemaQuery2 =
            "/describe/blocks/head/proto"
    in
        ( model
        , Cmd.batch
            [ getHeads model.nodeUrl
              --, Http.send (LoadSchema schemaQuery1) (getSchema model.nodeUrl schemaQuery1)
              --, Http.send (LoadSchema schemaQuery2) (getSchema model.nodeUrl schemaQuery2)
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
