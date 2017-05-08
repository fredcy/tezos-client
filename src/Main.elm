module Main exposing (main)

import Html
import Http
import Date
import Dict
import Time
import Model exposing (..)
import Update exposing (update, Msg(..), getBlocks, getSchema, getHeads)
import View exposing (view)


type Page
    = Blank
    | Home


main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    { nodeUrl : String
    , now : Float
    }


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
            , now = Date.fromTime flags.now
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
    Time.every (60 * Time.second) Tick