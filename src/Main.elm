module Main exposing (main)

import Html
import Http
import Date
import Dict
import Navigation
import Route exposing (Route)
import Time
import Data.Chain
import Model exposing (..)
import Update exposing (update, Msg(..))
import View exposing (view)
import Request.Block
import Request.Schema exposing (getSchema)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    { nodeUrl : String
    , now : Float
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        model =
            { schemaData = Dict.empty
            , errors = []
            , nodeUrl = flags.nodeUrl
            , showBlock = Nothing
            , showOperation = Nothing
            , showBranch = Nothing
            , chain = Data.Chain.init
            , now = Date.fromTime flags.now
            , pageState = Loaded Blank
            }

        schemaQuery1 =
            "/describe"

        schemaQuery2 =
            "/describe/blocks/head/proto"
    in
        ( model
        , Cmd.batch
            [ Request.Block.getHeads model.nodeUrl |> Http.send LoadHeads
              --, Http.send (LoadSchema schemaQuery1) (getSchema model.nodeUrl schemaQuery1)
              --, Http.send (LoadSchema schemaQuery2) (getSchema model.nodeUrl schemaQuery2)
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (60 * Time.second) Tick
