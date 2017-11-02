port module Main exposing (main)

import Html
import Http
import Date
import Json.Decode as Decode
import Dict
import Navigation
import Route exposing (Route)
import Table
import Time
import Data.Chain
import Model exposing (..)
import Page
import Update exposing (update, Msg(..))
import View exposing (view)
import Request
import Request.Block


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
        initModel =
            { schemaData = Dict.empty
            , errors = []
            , nodeUrl = flags.nodeUrl
            , chain = Data.Chain.init
            , now = Date.fromTime flags.now
            , pageState = Loaded Page.Blank
            , tableState = Table.initialSort "Hash"
            , query = ""
            , transactionTableState = Table.initialSort "time"
            , contractTableState = Table.initialSort "contract"
            }

        -- set initial route based on location bar
        ( routedModel, routeCmd ) =
            Update.setRoute (Route.fromLocation location) initModel
    in
        ( routedModel
        , Cmd.batch
            [ {- Request.Block.getHeads routedModel.nodeUrl
                 |> Http.send (Result.map Request.Heads >> RpcResponse)
              -}
              Request.Block.requestChainSummary routedModel.nodeUrl
                |> Http.send (Result.map Request.ChainSummary >> RpcResponse)
            , routeCmd
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (10 * Time.minute) Tick
        , Time.every (30 * Time.second) Now
        , monitor Monitor
        ]


port monitor : (Decode.Value -> msg) -> Sub msg
