module Main exposing (main)

import Http
import Date
import Dict
import InfiniteScroll
import Navigation
import Route
import Table
import Task
import Time
import WebSocket
import Window
import Data.Chain
import Model exposing (Model, PageState(Loaded))
import Msg exposing (Msg(Monitor, Now, RpcResponse, SetRoute, Tick, WindowResized))
import Page
import Update exposing (update)
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
            , peerTableState = Table.initialSort "node addr"
            , windowSize = Window.Size 400 400
            , infiniteScroll = InfiniteScroll.init loadMore
            }

        -- set initial route based on location bar
        ( routedModel, routeCmd ) =
            Update.setRoute (Route.fromLocation location) initModel
    in
        ( routedModel
        , Cmd.batch
            [ Request.Block.requestChainSummary routedModel.nodeUrl
                |> Http.send (Result.map Request.ChainSummary >> RpcResponse)
            , routeCmd
            , Window.size |> Task.perform WindowResized
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (10 * Time.minute) Tick
        , Time.every (30 * Time.second) Now
        , WebSocket.listen "ws://api.ostez.com/ws/monitor" Monitor
        , Window.resizes WindowResized
        ]


loadMore : InfiniteScroll.Direction -> Cmd Msg
loadMore dir =
    -- We need to use model information when handling the loadMore trigger, so
    -- we send ourselves a msg that we can handle in the main update loop where
    -- we have the full model.
    Task.perform identity (Task.succeed (Msg.LoadMore dir))
