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
import Page
import Update exposing (update, Msg(..))
import View exposing (view)
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
            , showBlock = Nothing
            , showOperation = Nothing
            , showBranch = Nothing
            , chain = Data.Chain.init
            , now = Date.fromTime flags.now
            , pageState = Loaded Page.Blank
            }

        -- set initial route based on location bar
        ( routedModel, routeCmd ) =
            Update.setRoute (Route.fromLocation location) initModel
    in
        ( routedModel
        , Cmd.batch
            [ Request.Block.getHeads routedModel.nodeUrl |> Http.send LoadHeads
            , routeCmd
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (30 * Time.second) Tick
